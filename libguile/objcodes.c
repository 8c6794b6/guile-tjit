/* Copyright (C) 2001, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#include <sys/stat.h>
#include <sys/types.h>
#include <assert.h>
#include <alignof.h>
#include <byteswap.h>

#include <full-read.h>

#include "_scm.h"
#include "elf.h"
#include "programs.h"
#include "objcodes.h"

/* Before, we used __BYTE_ORDER, but that is not defined on all
   systems. So punt and use automake, PDP endianness be damned. */
#define SCM_BYTE_ORDER_BE 4321
#define SCM_BYTE_ORDER_LE 1234

/* Byte order of the build machine.  */
#ifdef WORDS_BIGENDIAN
#define SCM_BYTE_ORDER SCM_BYTE_ORDER_BE
#else
#define SCM_BYTE_ORDER SCM_BYTE_ORDER_LE
#endif

/* This file contains the loader for Guile's on-disk format: ELF with
   some custom tags in the dynamic segment.  */

#if SIZEOF_SCM_T_BITS == 4
#define Elf_Half Elf32_Half
#define Elf_Word Elf32_Word
#define Elf_Ehdr Elf32_Ehdr
#define ELFCLASS ELFCLASS32
#define Elf_Phdr Elf32_Phdr
#define Elf_Dyn Elf32_Dyn
#elif SIZEOF_SCM_T_BITS == 8
#define Elf_Half Elf64_Half
#define Elf_Word Elf64_Word
#define Elf_Ehdr Elf64_Ehdr
#define ELFCLASS ELFCLASS64
#define Elf_Phdr Elf64_Phdr
#define Elf_Dyn Elf64_Dyn
#else
#error
#endif

#define DT_LOGUILE          0x37146000  /* Start of Guile-specific */
#define DT_GUILE_GC_ROOT    0x37146000  /* Offset of GC roots */
#define DT_GUILE_GC_ROOT_SZ 0x37146001  /* Size in machine words of GC
                                           roots */
#define DT_GUILE_ENTRY      0x37146002  /* Address of entry thunk */
#define DT_GUILE_RTL_VERSION 0x37146003 /* Bytecode version */
#define DT_HIGUILE          0x37146fff  /* End of Guile-specific */

#ifdef WORDS_BIGENDIAN
#define ELFDATA ELFDATA2MSB
#else
#define ELFDATA ELFDATA2LSB
#endif

enum bytecode_kind
  {
    BYTECODE_KIND_NONE,
    BYTECODE_KIND_GUILE_2_0
  };

static SCM
pointer_to_procedure (enum bytecode_kind bytecode_kind, char *ptr)
{
  switch (bytecode_kind)
    {
    case BYTECODE_KIND_GUILE_2_0:
      {
        SCM objcode;
        scm_t_bits tag = SCM_MAKE_OBJCODE_TAG (SCM_OBJCODE_TYPE_MMAP, 0);

        objcode = scm_double_cell (tag, (scm_t_bits) ptr, SCM_BOOL_F_BITS, 0);
        return scm_make_program (objcode, SCM_BOOL_F, SCM_UNDEFINED);
      }
    case BYTECODE_KIND_NONE:
    default:
      abort ();
    }
}

static const char*
check_elf_header (const Elf_Ehdr *header)
{
  if (!(header->e_ident[EI_MAG0] == ELFMAG0
        && header->e_ident[EI_MAG1] == ELFMAG1
        && header->e_ident[EI_MAG2] == ELFMAG2
        && header->e_ident[EI_MAG3] == ELFMAG3))
    return "not an ELF file";

  if (header->e_ident[EI_CLASS] != ELFCLASS)
    return "ELF file does not have native word size";

  if (header->e_ident[EI_DATA] != ELFDATA)
    return "ELF file does not have native byte order";

  if (header->e_ident[EI_VERSION] != EV_CURRENT)
    return "bad ELF version";

  if (header->e_ident[EI_OSABI] != ELFOSABI_STANDALONE)
    return "unexpected OS ABI";

  if (header->e_ident[EI_ABIVERSION] != 0)
    return "unexpected ABI version";

  if (header->e_type != ET_DYN)
    return "unexpected ELF type";

  if (header->e_machine != EM_NONE)
    return "unexpected machine";

  if (header->e_version != EV_CURRENT)
    return "unexpected ELF version";

  if (header->e_ehsize != sizeof *header)
    return "unexpected header size";

  if (header->e_phentsize != sizeof (Elf_Phdr))
    return "unexpected program header size";

  return NULL;
}

static int
segment_flags_to_prot (Elf_Word flags)
{
  int prot = 0;
          
  if (flags & PF_X)
    prot |= PROT_EXEC;
  if (flags & PF_W)
    prot |= PROT_WRITE;
  if (flags & PF_R)
    prot |= PROT_READ;

  return prot;
}

static int
map_segments (int fd, char **base,
              const Elf_Phdr *from, const Elf_Phdr *to)
{
  int prot = segment_flags_to_prot (from->p_flags);
  char *ret;

  ret = mmap (*base + from->p_vaddr,
              to->p_offset + to->p_filesz - from->p_offset,
              prot, MAP_PRIVATE, fd, from->p_offset);

  if (ret == (char *) -1)
    return 1;

  if (!*base)
    *base = ret;

  return 0;
}

static int
mprotect_segments (char *base, const Elf_Phdr *from, const Elf_Phdr *to)
{
  return mprotect (base + from->p_vaddr,
                   to->p_vaddr + to->p_memsz - from->p_vaddr,
                   segment_flags_to_prot (from->p_flags));
}

static char*
process_dynamic_segment (char *base, Elf_Phdr *dyn_phdr,
                         SCM *init_out, SCM *entry_out)
{
  char *dyn_addr = base + dyn_phdr->p_vaddr;
  Elf_Dyn *dyn = (Elf_Dyn *) dyn_addr;
  size_t i, dyn_size = dyn_phdr->p_memsz / sizeof (Elf_Dyn);
  char *init = 0, *gc_root = 0, *entry = 0;
  scm_t_ptrdiff gc_root_size = 0;
  enum bytecode_kind bytecode_kind = BYTECODE_KIND_NONE;

  for (i = 0; i < dyn_size; i++)
    {
      if (dyn[i].d_tag == DT_NULL)
        break;

      switch (dyn[i].d_tag)
        {
        case DT_INIT:
          if (init)
            return "duplicate DT_INIT";
          init = base + dyn[i].d_un.d_val;
          break;
        case DT_GUILE_GC_ROOT:
          if (gc_root)
            return "duplicate DT_GUILE_GC_ROOT";
          gc_root = base + dyn[i].d_un.d_val;
          break;
        case DT_GUILE_GC_ROOT_SZ:
          if (gc_root_size)
            return "duplicate DT_GUILE_GC_ROOT_SZ";
          gc_root_size = dyn[i].d_un.d_val;
          break;
        case DT_GUILE_ENTRY:
          if (entry)
            return "duplicate DT_GUILE_ENTRY";
          entry = base + dyn[i].d_un.d_val;
          break;
        case DT_GUILE_RTL_VERSION:
          if (bytecode_kind != BYTECODE_KIND_NONE)
            return "duplicate DT_GUILE_RTL_VERSION";
          {
            scm_t_uint16 major = dyn[i].d_un.d_val >> 16;
            scm_t_uint16 minor = dyn[i].d_un.d_val & 0xffff;
            if (major != 0x0200)
              return "incompatible bytecode kind";
            if (minor > SCM_OBJCODE_MINOR_VERSION)
              return "incompatible bytecode version";
            bytecode_kind = BYTECODE_KIND_GUILE_2_0;
            break;
          }
        }
    }

  if (bytecode_kind != BYTECODE_KIND_GUILE_2_0)
    return "missing DT_GUILE_RTL_VERSION";
  if (init)
    return "unexpected DT_INIT";
  if ((scm_t_uintptr) entry % 8)
    return "unaligned DT_GUILE_ENTRY";
  if (!entry)
    return "missing DT_GUILE_ENTRY";

  if (gc_root)
    GC_add_roots (gc_root, gc_root + gc_root_size);

  *init_out = SCM_BOOL_F;
  *entry_out = pointer_to_procedure (bytecode_kind, entry);
  return NULL;
}

#define ABORT(msg) do { err_msg = msg; goto cleanup; } while (0)

#ifdef HAVE_SYS_MMAN_H
static SCM
load_thunk_from_fd_using_mmap (int fd)
#define FUNC_NAME "load-thunk-from-disk"
{
  Elf_Ehdr header;
  Elf_Phdr *ph;
  const char *err_msg = 0;
  char *base = 0;
  size_t n;
  int i;
  int start_segment = -1;
  int prev_segment = -1;
  int dynamic_segment = -1;
  SCM init = SCM_BOOL_F, entry = SCM_BOOL_F;

  if (full_read (fd, &header, sizeof header) != sizeof header)
    ABORT ("object file too small");

  if ((err_msg = check_elf_header (&header)))
    goto cleanup;

  if (lseek (fd, header.e_phoff, SEEK_SET) == (off_t) -1)
    goto cleanup;
  
  n = header.e_phnum;
  ph = scm_gc_malloc_pointerless (n * sizeof (Elf_Phdr), "segment headers");

  if (full_read (fd, ph, n * sizeof (Elf_Phdr)) != n * sizeof (Elf_Phdr))
    ABORT ("failed to read program headers");
      
  for (i = 0; i < n; i++)
    {
      if (!ph[i].p_memsz)
        continue;

      if (ph[i].p_filesz != ph[i].p_memsz)
        ABORT ("expected p_filesz == p_memsz");
      
      if (!ph[i].p_flags)
        ABORT ("expected nonzero segment flags");

      if (ph[i].p_type == PT_DYNAMIC)
        {
          if (dynamic_segment >= 0)
            ABORT ("expected only one PT_DYNAMIC segment");
          dynamic_segment = i;
        }

      if (start_segment < 0)
        {
          if (!base && ph[i].p_vaddr)
            ABORT ("first loadable vaddr is not 0");
            
          start_segment = prev_segment = i;
          continue;
        }

      if (ph[i].p_flags == ph[start_segment].p_flags)
        {
          if (ph[i].p_vaddr - ph[prev_segment].p_vaddr 
              != ph[i].p_offset - ph[prev_segment].p_offset)
            ABORT ("coalesced segments not contiguous");

          prev_segment = i;
          continue;
        }

      /* Otherwise we have a new kind of segment.  Map previous
         segments.  */
      if (map_segments (fd, &base, &ph[start_segment], &ph[prev_segment]))
        goto cleanup;

      /* Open a new set of segments.  */
      start_segment = prev_segment = i;
    }

  /* Map last segments.  */
  if (start_segment < 0)
    ABORT ("no loadable segments");

  if (map_segments (fd, &base, &ph[start_segment], &ph[prev_segment]))
    goto cleanup;

  if (dynamic_segment < 0)
    ABORT ("no PT_DYNAMIC segment");

  if ((err_msg = process_dynamic_segment (base, &ph[dynamic_segment],
                                          &init, &entry)))
    goto cleanup;

  if (scm_is_true (init))
    scm_call_0 (init);

  /* Finally!  Return the thunk.  */
  return entry;

  /* FIXME: munmap on error? */
 cleanup:
  {
    int errno_save = errno;
    (void) close (fd);
    errno = errno_save;
    if (errno)
      SCM_SYSERROR;
    scm_misc_error (FUNC_NAME, err_msg ? err_msg : "error loading ELF file",
                    SCM_EOL);
  }
}
#undef FUNC_NAME
#endif /* HAVE_SYS_MMAN_H */

static SCM
load_thunk_from_memory (char *data, size_t len)
#define FUNC_NAME "load-thunk-from-memory"
{
  Elf_Ehdr header;
  Elf_Phdr *ph;
  const char *err_msg = 0;
  char *base = 0;
  size_t n, memsz = 0, alignment = 8;
  int i;
  int first_loadable = -1;
  int start_segment = -1;
  int prev_segment = -1;
  int dynamic_segment = -1;
  SCM init = SCM_BOOL_F, entry = SCM_BOOL_F;

  if (len < sizeof header)
    ABORT ("object file too small");

  memcpy (&header, data, sizeof header);

  if ((err_msg = check_elf_header (&header)))
    goto cleanup;

  n = header.e_phnum;
  if (len < header.e_phoff + n * sizeof (Elf_Phdr))
    goto cleanup;
  ph = (Elf_Phdr*) (data + header.e_phoff);

  for (i = 0; i < n; i++)
    {
      if (!ph[i].p_memsz)
        continue;

      if (ph[i].p_filesz != ph[i].p_memsz)
        ABORT ("expected p_filesz == p_memsz");

      if (!ph[i].p_flags)
        ABORT ("expected nonzero segment flags");

      if (ph[i].p_align < alignment)
        {
          if (ph[i].p_align % alignment)
            ABORT ("expected new alignment to be multiple of old");
          alignment = ph[i].p_align;
        }

      if (ph[i].p_type == PT_DYNAMIC)
        {
          if (dynamic_segment >= 0)
            ABORT ("expected only one PT_DYNAMIC segment");
          dynamic_segment = i;
        }

      if (first_loadable < 0)
        {
          if (ph[i].p_vaddr)
            ABORT ("first loadable vaddr is not 0");

          first_loadable = i;
        }

      if (ph[i].p_vaddr < memsz)
        ABORT ("overlapping segments");

      if (ph[i].p_offset + ph[i].p_filesz > len)
        ABORT ("segment beyond end of byte array");

      memsz = ph[i].p_vaddr + ph[i].p_memsz;
    }

  if (first_loadable < 0)
    ABORT ("no loadable segments");

  if (dynamic_segment < 0)
    ABORT ("no PT_DYNAMIC segment");

  /* Now copy segments.  */

  /* We leak this memory, as we leak the memory mappings in
     load_thunk_from_fd_using_mmap.

     If the file is has an alignment of 8, use the standard malloc.
     (FIXME to ensure alignment on non-GNU malloc.)  Otherwise use
     posix_memalign.  We only use mprotect if the aligment is 4096.  */
  if (alignment == 8)
    {
      base = malloc (memsz);
      if (!base)
        goto cleanup;
    }
  else
    if ((errno = posix_memalign ((void **) &base, alignment, memsz)))
      goto cleanup;

  memset (base, 0, memsz);

  for (i = 0; i < n; i++)
    {
      if (!ph[i].p_memsz)
        continue;

      memcpy (base + ph[i].p_vaddr,
              data + ph[i].p_offset,
              ph[i].p_memsz);

      if (start_segment < 0)
        {
          start_segment = prev_segment = i;
          continue;
        }

      if (ph[i].p_flags == ph[start_segment].p_flags)
        {
          prev_segment = i;
          continue;
        }

      if (alignment == 4096)
        if (mprotect_segments (base, &ph[start_segment], &ph[prev_segment]))
          goto cleanup;

      /* Open a new set of segments.  */
      start_segment = prev_segment = i;
    }

  /* Mprotect the last segments.  */
  if (alignment == 4096)
    if (mprotect_segments (base, &ph[start_segment], &ph[prev_segment]))
      goto cleanup;

  if ((err_msg = process_dynamic_segment (base, &ph[dynamic_segment],
                                          &init, &entry)))
    goto cleanup;

  if (scm_is_true (init))
    scm_call_0 (init);

  /* Finally!  Return the thunk.  */
  return entry;

 cleanup:
  {
    if (errno)
      SCM_SYSERROR;
    scm_misc_error (FUNC_NAME, err_msg ? err_msg : "error loading ELF file",
                    SCM_EOL);
  }
}
#undef FUNC_NAME

#ifndef HAVE_SYS_MMAN_H
static SCM
load_thunk_from_fd_using_read (int fd)
#define FUNC_NAME "load-thunk-from-disk"
{
  char *data;
  size_t len;
  struct stat st;
  int ret;

  ret = fstat (fd, &st);
  if (ret < 0)
    SCM_SYSERROR;
  len = st.st_size;
  data = scm_gc_malloc_pointerless (len, "objcode");
  if (full_read (fd, data, len) != len)
    {
      int errno_save = errno;
      (void) close (fd);
      errno = errno_save;
      if (errno)
        SCM_SYSERROR;
      scm_misc_error (FUNC_NAME, "short read while loading objcode",
                      SCM_EOL);
    }
  (void) close (fd);
  return load_thunk_from_memory (data, len);
}
#undef FUNC_NAME
#endif /* ! HAVE_SYS_MMAN_H */

SCM_DEFINE (scm_load_thunk_from_file, "load-thunk-from-file", 1, 0, 0,
	    (SCM filename),
	    "")
#define FUNC_NAME s_scm_load_thunk_from_file
{
  char *c_filename;
  int fd;

  SCM_VALIDATE_STRING (1, filename);

  c_filename = scm_to_locale_string (filename);
  fd = open (c_filename, O_RDONLY | O_CLOEXEC);
  free (c_filename);
  if (fd < 0) SCM_SYSERROR;

#ifdef HAVE_SYS_MMAN_H
  return load_thunk_from_fd_using_mmap (fd);
#else
  return load_thunk_from_fd_using_read (fd);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_load_thunk_from_memory, "load-thunk-from-memory", 1, 0, 0,
	    (SCM bv),
	    "")
#define FUNC_NAME s_scm_load_thunk_from_memory
{
  SCM_VALIDATE_BYTEVECTOR (1, bv);

  return load_thunk_from_memory ((char *) SCM_BYTEVECTOR_CONTENTS (bv),
                                 SCM_BYTEVECTOR_LENGTH (bv));
}
#undef FUNC_NAME


/*
 * Objcode type
 */

/* Convert X, which is in byte order BYTE_ORDER, to its native
   representation.  */
static inline uint32_t
to_native_order (uint32_t x, int byte_order)
{
  if (byte_order == SCM_BYTE_ORDER)
    return x;
  else
    return bswap_32 (x);
}

SCM
scm_c_make_objcode_slice (SCM parent, const scm_t_uint8 *ptr)
#define FUNC_NAME "make-objcode-slice"
{
  const struct scm_objcode *data, *parent_data;
  const scm_t_uint8 *parent_base;

  SCM_VALIDATE_OBJCODE (1, parent);
  parent_data = SCM_OBJCODE_DATA (parent);
  parent_base = SCM_C_OBJCODE_BASE (parent_data);

  if (ptr < parent_base
      || ptr >= (parent_base + parent_data->len + parent_data->metalen
                 - sizeof (struct scm_objcode)))
    scm_misc_error
      (FUNC_NAME, "offset out of bounds (~a vs ~a + ~a + ~a)",
       scm_list_4 (scm_from_unsigned_integer ((scm_t_bits) ptr),
                   scm_from_unsigned_integer ((scm_t_bits) parent_base),
                   scm_from_uint32 (parent_data->len),
                   scm_from_uint32 (parent_data->metalen)));

  /* Make sure bytecode for the objcode-meta is suitable aligned.  Failing to
     do so leads to SIGBUS/SIGSEGV on some arches (e.g., SPARC).  */
  assert ((((scm_t_bits) ptr) &
	   (alignof_type (struct scm_objcode) - 1UL)) == 0);

  data = (struct scm_objcode*) ptr;
  assert (SCM_C_OBJCODE_BASE (data) + data->len + data->metalen
	  <= parent_base + parent_data->len + parent_data->metalen);

  return scm_double_cell (SCM_MAKE_OBJCODE_TAG (SCM_OBJCODE_TYPE_SLICE, 0),
                          (scm_t_bits)data, SCM_UNPACK (parent), 0);
}
#undef FUNC_NAME


/*
 * Scheme interface
 */

SCM_DEFINE (scm_objcode_p, "objcode?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_objcode_p
{
  return scm_from_bool (SCM_OBJCODE_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_objcode_meta, "objcode-meta", 1, 0, 0,
	    (SCM objcode),
	    "")
#define FUNC_NAME s_scm_objcode_meta
{
  SCM_VALIDATE_OBJCODE (1, objcode);

  if (SCM_OBJCODE_META_LEN (objcode) == 0)
    return SCM_BOOL_F;
  else
    return scm_c_make_objcode_slice (objcode, (SCM_OBJCODE_BASE (objcode)
                                               + SCM_OBJCODE_LEN (objcode)));
}
#undef FUNC_NAME

/* Wrap BYTECODE in objcode, interpreting its lengths according to
   BYTE_ORDER.  */
static SCM
bytecode_to_objcode (SCM bytecode, int byte_order)
#define FUNC_NAME "bytecode->objcode"
{
  size_t size, len, metalen;
  const scm_t_uint8 *c_bytecode;
  struct scm_objcode *data;

  if (!scm_is_bytevector (bytecode))
    scm_wrong_type_arg (FUNC_NAME, 1, bytecode);

  size = SCM_BYTEVECTOR_LENGTH (bytecode);
  c_bytecode = (const scm_t_uint8*)SCM_BYTEVECTOR_CONTENTS (bytecode);

  SCM_ASSERT_RANGE (0, bytecode, size >= sizeof(struct scm_objcode));
  data = (struct scm_objcode*)c_bytecode;

  len = to_native_order (data->len, byte_order);
  metalen = to_native_order (data->metalen, byte_order);

  if (len + metalen != (size - sizeof (*data)))
    scm_misc_error (FUNC_NAME, "bad bytevector size (~a != ~a)",
		    scm_list_2 (scm_from_size_t (size),
				scm_from_uint32 (sizeof (*data) + len + metalen)));

  /* foolishly, we assume that as long as bytecode is around, that c_bytecode
     will be of the same length; perhaps a bad assumption? */
  return scm_double_cell (SCM_MAKE_OBJCODE_TAG (SCM_OBJCODE_TYPE_BYTEVECTOR, 0),
                          (scm_t_bits)data, SCM_UNPACK (bytecode), 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytecode_to_objcode, "bytecode->objcode", 1, 1, 0,
	    (SCM bytecode, SCM endianness),
	    "")
#define FUNC_NAME s_scm_bytecode_to_objcode
{
  int byte_order;

  if (SCM_UNBNDP (endianness))
    byte_order = SCM_BYTE_ORDER;
  else if (scm_is_eq (endianness, scm_endianness_big))
    byte_order = SCM_BYTE_ORDER_BE;
  else if (scm_is_eq (endianness, scm_endianness_little))
    byte_order = SCM_BYTE_ORDER_LE;
  else
    scm_wrong_type_arg (FUNC_NAME, 2, endianness);

  return bytecode_to_objcode (bytecode, byte_order);
}
#undef FUNC_NAME

SCM_DEFINE (scm_objcode_to_bytecode, "objcode->bytecode", 1, 1, 0,
	    (SCM objcode, SCM endianness),
	    "")
#define FUNC_NAME s_scm_objcode_to_bytecode
{
  scm_t_uint32 len, meta_len, total_len;
  int byte_order;

  SCM_VALIDATE_OBJCODE (1, objcode);

  if (SCM_UNBNDP (endianness))
    byte_order = SCM_BYTE_ORDER;
  else if (scm_is_eq (endianness, scm_endianness_big))
    byte_order = SCM_BYTE_ORDER_BE;
  else if (scm_is_eq (endianness, scm_endianness_little))
    byte_order = SCM_BYTE_ORDER_LE;
  else
    scm_wrong_type_arg (FUNC_NAME, 2, endianness);

  len = SCM_OBJCODE_LEN (objcode);
  meta_len = SCM_OBJCODE_META_LEN (objcode);

  total_len = sizeof (struct scm_objcode);
  total_len += to_native_order (len, byte_order);
  total_len += to_native_order (meta_len, byte_order);

  return scm_c_take_gc_bytevector ((scm_t_int8*)SCM_OBJCODE_DATA (objcode),
                                   total_len, objcode);
}
#undef FUNC_NAME

void
scm_i_objcode_print (SCM objcode, SCM port, scm_print_state *pstate)
{
  scm_puts_unlocked ("#<objcode ", port);
  scm_uintprint ((scm_t_bits)SCM_OBJCODE_BASE (objcode), 16, port);
  scm_puts_unlocked (">", port);
}


void
scm_bootstrap_objcodes (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_objcodes",
                            (scm_t_extension_init_func)scm_init_objcodes, NULL);
}

void
scm_init_objcodes (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/objcodes.x"
#endif

  scm_c_define ("word-size", scm_from_size_t (sizeof(SCM)));
  scm_c_define ("byte-order", scm_from_uint16 (SCM_BYTE_ORDER));
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
