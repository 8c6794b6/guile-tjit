/* Copyright (C) 2001, 2009, 2010, 2011, 2012
 *    2013, 2014 Free Software Foundation, Inc.
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
#include <verify.h>

#include <full-read.h>

#include "_scm.h"
#include "elf.h"
#include "programs.h"
#include "loader.h"

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
#define DT_GUILE_VM_VERSION 0x37146003  /* Bytecode version */
#define DT_GUILE_FRAME_MAPS 0x37146004  /* Frame maps */
#define DT_HIGUILE          0x37146fff  /* End of Guile-specific */

#ifdef WORDS_BIGENDIAN
#define ELFDATA ELFDATA2MSB
#else
#define ELFDATA ELFDATA2LSB
#endif

static void register_elf (char *data, size_t len, char *frame_maps);

enum bytecode_kind
  {
    BYTECODE_KIND_NONE,
    BYTECODE_KIND_GUILE_2_2
  };

static SCM
pointer_to_procedure (enum bytecode_kind bytecode_kind, char *ptr)
{
  switch (bytecode_kind)
    {
    case BYTECODE_KIND_GUILE_2_2:
      {
        return scm_i_make_program ((scm_t_uint32 *) ptr);
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

#define IS_ALIGNED(offset, alignment) \
  (!((offset) & ((alignment) - 1)))
#define ALIGN(offset, alignment) \
  ((offset + (alignment - 1)) & ~(alignment - 1))

/* Return the alignment required by the ELF at DATA, of LEN bytes.  */
static size_t
elf_alignment (const char *data, size_t len)
{
  Elf_Ehdr *header;
  int i;
  size_t alignment = 8;

  if (len < sizeof(Elf_Ehdr))
    return alignment;
  header = (Elf_Ehdr *) data;
  if (header->e_phoff + header->e_phnum * header->e_phentsize >= len)
    return alignment;
  for (i = 0; i < header->e_phnum; i++)
    {
      Elf_Phdr *phdr;
      const char *phdr_addr = data + header->e_phoff + i * header->e_phentsize;

      if (!IS_ALIGNED ((scm_t_uintptr) phdr_addr, alignof_type (Elf_Phdr)))
        return alignment;
      phdr = (Elf_Phdr *) phdr_addr;

      if (phdr->p_align & (phdr->p_align - 1))
        return alignment;

      if (phdr->p_align > alignment)
        alignment = phdr->p_align;
    }

  return alignment;
}

/* This function leaks the memory that it allocates.  */
static char*
alloc_aligned (size_t len, unsigned alignment)
{
  char *ret;

  if (alignment == 8)
    {
      /* FIXME: Assert that we actually have an 8-byte-aligned malloc.  */
      ret = malloc (len);
    }
#if defined(HAVE_SYS_MMAN_H) && defined(MMAP_ANONYMOUS)
  else if (alignment == SCM_PAGE_SIZE)
    {
      ret = mmap (NULL, len, PROT_READ | PROT_WRITE, -1, 0);
      if (ret == MAP_FAILED)
        SCM_SYSERROR;
    }
#endif
  else
    {
      if (len + alignment < len)
        abort ();

      ret = malloc (len + alignment - 1);
      if (!ret)
        abort ();
      ret = (char *) ALIGN ((scm_t_uintptr) ret, alignment);
    }

  return ret;
}

static char*
copy_and_align_elf_data (const char *data, size_t len)
{
  size_t alignment;
  char *copy;

  alignment = elf_alignment (data, len);
  copy = alloc_aligned (len, alignment);
  memcpy(copy, data, len);

  return copy;
}

#ifdef HAVE_SYS_MMAN_H
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
#endif

static char*
process_dynamic_segment (char *base, Elf_Phdr *dyn_phdr,
                         SCM *init_out, SCM *entry_out, char **frame_maps_out)
{
  char *dyn_addr = base + dyn_phdr->p_vaddr;
  Elf_Dyn *dyn = (Elf_Dyn *) dyn_addr;
  size_t i, dyn_size = dyn_phdr->p_memsz / sizeof (Elf_Dyn);
  char *init = 0, *gc_root = 0, *entry = 0, *frame_maps = 0;
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
        case DT_GUILE_VM_VERSION:
          if (bytecode_kind != BYTECODE_KIND_NONE)
            return "duplicate DT_GUILE_VM_VERSION";
          {
            scm_t_uint16 major = dyn[i].d_un.d_val >> 16;
            scm_t_uint16 minor = dyn[i].d_un.d_val & 0xffff;
            switch (major)
              {
              case 0x0202:
                bytecode_kind = BYTECODE_KIND_GUILE_2_2;
                /* As we get closer to 2.2, we will allow for backwards
                   compatibility and we can change this test to ">"
                   instead of "!=".  However until then, to deal with VM
                   churn it's best to keep these things in
                   lock-step.  */
                if (minor != SCM_OBJCODE_MINOR_VERSION)
                  return "incompatible bytecode version";
                break;
              default:
                return "incompatible bytecode kind";
              }
            break;
          }
        case DT_GUILE_FRAME_MAPS:
          if (frame_maps)
            return "duplicate DT_GUILE_FRAME_MAPS";
          frame_maps = base + dyn[i].d_un.d_val;
          break;
        }
    }

  if (!entry)
    return "missing DT_GUILE_ENTRY";

  switch (bytecode_kind)
    {
    case BYTECODE_KIND_GUILE_2_2:
      if ((scm_t_uintptr) init % 4)
        return "unaligned DT_INIT";
      if ((scm_t_uintptr) entry % 4)
        return "unaligned DT_GUILE_ENTRY";
      break;
    case BYTECODE_KIND_NONE:
    default:
      return "missing DT_GUILE_VM_VERSION";
    }

  if (gc_root)
    GC_add_roots (gc_root, gc_root + gc_root_size);

  *init_out = init ? pointer_to_procedure (bytecode_kind, init) : SCM_BOOL_F;
  *entry_out = pointer_to_procedure (bytecode_kind, entry);
  *frame_maps_out = frame_maps;

  return NULL;
}

#define ABORT(msg) do { err_msg = msg; goto cleanup; } while (0)

static SCM
load_thunk_from_memory (char *data, size_t len, int is_read_only)
#define FUNC_NAME "load-thunk-from-memory"
{
  Elf_Ehdr *header;
  Elf_Phdr *ph;
  const char *err_msg = 0;
  size_t n, alignment = 8;
  int i;
  int dynamic_segment = -1;
  SCM init = SCM_BOOL_F, entry = SCM_BOOL_F;
  char *frame_maps = 0;

  if (len < sizeof *header)
    ABORT ("object file too small");

  header = (Elf_Ehdr*) data;
  
  if ((err_msg = check_elf_header (header)))
    goto cleanup;

  if (header->e_phnum == 0)
    ABORT ("no loadable segments");
  n = header->e_phnum;

  if (len < header->e_phoff + n * sizeof (Elf_Phdr))
    ABORT ("object file too small");

  ph = (Elf_Phdr*) (data + header->e_phoff);

  /* Check that the segment table is sane.  */
  for (i = 0; i < n; i++)
    {
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

      if (i == 0)
        {
          if (ph[i].p_vaddr != 0)
            ABORT ("first loadable vaddr is not 0");
        }
      else
        {
          if (ph[i].p_vaddr < ph[i-1].p_vaddr + ph[i-1].p_memsz)
            ABORT ("overlapping segments");

          if (ph[i].p_offset + ph[i].p_filesz > len)
            ABORT ("segment beyond end of byte array");
        }
    }

  if (dynamic_segment < 0)
    ABORT ("no PT_DYNAMIC segment");

  if (!IS_ALIGNED ((scm_t_uintptr) data, alignment))
    ABORT ("incorrectly aligned base");

  /* Allow writes to writable pages.  */
  if (is_read_only)
    {
#ifdef HAVE_SYS_MMAN_H
      for (i = 0; i < n; i++)
        {
          if (ph[i].p_flags == PF_R)
            continue;
          if (ph[i].p_align != 4096)
            continue;

          if (mprotect (data + ph[i].p_vaddr,
                        ph[i].p_memsz,
                        segment_flags_to_prot (ph[i].p_flags)))
            goto cleanup;
        }
#else
      ABORT ("expected writable pages");
#endif
    }

  if ((err_msg = process_dynamic_segment (data, &ph[dynamic_segment],
                                          &init, &entry, &frame_maps)))
    goto cleanup;

  if (scm_is_true (init))
    scm_call_0 (init);

  register_elf (data, len, frame_maps);

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

#define SCM_PAGE_SIZE 4096

static char*
map_file_contents (int fd, size_t len, int *is_read_only)
#define FUNC_NAME "load-thunk-from-file"
{
  char *data;

#ifdef HAVE_SYS_MMAN_H
  data = mmap (NULL, len, PROT_READ, MAP_PRIVATE, fd, 0);
  if (data == MAP_FAILED)
    SCM_SYSERROR;
  *is_read_only = 1;
#else
  if (lseek (fd, 0, SEEK_START) < 0)
    {
      int errno_save = errno;
      (void) close (fd);
      errno = errno_save;
      SCM_SYSERROR;
    }

  /* Given that we are using the read fallback, optimistically assume
     that the .go files were made with 8-byte alignment.
     alignment.  */
  data = malloc (end);
  if (!data)
    {
      (void) close (fd);
      scm_misc_error (FUNC_NAME, "failed to allocate ~A bytes",
                      scm_list_1 (scm_from_size_t (end)));
    }

  if (full_read (fd, data, end) != end)
    {
      int errno_save = errno;
      (void) close (fd);
      errno = errno_save;
      if (errno)
        SCM_SYSERROR;
      scm_misc_error (FUNC_NAME, "short read while loading objcode",
                      SCM_EOL);
    }

  /* If our optimism failed, fall back.  */
  {
    unsigned alignment = sniff_elf_alignment (data, end);

    if (alignment != 8)
      {
        char *copy = copy_and_align_elf_data (data, end, alignment);
        free (data);
        data = copy;
      }
  }

  *is_read_only = 0;
#endif

  return data;
}
#undef FUNC_NAME

SCM_DEFINE (scm_load_thunk_from_file, "load-thunk-from-file", 1, 0, 0,
	    (SCM filename),
	    "")
#define FUNC_NAME s_scm_load_thunk_from_file
{
  char *c_filename;
  int fd, is_read_only;
  off_t end;
  char *data;

  SCM_VALIDATE_STRING (1, filename);

  c_filename = scm_to_locale_string (filename);
  fd = open (c_filename, O_RDONLY | O_BINARY | O_CLOEXEC);
  free (c_filename);
  if (fd < 0) SCM_SYSERROR;

  end = lseek (fd, 0, SEEK_END);
  if (end < 0)
    SCM_SYSERROR;

  data = map_file_contents (fd, end, &is_read_only);

  (void) close (fd);

  return load_thunk_from_memory (data, end, is_read_only);
}
#undef FUNC_NAME

SCM_DEFINE (scm_load_thunk_from_memory, "load-thunk-from-memory", 1, 0, 0,
	    (SCM bv),
	    "")
#define FUNC_NAME s_scm_load_thunk_from_memory
{
  char *data;
  size_t len;

  SCM_VALIDATE_BYTEVECTOR (1, bv);

  data = (char *) SCM_BYTEVECTOR_CONTENTS (bv);
  len = SCM_BYTEVECTOR_LENGTH (bv);

  /* Copy data in order to align it, to trace its GC roots and
     writable sections, and to keep it in memory.  */

  data = copy_and_align_elf_data (data, len);

  return load_thunk_from_memory (data, len, 0);
}
#undef FUNC_NAME



struct mapped_elf_image
{
  char *start;
  char *end;
  char *frame_maps;
};

static struct mapped_elf_image *mapped_elf_images = NULL;
static size_t mapped_elf_images_count = 0;
static size_t mapped_elf_images_allocated = 0;

static size_t
find_mapped_elf_insertion_index (char *ptr)
{
  /* "mapped_elf_images_count" must never be dereferenced.  */
  size_t start = 0, end = mapped_elf_images_count;

  while (start < end)
    {
      size_t n = start + (end - start) / 2;

      if (ptr < mapped_elf_images[n].end)
        end = n;
      else
        start = n + 1;
    }

  return start;
}

static void
register_elf (char *data, size_t len, char *frame_maps)
{
  scm_i_pthread_mutex_lock (&scm_i_misc_mutex);
  {
    /* My kingdom for a generic growable sorted vector library.  */
    if (mapped_elf_images_count == mapped_elf_images_allocated)
      {
        struct mapped_elf_image *prev;
        size_t n;

        if (mapped_elf_images_allocated)
          mapped_elf_images_allocated *= 2;
        else
          mapped_elf_images_allocated = 16;

        prev = mapped_elf_images;
        mapped_elf_images =
          scm_gc_malloc_pointerless (sizeof (*mapped_elf_images)
                                     * mapped_elf_images_allocated,
                                     "mapped elf images");

        for (n = 0; n < mapped_elf_images_count; n++)
          {
            mapped_elf_images[n].start = prev[n].start;
            mapped_elf_images[n].end = prev[n].end;
            mapped_elf_images[n].frame_maps = prev[n].frame_maps;
          }
      }

    {
      size_t end;
      size_t n = find_mapped_elf_insertion_index (data);

      for (end = mapped_elf_images_count; n < end; end--)
        {
          const struct mapped_elf_image *prev = &mapped_elf_images[end - 1];
          mapped_elf_images[end].start = prev->start;
          mapped_elf_images[end].end = prev->end;
          mapped_elf_images[end].frame_maps = prev->frame_maps;
        }
      mapped_elf_images_count++;

      mapped_elf_images[n].start = data;
      mapped_elf_images[n].end = data + len;
      mapped_elf_images[n].frame_maps = frame_maps;
    }
  }
  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);
}

static struct mapped_elf_image *
find_mapped_elf_image_unlocked (char *ptr)
{
  size_t n = find_mapped_elf_insertion_index ((char *) ptr);

  if (n < mapped_elf_images_count
      && mapped_elf_images[n].start <= ptr
      && ptr < mapped_elf_images[n].end)
    return &mapped_elf_images[n];

  return NULL;
}

static int
find_mapped_elf_image (char *ptr, struct mapped_elf_image *image)
{
  int result;

  scm_i_pthread_mutex_lock (&scm_i_misc_mutex);
  {
    struct mapped_elf_image *img = find_mapped_elf_image_unlocked (ptr);
    if (img)
      {
        memcpy (image, img, sizeof (*image));
        result = 1;
      }
    else
      result = 0;
  }
  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);

  return result;
}

static SCM
scm_find_mapped_elf_image (SCM ip)
{
  struct mapped_elf_image image;

  if (find_mapped_elf_image ((char *) scm_to_uintptr_t (ip), &image))
    {
      signed char *data = (signed char *) image.start;
      size_t len = image.end - image.start;

      return scm_c_take_gc_bytevector (data, len, SCM_BOOL_F);
    }

  return SCM_BOOL_F;
}

static SCM
scm_all_mapped_elf_images (void)
{
  SCM result = SCM_EOL;

  scm_i_pthread_mutex_lock (&scm_i_misc_mutex);
  {
    size_t n;
    for (n = 0; n < mapped_elf_images_count; n++)
      {
        signed char *data = (signed char *) mapped_elf_images[n].start;
        size_t len = mapped_elf_images[n].end - mapped_elf_images[n].start;
        result = scm_cons (scm_c_take_gc_bytevector (data, len, SCM_BOOL_F),
                           result);
      }
  }
  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);

  return result;
}

struct frame_map_prefix
{
  scm_t_uint32 text_offset;
  scm_t_uint32 maps_offset;
};

struct frame_map_header
{
  scm_t_uint32 addr;
  scm_t_uint32 map_offset;
};

verify (sizeof (struct frame_map_prefix) == 8);
verify (sizeof (struct frame_map_header) == 8);

const scm_t_uint8 *
scm_find_dead_slot_map_unlocked (const scm_t_uint32 *ip)
{
  struct mapped_elf_image *image;
  char *base;
  struct frame_map_prefix *prefix;
  struct frame_map_header *headers;
  scm_t_uintptr addr = (scm_t_uintptr) ip;
  size_t start, end;

  image = find_mapped_elf_image_unlocked ((char *) ip);
  if (!image || !image->frame_maps)
    return NULL;

  base = image->frame_maps;
  prefix = (struct frame_map_prefix *) base;
  headers = (struct frame_map_header *) (base + sizeof (*prefix));

  if (addr < ((scm_t_uintptr) image->start) + prefix->text_offset)
    return NULL;
  addr -= ((scm_t_uintptr) image->start) + prefix->text_offset;

  start = 0;
  end = (prefix->maps_offset - sizeof (*prefix)) / sizeof (*headers);

  if (end == 0 || addr > headers[end - 1].addr)
    return NULL;

  while (start < end)
    {
      size_t n = start + (end - start) / 2;

      if (addr == headers[n].addr)
        return (const scm_t_uint8*) (base + headers[n].map_offset);
      else if (addr < headers[n].addr)
        end = n;
      else
        start = n + 1;
    }

  return NULL;
}


void
scm_bootstrap_loader (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_loader",
                            (scm_t_extension_init_func)scm_init_loader, NULL);
}

void
scm_init_loader (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/loader.x"
#endif

  scm_c_define_gsubr ("find-mapped-elf-image", 1, 0, 0,
                      (scm_t_subr) scm_find_mapped_elf_image);
  scm_c_define_gsubr ("all-mapped-elf-images", 0, 0, 0,
                      (scm_t_subr) scm_all_mapped_elf_images);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
