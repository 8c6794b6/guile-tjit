/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2003 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */




#include <string.h>

#include "libguile/_scm.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/hashtab.h"

#include "libguile/validate.h"
#include "libguile/keywords.h"


scm_t_bits scm_tc16_keyword;

static int
keyword_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  SCM symbol = SCM_KEYWORDSYM (exp);

  scm_puts ("#:", port);
  scm_print_symbol_name (SCM_SYMBOL_CHARS (symbol) + 1,
			 SCM_SYMBOL_LENGTH (symbol) - 1,
			 port);
  scm_remember_upto_here_1 (symbol);
  return 1;
}

SCM_DEFINE (scm_make_keyword_from_dash_symbol, "make-keyword-from-dash-symbol", 1, 0, 0, 
            (SCM symbol),
            "Make a keyword object from a @var{symbol} that starts with a dash.")
#define FUNC_NAME s_scm_make_keyword_from_dash_symbol
{
  SCM keyword;

  SCM_ASSERT (SCM_SYMBOLP (symbol)
	      && ('-' == SCM_SYMBOL_CHARS(symbol)[0]),
	      symbol, SCM_ARG1, FUNC_NAME);

  SCM_DEFER_INTS;
  keyword = scm_hashq_ref (scm_keyword_obarray, symbol, SCM_BOOL_F);
  if (SCM_FALSEP (keyword))
    {
      SCM_NEWSMOB (keyword, scm_tc16_keyword, SCM_UNPACK (symbol));
      scm_hashq_set_x (scm_keyword_obarray, symbol, keyword);
    }
  SCM_ALLOW_INTS;
  return keyword;
}
#undef FUNC_NAME

SCM
scm_c_make_keyword (char *s)
{
  char *buf = scm_malloc (strlen (s) + 2);
  SCM symbol;

  buf[0] = '-';
  strcpy (buf + 1, s);
  symbol = scm_str2symbol (buf);
  free (buf);

  return scm_make_keyword_from_dash_symbol (symbol);
}

SCM_DEFINE (scm_keyword_p, "keyword?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if the argument @var{obj} is a keyword, else\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_keyword_p
{
  return SCM_BOOL (SCM_KEYWORDP (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_keyword_dash_symbol, "keyword-dash-symbol", 1, 0, 0, 
            (SCM keyword),
	    "Return the dash symbol for @var{keyword}.\n"
	    "This is the inverse of @code{make-keyword-from-dash-symbol}.")
#define FUNC_NAME s_scm_keyword_dash_symbol
{
  SCM_VALIDATE_KEYWORD (1, keyword);
  return SCM_KEYWORDSYM (keyword);
}
#undef FUNC_NAME



void
scm_init_keywords ()
{
  scm_tc16_keyword = scm_make_smob_type ("keyword", 0);
  scm_set_smob_mark (scm_tc16_keyword, scm_markcdr);
  scm_set_smob_print (scm_tc16_keyword, keyword_print);

  scm_keyword_obarray = scm_c_make_hash_table (0);
#include "libguile/keywords.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
