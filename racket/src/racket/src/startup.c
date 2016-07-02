/*
  Racket
  Copyright (c) 2004-2016 PLT Design Inc.
  Copyright (c) 2000-2001 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"
#include "schminc.h"

static Scheme_Linklet *eval_linklet_string(const char *str, intptr_t len, int extract)
{
  Scheme_Object *port, *expr;

  if (len < 0)
    len = strlen(str);
  port = scheme_make_sized_byte_string_input_port(str, -len); /* negative means it's constant */

  expr = scheme_internal_read(port, NULL, 1, 1, 0, 0, -1, NULL, NULL);

  if (extract) {
    /* expr is a linklet bundle; 'startup is mapped to the linklet */
    return (Scheme_Linklet *)scheme_hash_tree_get((Scheme_Hash_Tree *)SCHEME_PTR_VAL(expr),
                                                  scheme_intern_symbol("startup"));
  } else {
    return scheme_compile_and_optimize_linklet(scheme_datum_to_syntax(expr, scheme_false, 0),
                                               scheme_intern_symbol("startup"));
  }
}

Scheme_Linklet *scheme_startup_linklet()
{
#define EVAL_ONE_STR(str) return eval_linklet_string(str, -1, 0)
#define EVAL_ONE_SIZED_STR(str, len) return eval_linklet_string(str, len, 1)

#if USE_COMPILED_STARTUP
  
  /* Generated by the build process in the build area; might simply
     redirect to "startup.inc": */
# include "cstartup.inc"
#else
  /* Generated from a Racket program, but included with sources in
     this form: */
# include "startup.inc"
#endif
}
