/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* To walk the memory roots for garbage collection */

#include "finalise.h"
#include "globroots.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "fiber.h"
#include "major_gc.h"
#include "shared_heap.h"
#include "fiber.h"

#ifdef NATIVE_CODE
#include "stack.h"
/* Communication with [caml_start_program] and [caml_call_gc]. */

/* The global roots.
   FIXME: These should be promoted, and not scanned here.
   FIXME: caml_globals_inited makes assumptions about store ordering.
   XXX KC : What to do here?
*/

intnat caml_globals_inited = 0;
static intnat caml_globals_scanned = 0;
#endif

CAMLexport __thread struct caml__roots_block *caml_local_roots = NULL;

CAMLexport void caml_do_local_roots (scanning_action f, struct domain* domain)
{
  struct caml__roots_block *lr;
  int i, j;
  value* sp;

#ifdef NATIVE_CODE
  /* The global roots.
     FIXME: These should be promoted, and not scanned here.
     FIXME: caml_globals_inited makes assumptions about store ordering.
  */
  value glob;
  for (i = 0; i <= caml_globals_inited && caml_globals[i] != 0; i++) {
    glob = caml_globals[i];
    for (j = 0; j < Wosize_val(glob); j++){
      f(Op_val(glob)[j], &Op_val(glob)[j]);
    }
  }
#endif

  f(domain->state->current_stack, &(domain->state->current_stack));
  for (lr = *(domain->local_roots); lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        if (*sp != 0) {
          f (*sp, sp);
        }
      }
    }
  }
}
