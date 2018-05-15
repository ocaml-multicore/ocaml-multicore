/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Machine-dependent interface with the asm code */

#ifndef CAML_STACK_H
#define CAML_STACK_H
#include <stddef.h>
#include "caml/domain_state.h"
#include "caml/misc.h"


#ifdef CAML_INTERNALS

#ifdef TARGET_amd64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#endif

#ifdef TARGET_arm64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#endif



/*

*/

/* pushed to the C stack when calling OCaml from C */
struct callback_context {
  value fiber; /* the fiber being called into */
  struct callback_context* prev; /* previous C frame, or NULL if top level */
};

/* pushed to an OCaml fiber stack when the fiber pauses, in order
   to perform an effect, handle effects from another fiber, or call C */
struct caml_context {
  uintnat  exception_handler_offset; /* pointer to the exception trap frame */
  value* gc_regs;           /* pointer to register block (may be NULL) */
  void*  pc;                /* return address */
};

/* pushed to the C stack when calling C from OCaml */
struct c_call_context {
  struct caml_context* caml;   /* fiber that paused to make this call */
  struct callback_context callback; /* enclosing C frame */
};

struct caml_exn_frame {
  uintnat next_offset;
  void* handler_pc;
};

struct caml_start_context {
  struct caml_exn_frame exn;
  struct callback_context* callback;
};

static inline struct c_call_context* get_c_call_context(struct callback_context* cb)
{
  CAMLassert (cb != NULL);
  /* Since this is C code, the callback_context must be part of an
     enclosing c_call_context */
  return
    (struct c_call_context*)
    ((char*)Caml_state->c_context -
     offsetof(struct c_call_context, callback));    
}

/* Declaration of variables used in the asm code */
extern value * caml_globals[];
extern intnat caml_globals_inited;
extern intnat * caml_frametable[];

/* XXX KC
CAMLextern frame_descr * caml_next_frame_descriptor(uintnat * pc, char ** sp);
 */

#endif /* CAML_INTERNALS */

#endif /* CAML_STACK_H */
