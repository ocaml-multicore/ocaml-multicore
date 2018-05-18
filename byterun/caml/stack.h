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
#include "domain_state.h"
#include "misc.h"


#ifdef CAML_INTERNALS

#ifdef TARGET_amd64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#endif

#ifdef TARGET_arm64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#endif

/*

  Stack layout.

  In order to support effect handlers efficiently, OCaml fibers run on
  individual small stacks, managed separately from the system-provided C
  stack.

  The compiler inserts frequent stack overflow checks to ensure there is
  enough space on the current stack. If not, the stack is enlarged and
  moved (but kept contiguous). C compilers do not perform these checks, and
  C code cannot handle the stack moving, so C calls must be performed on
  the system-provided C stack.

  This means that the C and OCaml stack frames of a single fiber which does
  multiple calls between C and OCaml are spread over two separate stacks,
  using various structures (defined below) to maintain links between them.

  Here is the layout of a fiber that does a C->OCaml call, which uses
  caml_callback to call back to OCaml, which then does another C call:

                                                      ^
         C stack                   OCaml stack        |
            |                  +-----------------+    | parent stack
            |                  |                 |    |
            |                  |    fiber_link   |----+
            |                  |                 |
            |                  |-----------------|
            |                  .                 .
            |                  .     OCaml       .
            |                  .     frames      .
            |                  .                 .
   +-----------------+         |-----------------|
   | cstack_ccall    |-------> |  fiber_context  |
   |-----------------|         +-----------------+
   .                 .                  |
   .        C        .                  |
   .      frames     .                  |
   .                 .                  |
   |-----------------|         +-----------------+
   | cstack_callback | <-------|  fiber_callback |
   +-----------------+         |-----------------|
            |                  .                 .
            |                  .     OCaml       .
            |                  .     frames      .
            |                  .                 .
   +-----------------+         |-----------------|
   | cstack_call     |-------> |  fiber_context  |
   +-----------------+         +-----------------+
   .                 .         .                 .
   .        C        .         . [unused stack]  .
   .      frames     .         .                 .
   .                 .         .                 .
                               +--------+--+-----+
                               | length |GC| tag |
                               + -------+--+-----+

  All of these structures are immutable once pushed to the stack, except
  that the GC will update pointers if a stack moves.

  The sections marked '|' consume no bytes: the fiber_callback structure is
  pushed just below the fiber_context structure, and the cstack_call
  structure is just below cstack_callback. This is represented in the
  struct definitions below by including fiber_context as the last member of
  fiber_callback. (Since stacks grow down, the last member of a structure
  is the part that was pushed first).

*/

/* pushed to an OCaml fiber during 'try' blocks */
struct fiber_exn_frame {
  uintnat next_offset;
  uintnat handler_pc;
};

/* at the top of a fiber stack */
struct fiber_link {
  /* used to catch exceptions escaping the fiber */
  struct fiber_exn_frame exn;

  value handle_val;
  value handle_eff;
  value handle_exn;
  value parent;
  struct fiber_context* sp;
  struct domain* dirty_domain;
};

/* pushed to an OCaml fiber stack when the fiber pauses, in order
   to perform an effect, handle effects from another fiber, or call C */
struct fiber_context {
  uintnat exception_handler_offset; /* pointer to the exception trap frame */
  value* gc_regs;           /* pointer to register block (may be NULL) */
  uintnat pc;                /* return address */
};


/* pushed to an OCaml fiber when calling OCaml from C */
struct fiber_callback {
  struct fiber_exn_frame exn;
  struct cstack_callback* callback;

  /* previously-pushed fiber context */
  struct fiber_context ctx;
};

/* pushed to the C stack when calling OCaml from C */
struct cstack_callback {
  value fiber; /* the fiber being called into */
  struct cstack_callback* prev; /* previous C frame, or NULL if top level */
};

/* pushed to the C stack when calling C from OCaml */
struct cstack_ccall {
  struct fiber_context* caml;   /* fiber that paused to make this call */

  /* previously-pushed C callback context */
  struct cstack_callback callback;
};

static inline struct fiber_link* fiber_link(value fib)
{
  uintnat stack_end;
  CAMLassert (Is_block(fib) && Tag_val(fib) == Stack_tag);
  stack_end = (uintnat)(fib + Wosize_val(fib));
  stack_end &= (uintnat)-16;    /* stacks are aligned to 16 bytes */
  return (struct fiber_link*)(stack_end - sizeof(struct fiber_link));
}

/* When running C code, all cstack_callbacks have a cstack_ccall
   just underneath them */
static inline struct cstack_ccall* cstack_call(struct cstack_callback* cb)
{
  CAMLassert (cb != NULL);
  return
    (struct cstack_ccall*)
    ((char*)cb - offsetof(struct cstack_ccall, callback));
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
