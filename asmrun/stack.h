/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Machine-dependent interface with the asm code */

#ifndef CAML_STACK_H
#define CAML_STACK_H

/* Macros to access the stack frame */

#ifdef TARGET_sparc
#define Saved_return_address(sp) *((intnat *)((sp) + 92))
#define Callback_link(sp) ((struct caml_context *)((sp) + 104)) //FIXME KC
#endif

#ifdef TARGET_i386
#define Saved_return_address(sp) *((intnat *)((sp) - 4))
#ifndef SYS_win32
#define Callback_link(sp) ((struct caml_context *)((sp) + 16)) //FIXME KC
#else
#define Callback_link(sp) ((struct caml_context *)((sp) + 8)) //FIXME KC
#endif
#endif

#ifdef TARGET_power
#define Saved_return_address(sp) *((intnat *)((sp) - SIZEOF_PTR))
#define Already_scanned(sp, retaddr) ((retaddr) & 1)
#define Mark_scanned(sp, retaddr) \
          (*((intnat *)((sp) - SIZEOF_PTR)) = (retaddr) | 1)
#define Mask_already_scanned(retaddr) ((retaddr) & ~1)
#ifdef SYS_aix
#define Trap_frame_size 32
#else
#define Trap_frame_size 16
#endif
#define Callback_link(sp) ((struct caml_context *)((sp) + Trap_frame_size)) //FIXME KC
#endif

#ifdef TARGET_arm
#define Saved_return_address(sp) *((intnat *)((sp) - 4))
#define Callback_link(sp) ((struct caml_context *)((sp) + 8)) //FIXME KC
#endif

#ifdef TARGET_amd64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define Next_chunk_offset 16
#endif

#ifdef TARGET_arm64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define Callback_link(sp) ((struct caml_context *)((sp) + 16)) //FIXME KC
#endif

/* Structure of OCaml callback contexts */

struct caml_context {
  uintnat exception_ptr_offset; /* exception pointer offset from top of stack */
  value * gc_regs;              /* pointer to register block */
};

extern uintnat caml_stack_usage (void);

/* Declaration of variables used in the asm code */
extern value caml_globals[];
extern intnat caml_globals_inited;
extern intnat * caml_frametable[];

#endif /* CAML_STACK_H */
