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

/* Primitives for the toplevel */

#include <string.h>
#include "caml/alloc.h"
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/fix_code.h"
#include "caml/interp.h"
#include "caml/intext.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/prims.h"
#include "caml/fiber.h"
#include "caml/params.h"

#ifndef NATIVE_CODE

CAMLprim value caml_get_global_data(value unit)
{
  return caml_read_root(caml_global_data);
}

CAMLprim value caml_get_section_table(value unit)
{
  if (caml_params->section_table == NULL) caml_raise_not_found();
  return caml_input_value_from_block(caml_params->section_table,
                                     caml_params->section_table_size);
}

CAMLprim value caml_reify_bytecode(value prog, value len)
{
#ifdef ARCH_BIG_ENDIAN
  caml_fixup_endianness((code_t) prog, (asize_t) Long_val(len));
#endif
#ifdef THREADED_CODE
  caml_thread_code((code_t) prog, (asize_t) Long_val(len));
#endif
  return caml_alloc_1(Closure_tag, Val_bytecode(prog));
}

CAMLprim value caml_register_code_fragment(value prog, value len, value digest)
{
  struct code_fragment * cf = caml_stat_alloc(sizeof(struct code_fragment));
  cf->code_start = (char *) prog;
  cf->code_end = (char *) prog + Long_val(len);
  memcpy(cf->digest, String_val(digest), 16);
  cf->digest_computed = 1;
  caml_ext_table_add(&caml_code_fragments_table, cf);
  return Val_unit;
}

CAMLprim value caml_realloc_global(value size)
{
  CAMLparam1(size);
  CAMLlocal2(old_global_data, new_global_data);
  mlsize_t requested_size, actual_size, i;
  old_global_data = caml_read_root(caml_global_data);

  requested_size = Long_val(size);
  actual_size = Wosize_val(old_global_data);
  if (requested_size >= actual_size) {
    requested_size = (requested_size + 0x100) & 0xFFFFFF00;
    caml_gc_log ("Growing global data to %u entries",
                 (unsigned)requested_size);
    new_global_data = caml_alloc_shr(requested_size, 0);
    for (i = 0; i < actual_size; i++)
      caml_initialize_field(new_global_data, i, Field_imm(old_global_data, i));
    for (i = actual_size; i < requested_size; i++){
      caml_initialize_field(new_global_data, i, Val_long(0));
    }
    caml_modify_root(caml_global_data, new_global_data);
  }
  CAMLreturn (Val_unit);
}

CAMLprim value caml_get_current_environment(value unit)
{
  return *CAML_DOMAIN_STATE->extern_sp;
}

CAMLprim value caml_invoke_traced_function(value codeptr, value env, value arg)
{
  struct caml_domain_state* domain_state = CAML_DOMAIN_STATE;
  /* Stack layout on entry:
       return frame into instrument_closure function
       arg3 to call_original_code (arg)
       arg2 to call_original_code (env)
       arg1 to call_original_code (codeptr)
       arg3 to call_original_code (arg)
       arg2 to call_original_code (env)
       saved env */

  /* Stack layout on exit:
       return frame into instrument_closure function
       actual arg to code (arg)
       pseudo return frame into codeptr:
         extra_args = 0
         environment = env
         PC = codeptr
       arg3 to call_original_code (arg)                   same 6 bottom words as
       arg2 to call_original_code (env)                   on entrance, but
       arg1 to call_original_code (codeptr)               shifted down 4 words
       arg3 to call_original_code (arg)
       arg2 to call_original_code (env)
       saved env */

  value * osp, * nsp;
  int i;

  osp = domain_state->extern_sp;
  domain_state->extern_sp -= 4;
  nsp = domain_state->extern_sp;
  for (i = 0; i < 6; i++) nsp[i] = osp[i];
  nsp[6] = codeptr;
  nsp[7] = env;
  nsp[8] = Val_int(0);
  nsp[9] = arg;
  return Val_unit;
}

#else

/* Dummy definitions to support compilation of ocamlc.opt */

value caml_get_global_data(value unit)
{
  caml_invalid_argument("Meta.get_global_data");
  return Val_unit; /* not reached */
}

value caml_get_section_table(value unit)
{
  caml_invalid_argument("Meta.get_section_table");
  return Val_unit; /* not reached */
}

value caml_realloc_global(value size)
{
  caml_invalid_argument("Meta.realloc_global");
  return Val_unit; /* not reached */
}

value caml_invoke_traced_function(value codeptr, value env, value arg)
{
  caml_invalid_argument("Meta.invoke_traced_function");
  return Val_unit; /* not reached */
}

value caml_reify_bytecode(value prog, value len)
{
  caml_invalid_argument("Meta.reify_bytecode");
  return Val_unit; /* not reached */
}

#endif
