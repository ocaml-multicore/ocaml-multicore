/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*          Damien Doligez, projet Moscova, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#include "caml/misc.h"
#include "caml/fail.h"

/* Put (f,v) in the recent set. */
CAMLprim value caml_final_register (value f, value v)
{
  caml_failwith("finalisers unimplemented");
}

CAMLprim value caml_final_release (value unit)
{
  caml_failwith("finalisers unimplemented");
}
