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

/* Main entry point (can be overridden by a user-provided main()
   function that calls caml_main() later). */

#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/sys.h"

CAMLextern void caml_main (char **);

#ifdef _WIN32
CAMLextern void caml_expand_command_line (int *, char ***);
#endif

int main(int argc, char **argv)
{
#ifdef DEBUG
  caml_gc_log ("### OCaml runtime: debug mode ###");
#if 0
  {
    int i;
    char *ocp;
    char *cp;

    ocp = getenv ("OCAMLRUNPARAM");
    caml_gc_log ("### OCAMLRUNPARAM=%s", ocp == NULL ? "" : ocp);
    cp = getenv ("CAMLRUNPARAM");
    caml_gc_log ("### CAMLRUNPARAM=%s", cp == NULL ? "" : cp);
    caml_gc_log ("### working dir: %s", getcwd (NULL, 0));
  }
#endif
#endif
#ifdef _WIN32
  /* Expand wildcards and diversions in command line */
  caml_expand_command_line(&argc, &argv);
#endif
  caml_main(argv);
  caml_sys_exit(Val_int(0));
  return 0; /* not reached */
}
