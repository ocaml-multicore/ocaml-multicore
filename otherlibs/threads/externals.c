#define CAML_INTERNALS

#include "caml/callback.h"
#include "caml/domain.h"
#include "caml/mlvalues.h"
#include "caml/sys.h"

/* OS-specific code */
#ifdef _WIN32
#include "externals-win32.h"
#else
#include "externals-posix.h"
#endif

#include <stdio.h>

/* Hooks for caml_enter_blocking_section and caml_leave_blocking_section */

static void caml_threads_enter_blocking_section(void)
{
  static const value *f = NULL;
  if (f == NULL) f = caml_named_value("threads_enter_blocking_section");
  if (f != NULL) caml_callback_exn(*f, Val_unit);
}

static void caml_threads_leave_blocking_section(void)
{
  static const value *f = NULL;
  if (f == NULL) f = caml_named_value("threads_leave_blocking_section");
  if (f != NULL) caml_callback_exn(*f, Val_unit);
}

CAMLprim value caml_threads_init(value unit)
{
  caml_enter_blocking_section_hook = caml_threads_enter_blocking_section;
  caml_leave_blocking_section_hook = caml_threads_leave_blocking_section;

  return Val_unit;
}
