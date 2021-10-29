#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>

#include <unistd.h>

static int _Atomic counter = 0;

value spend_time_idling_in_c(value val) {
    CAMLparam0();

    atomic_store(&counter, 1);

    while( atomic_load(&counter) < 1000000 ) {
        sleep(1);
    }

    CAMLreturn(Val_unit);
}

value get_thing(value val) {
    CAMLparam0();

    CAMLreturn(Val_long(atomic_load(&counter)));
}

value increment_thing(value val) {
    CAMLparam0();

    caml_enter_blocking_section();
    atomic_fetch_add(&counter, 1);
    caml_leave_blocking_section();

    CAMLreturn(Val_unit);
}