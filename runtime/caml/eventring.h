/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                          Sadiq Jaffer, Opsian                          */
/*                                                                        */
/*   Copyright 2021 Opsian Ltd                                            */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_EVENTRING_H
#define CAML_EVENTRING_H

#include "eventlog.h"
#include "mlvalues.h"
#include <stdint.h>

#ifdef CAML_INSTR
#define CAML_EV_ALLOC(s) caml_ev_alloc(s)
#define CAML_EV_ALLOC_FLUSH() caml_ev_alloc_flush()
#else
#define CAML_EV_ALLOC(s)      /**/
#define CAML_EV_ALLOC_FLUSH() /**/
#endif

#define CAML_EV_FLUSH() caml_ev_flush()

/* external C-API for reading from the eventring */
struct caml_eventring_cursor;

struct caml_eventring_callbacks {
  void (*ev_runtime_begin)(void *callback_data, uint64_t timestamp,
                           ev_runtime_phase phase);
  void (*ev_runtime_end)(void *callback_data, uint64_t timestamp,
                         ev_runtime_phase phase);
  void (*ev_runtime_counter)(void *callback_data, uint64_t timestamp,
                             ev_runtime_counter counter, uint64_t val);
  void (*ev_alloc)(void *callback_data, uint64_t timestamp, uint64_t *sz);
  void (*ev_lifecycle)(void *callback_data, int64_t timestamp,
                       ev_lifecycle lifecycle, int64_t data);
  void (*ev_lost_events)(void *callback_data, int lost_events);
};

/* Starts eventring. Needs to be called before [caml_eventring_create_cursor] */
extern value caml_eventring_start();
extern value caml_eventring_pause();
extern value caml_eventring_resume();

/* [eventring_path] is a path to a directory containing eventrings. [pid] is the
    process id (or equivalent) of the startup OCaml process. This function will
    return a cursor which can we be used with caml_eventring_read_poll to read
    events from the eventrings. */
extern struct caml_eventring_cursor* caml_eventring_create_cursor(const char *eventring_path, int pid);

/* frees a cursor obtained from caml_eventring_creator_cursor */
extern void caml_eventring_free_cursor(struct caml_eventring_cursor *cursor);

/* polls the eventring pointed to by [cursor] and calls the appropriate callback
    provided in [callbacks] for each new event up to at most [max_events] times. 
    Returns the number of events consumed.
    
    0 or negative [max_events] indicates no limit to the number of callbacks. */
CAMLextern int caml_eventring_read_poll(struct caml_eventring_cursor *cursor,
                         struct caml_eventring_callbacks *callbacks,
                         void *callback_data,
                         int max_events);

/* OCaml API for reading from the eventring */
extern value caml_eventring_create_wrapped_cursor(value path_pid);
extern value caml_eventring_free_wrapped_cursor(value wrapped_cursor);
extern value caml_eventring_read_poll_wrapped(value wrapped_cursor,
                                              value callbacks,
                                              value max_events_option);

#ifdef CAML_INTERNALS

/* Functions for putting runtime data on to the eventring */

void caml_eventring_init();
void caml_eventring_destroy();
void caml_ev_begin(ev_runtime_phase phase);
void caml_ev_end(ev_runtime_phase phase);
void caml_ev_counter(ev_runtime_counter counter, uint64_t val);
void caml_ev_lifecycle(ev_lifecycle lifecycle, int64_t data);
void caml_ev_alloc(uint64_t sz);
void caml_ev_alloc_flush();
void caml_ev_flush();

#endif /* CAML_INTERNALS */

#endif /*CAML_EVENTRING_H*/
