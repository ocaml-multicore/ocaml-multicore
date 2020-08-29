module IntArg = struct
  type t = int

  let compare (x : int) (y : int) = if x < y then -1 else if x > y then 1 else 0
end

module IntSet = Set.Make (IntArg)

let add_poll_before (f : Mach.instruction) : Mach.instruction =
  let new_live = Reg.Set.union f.live (Reg.Set.of_seq (Array.to_seq f.arg)) in
  {
    desc = Iop Ipoll;
    next = f;
    arg = Array.make 0 Reg.dummy;
    res = Array.make 0 Reg.dummy;
    dbg = f.dbg;
    live = new_live;
    available_before = f.available_before;
    available_across = f.available_across;
  }

type allocation_result = Allocation | NoAllocation | Exited

let combine_paths p0 p1 = match p0, p1 with Allocation, Allocation -> Allocation
| Exited, _ | _, Exited -> Exited
| NoAllocation, _ | _, NoAllocation -> NoAllocation

let rec reduce_paths_array arr =
  let rec red_arr acc arr n =
    match n with 
    | 0 -> acc
    | _ ->
      let curr_path = check_path arr.(n) in
        let new_acc = match acc with 
        | None -> curr_path
        | Some(v) -> combine_paths v curr_path
        in red_arr (Some new_acc) arr (n-1)
  in 
  let res = red_arr None arr ((Array.length arr)-1) in
    match res with
    | None -> NoAllocation
    | Some(v) -> v
and reduce_paths_list l =
  let rec red_list acc l =
    match l with 
    | [] -> acc
    | (h :: tl) ->
      let curr_path = check_path h in
        let new_acc = match acc with 
        | None -> curr_path
        | Some(v) -> combine_paths v curr_path
        in red_list (Some new_acc) tl
  in 
  let res = red_list None l in
    match res with
    | None -> NoAllocation
    | Some(v) -> v
and check_path (f : Mach.instruction) : allocation_result =
  match f.desc with
  | Iifthenelse (_, i0, i1) ->
      (match combine_paths (check_path i0) (check_path i1) with
      | NoAllocation -> check_path f.next
      | pv -> pv)
  | Iswitch (_, cases) ->
      let case_state = reduce_paths_array cases in
      (match case_state with
      | NoAllocation -> check_path f.next
      | pv -> pv)
  | Icatch (_, handlers, body) ->
      let handlers_state = reduce_paths_list (List.map (fun (_,h) -> h) handlers) in
      (match combine_paths handlers_state (check_path body) with
      | NoAllocation -> check_path f.next
      | pv -> pv)
  | Itrywith (body, handler) ->
      (match combine_paths (check_path body) (check_path handler) with
      | NoAllocation -> check_path f.next
      | pv -> pv)
  | Ireturn
  | Iop (Itailcall_ind _)
  | Iop (Itailcall_imm _)
  | Iraise _ ->
      Exited
  | Iend | Iexit _ -> NoAllocation
  | Iop (Ialloc _) -> Allocation
  | Iop _ -> check_path f.next

(* this determines whether from a given instruction we unconditionally
   allocate and this is used to avoid adding polls unnecessarily *)
let allocates_unconditionally (i : Mach.instruction) =
  match check_path i with
  | Allocation -> true
  | NoAllocation | Exited -> false

let is_leaf_func_without_loops (fun_body : Mach.instruction) =
  let rec contains_calls (i : Mach.instruction) =
  match i.desc with
  | Iifthenelse (_, ifso, ifnot) ->
    (contains_calls ifso) || (contains_calls ifnot) || contains_calls i.next
  | Iswitch (_, cases) ->
    (Array.exists (fun c -> contains_calls c) cases) || contains_calls i.next
  | Icatch (rec_flag, handlers, body) ->
    begin
      match rec_flag with 
    | Recursive ->
      true
    | Nonrecursive ->
      (List.exists (fun (_, h) -> contains_calls h) handlers) || contains_calls body || contains_calls i.next
    end
  | Itrywith (body, handler) ->
    (contains_calls body) || (contains_calls handler) || contains_calls i.next
  | Iend -> false
  | Iop(Iextcall _ | Icall_ind _ | Icall_imm _ | Itailcall_imm _ | Itailcall_ind _) ->
      true
  | Ireturn | Iexit _| Iraise _ -> false
  | Iop _ -> contains_calls i.next
  in not(contains_calls fun_body)

(* finds_rec_handlers *)
let rec find_rec_handlers (f : Mach.instruction) =
  match f.desc with
  | Iifthenelse (_, i0, i1) ->
      let i0_rec_handlers = find_rec_handlers i0 in
      let i1_rec_handlers = find_rec_handlers i1 in
      let next_rec_handlers = find_rec_handlers f.next in
      IntSet.(union (union i0_rec_handlers i1_rec_handlers) next_rec_handlers)
  | Iswitch (_, cases) ->
      let case_rec_handlers =
        Array.fold_left
          (fun int_set case -> IntSet.union int_set (find_rec_handlers case))
          IntSet.empty cases
      in
      IntSet.union case_rec_handlers (find_rec_handlers f.next)
  | Icatch (rec_flag, handlers, body) -> (
      match rec_flag with
      | Recursive ->
          let rec_handlers =
            List.fold_left
              (fun int_set (id, handler) ->
                let inner_rec_handlers = find_rec_handlers handler in
                let current_rec_handlers = if not (allocates_unconditionally handler) then
                  IntSet.add id int_set
                else int_set in
                IntSet.union inner_rec_handlers current_rec_handlers)
              IntSet.empty handlers
          in
          let body_rec_handlers = find_rec_handlers body in
          IntSet.(
            union
              (union body_rec_handlers rec_handlers)
              (find_rec_handlers f.next))
      | Nonrecursive ->
          let non_rec_catch_handlers =
            List.fold_left
              (fun int_set (_, handler) ->
                IntSet.union int_set (find_rec_handlers handler))
              IntSet.empty handlers
          in
          let body_rec_handlers = find_rec_handlers body in
          IntSet.(
            union
              (union body_rec_handlers non_rec_catch_handlers)
              (find_rec_handlers f.next)) )
  | Itrywith (body, handler) ->
      let handler_rec_handler = find_rec_handlers handler in
      let body_rec_handlers = find_rec_handlers body in
      IntSet.(
        union
          (union body_rec_handlers handler_rec_handler)
          (find_rec_handlers f.next))
  | Iexit _ | Iend | Ireturn
  | Iop (Itailcall_ind _)
  | Iop (Itailcall_imm _)
  | Iraise _ ->
      IntSet.empty
  | Iop _ -> find_rec_handlers f.next

let rec add_polls_to_exit (forward_handlers : int list)
    (rec_handlers : IntSet.t) (f : Mach.instruction) =
  let forward_rec_add_polls = add_polls_to_exit forward_handlers rec_handlers in
  match f.desc with
  | Iifthenelse (test, i0, i1) ->
      {
        f with
        desc =
          Iifthenelse (test, forward_rec_add_polls i0, forward_rec_add_polls i1);
        next = forward_rec_add_polls f.next;
      }
  | Iswitch (index, cases) ->
      {
        f with
        desc = Iswitch (index, Array.map forward_rec_add_polls cases);
        next = forward_rec_add_polls f.next;
      }
  | Icatch (rec_flag, handlers, body) ->
      {
        f with
        desc =
          Icatch
            ( rec_flag,
              List.map
                (fun (idx, instrs) -> (idx, forward_rec_add_polls instrs))
                handlers,
              add_polls_to_exit
                (forward_handlers @ List.map (fun (i, _) -> i) handlers)
                rec_handlers body );
        next = forward_rec_add_polls f.next;
      }
  | Itrywith (body, handler) ->
      {
        f with
        desc =
          Itrywith (forward_rec_add_polls body, forward_rec_add_polls handler);
        next = forward_rec_add_polls f.next;
      }
  | Iexit id ->
      if
        IntSet.mem id rec_handlers
        && not (List.exists (fun a -> a == id) forward_handlers)
      then
        let new_f = add_poll_before f in
        { new_f with next = { f with next = forward_rec_add_polls f.next } }
      else { f with Mach.next = forward_rec_add_polls f.next }
  | Iend | Ireturn | Iop (Itailcall_ind _) | Iop (Itailcall_imm _) | Iraise _ ->
      f
  | Iop _ -> { f with next = forward_rec_add_polls f.next }

let fundecl (i : Mach.fundecl) : Mach.fundecl =
  let f = i.fun_body in
  let rec_handlers = find_rec_handlers f in
  { i with fun_body = add_polls_to_exit [] rec_handlers f }
