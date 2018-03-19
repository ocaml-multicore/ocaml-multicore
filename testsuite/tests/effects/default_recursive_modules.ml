module rec Even : sig
   val is_even : int -> bool
end = struct
   effect IsEven : int -> bool with function
                                    | IsEven 0 -> true
                                    | IsEven n -> Odd.is_odd (n-1)

   let is_even n = perform (IsEven n)
end
and Odd : sig
   val is_odd : int -> bool
end = struct
   effect IsOdd : int -> bool with function
                                   | IsOdd 0 -> false
                                   | IsOdd n -> Even.is_even (n-1)
   let is_odd n = perform (IsOdd n)
end

let _ =
  let ps = List.map Even.is_even [0;1;2;3;4;5;6;7;8;9;10] in
  let ps = List.map (function
                     | true -> "true"
                     | false -> "false")
                    ps
  in
  List.map print_endline ps
