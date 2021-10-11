(* TEST
   flags = "-drawlambda"
   * expect
*)

(* Successful flattening *)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let
  (*match*/93 = 3
   *match*/94 = 2
   *match*/95 = 1
   *match*/96 = *match*/93
   *match*/97 = *match*/94
   *match*/98 = *match*/95)
  (catch
    (catch
      (catch (if (!= *match*/97 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/96 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
- : bool = false
|}];;

(* Failed flattening: we need to allocate the tuple to bind x. *)

match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let
  (*match*/101 = 3
   *match*/102 = 2
   *match*/103 = 1
   *match*/104 = (makeblock 0 *match*/101 *match*/102 *match*/103))
  (catch
    (catch
      (let (*match*/105 =a (field_imm 0 *match*/104))
        (catch
          (let (*match*/106 =a (field_imm 1 *match*/104))
            (if (!= *match*/106 3) (exit 7)
              (let (*match*/107 =a (field_imm 2 *match*/104))
                (exit 5 *match*/104))))
         with (7)
          (if (!= *match*/105 1) (exit 6)
            (let
              (*match*/109 =a (field_imm 2 *match*/104)
               *match*/108 =a (field_imm 1 *match*/104))
              (exit 5 *match*/104)))))
     with (6) 0)
   with (5 x/99) (seq (ignore x/99) 1)))
- : bool = false
|}];;
