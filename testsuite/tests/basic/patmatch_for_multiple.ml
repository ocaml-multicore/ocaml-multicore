(* TEST
   flags = "-drawlambda -dlambda"
   * expect
*)

(* Note: the tests below contain *both* the -drawlambda and
   the -dlambda intermediate representations:
   -drawlambda is the Lambda code generated directly by the
     pattern-matching compiler; it contain "alias" bindings or static
     exits that are unused, and will be removed by simplification, or
     that are used only once, and will be inlined by simplification.
   -dlambda is the Lambda code resulting from simplification.

  The -drawlambda output more closely matches what the
  pattern-compiler produces, and the -dlambda output more closely
  matches the final generated code.

  In this test we decided to show both to notice that some allocations
  are "optimized away" during simplification (see "here flattening is
  an optimization" below).
*)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let (*match*/96 = 3 *match*/97 = 2 *match*/98 = 1)
  (catch
    (catch
      (catch (if (!= *match*/97 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/96 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/96 = 3 *match*/97 = 2 *match*/98 = 1)
  (catch (if (!= *match*/97 3) (if (!= *match*/96 1) 0 (exit 1)) (exit 1))
   with (1) 1))
- : bool = false
|}];;

(* This tests needs to allocate the tuple to bind 'x',
   but this is only done in the branches that use it. *)
match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let (*match*/101 = 3 *match*/102 = 2 *match*/103 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/102 3) (exit 6)
          (let (x/105 =a (makeblock 0 *match*/101 *match*/102 *match*/103))
            (exit 4 x/105)))
       with (6)
        (if (!= *match*/101 1) (exit 5)
          (let (x/104 =a (makeblock 0 *match*/101 *match*/102 *match*/103))
            (exit 4 x/104))))
     with (5) 0)
   with (4 x/99) (seq (ignore x/99) 1)))
(let (*match*/101 = 3 *match*/102 = 2 *match*/103 = 1)
  (catch
    (if (!= *match*/102 3)
      (if (!= *match*/101 1) 0
        (exit 4 (makeblock 0 *match*/101 *match*/102 *match*/103)))
      (exit 4 (makeblock 0 *match*/101 *match*/102 *match*/103)))
   with (4 x/99) (seq (ignore x/99) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/106[int] b/107 : int 0)
(function a/106[int] b/107 : int 0)
- : bool -> 'a -> unit = <fun>
|}];;

(* More complete tests.

   The test cases below compare the compiler output on alias patterns
   that are outside an or-pattern (handled during half-simplification,
   then flattened) or inside an or-pattern (handled during simplification).

   We used to have a Cannot_flatten exception that would result in fairly
   different code generated in both cases, but now the compilation strategy
   is fairly similar.
*)
let _ = fun a b -> match a, b with
| (true, _) as p -> p
| (false, _) as p -> p
(* outside, trivial *)
[%%expect {|
(function a/110[int] b/111 (let (p/112 =a (makeblock 0 a/110 b/111)) p/112))
(function a/110[int] b/111 (makeblock 0 a/110 b/111))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/114[int] b/115 (let (p/116 =a (makeblock 0 a/114 b/115)) p/116))
(function a/114[int] b/115 (makeblock 0 a/114 b/115))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/120[int] b/121
  (let (x/122 =a[int] a/120 p/123 =a (makeblock 0 a/120 b/121))
    (makeblock 0 (int,*) x/122 p/123)))
(function a/120[int] b/121
  (makeblock 0 (int,*) a/120 (makeblock 0 a/120 b/121)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/126[int] b/127
  (let (x/128 =a[int] a/126 p/129 =a (makeblock 0 a/126 b/127))
    (makeblock 0 (int,*) x/128 p/129)))
(function a/126[int] b/127
  (makeblock 0 (int,*) a/126 (makeblock 0 a/126 b/127)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/136[int] b/137[int]
  (if a/136
    (let (x/138 =a[int] a/136 p/139 =a (makeblock 0 a/136 b/137))
      (makeblock 0 (int,*) x/138 p/139))
    (let (x/140 =a b/137 p/141 =a (makeblock 0 a/136 b/137))
      (makeblock 0 (int,*) x/140 p/141))))
(function a/136[int] b/137[int]
  (if a/136 (makeblock 0 (int,*) a/136 (makeblock 0 a/136 b/137))
    (makeblock 0 (int,*) b/137 (makeblock 0 a/136 b/137))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/142[int] b/143[int]
  (catch
    (if a/142
      (let (x/150 =a[int] a/142 p/151 =a (makeblock 0 a/142 b/143))
        (exit 10 x/150 p/151))
      (let (x/148 =a b/143 p/149 =a (makeblock 0 a/142 b/143))
        (exit 10 x/148 p/149)))
   with (10 x/144[int] p/145) (makeblock 0 (int,*) x/144 p/145)))
(function a/142[int] b/143[int]
  (catch
    (if a/142 (exit 10 a/142 (makeblock 0 a/142 b/143))
      (exit 10 b/143 (makeblock 0 a/142 b/143)))
   with (10 x/144[int] p/145) (makeblock 0 (int,*) x/144 p/145)))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

(* here flattening is an optimisation: the allocation is moved as an
   alias within each branch, and in the first branch it is unused and
   will be removed by simplification, so the final code
   (see the -dlambda output) will not allocate in the first branch. *)
let _ = fun a b -> match a, b with
| (true as x, _) as _p -> x, (true, true)
| (false as x, _) as p -> x, p
(* outside, onecase *)
[%%expect {|
(function a/152[int] b/153[int]
  (if a/152
    (let (x/154 =a[int] a/152 _p/155 =a (makeblock 0 a/152 b/153))
      (makeblock 0 (int,*) x/154 [0: 1 1]))
    (let (x/156 =a[int] a/152 p/157 =a (makeblock 0 a/152 b/153))
      (makeblock 0 (int,*) x/156 p/157))))
(function a/152[int] b/153[int]
  (if a/152 (makeblock 0 (int,*) a/152 [0: 1 1])
    (makeblock 0 (int,*) a/152 (makeblock 0 a/152 b/153))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/158[int] b/159
  (let (x/160 =a[int] a/158 p/161 =a (makeblock 0 a/158 b/159))
    (makeblock 0 (int,*) x/160 p/161)))
(function a/158[int] b/159
  (makeblock 0 (int,*) a/158 (makeblock 0 a/158 b/159)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
[%%expect{|
0
0
type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
|}]

(* another example where we avoid an allocation in the first case *)
let _ =fun a b -> match a, b with
| (true, Cons p) -> p
| (_, _) as p -> p
(* outside, tuplist *)
[%%expect {|
(function a/171[int] b/172
  (catch
    (if a/171 (if b/172 (let (p/173 =a (field_imm 0 b/172)) p/173) (exit 12))
      (exit 12))
   with (12) (let (p/174 =a (makeblock 0 a/171 b/172)) p/174)))
(function a/171[int] b/172
  (catch (if a/171 (if b/172 (field_imm 0 b/172) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/171 b/172)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/175[int] b/176
  (catch
    (catch
      (if a/175
        (if b/176 (let (p/180 =a (field_imm 0 b/176)) (exit 13 p/180))
          (exit 14))
        (exit 14))
     with (14) (let (p/179 =a (makeblock 0 a/175 b/176)) (exit 13 p/179)))
   with (13 p/177) p/177))
(function a/175[int] b/176
  (catch
    (catch
      (if a/175 (if b/176 (exit 13 (field_imm 0 b/176)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/175 b/176)))
   with (13 p/177) p/177))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
