val fundecl : Mach.fundecl -> Mach.fundecl
val allocates_unconditionally : Mach.instruction -> bool
val is_leaf_func_without_loops : Mach.instruction -> bool