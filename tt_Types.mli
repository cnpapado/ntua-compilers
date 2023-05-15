type typ = TYPE_none        (* no type (should not be used)       *)
         | TYPE_int         (* int                                *)
         | TYPE_double      (* double                             *)
         | TYPE_char        (* char                               *)
         | TYPE_bool        (* bool                               *)
         | TYPE_array of    (* array                              *)
             typ *          (* element type                       *)
             int            (* size of array, if known, or zero   *)
         | TYPE_proc        (* proc (return type)                 *)

val sizeOfType : typ -> int
val equalType : typ -> typ -> bool
