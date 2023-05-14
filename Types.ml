type typ = TYPE_none        (* no type (should not be used)       *)
         | TYPE_int         (* int                                *)
         | TYPE_double      (* double                             *)
         | TYPE_char        (* char                               *)
         | TYPE_bool        (* bool                               *)
         | TYPE_array of    (* array                              *)
             typ *          (* element type                       *)
             int            (* size of array, if known, or zero   *)
         | TYPE_proc        (* proc (return type)                 *)

let rec sizeOfType t =
   match t with
   | TYPE_int            -> 2
   | TYPE_bool           -> 1
   | TYPE_array (et, sz) -> sz * sizeOfType et
   | _                   -> 0

let rec equalType t1 t2 =
   match t1, t2 with
   | TYPE_array (et1, sz1), TYPE_array (et2, sz2) -> equalType et1 et2
   | _                                            -> t1 = t2
