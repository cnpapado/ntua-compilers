type typ = TYPE_none        (* no type (should not be used)       *)
         | TYPE_int         (* int                                *)
         | TYPE_double      (* double                             *)
         | TYPE_char        (* char                               *)
         | TYPE_bool        (* bool                               *)
         | TYPE_array of {ttype: typ; size: int}
         | TYPE_ptr of {ttype: typ; level: int}
         | TYPE_void        (* artificial type of void functions *)

let rec sizeOfType t =
  match t with
  | TYPE_int                           -> 2
  | TYPE_double                        -> 10
  | TYPE_char                          -> 1
  | TYPE_bool                          -> 1
  | TYPE_array {ttype = et; size = sz} -> sz * sizeOfType et
  | TYPE_ptr {ttype= _ ; level = l}    -> l * 2
  | _                                  -> 0

let rec equalType t1 t2 =
   match t1, t2 with
   | TYPE_array {ttype = et1;size = _}, TYPE_array {ttype = et2; size = _} -> equalType et1 et2
   | TYPE_ptr {ttype= et1 ; level = _}, TYPE_ptr {ttype= et2 ; level = _}  -> equalType et1 et2 
   | _                                                                     -> t1 = t2
