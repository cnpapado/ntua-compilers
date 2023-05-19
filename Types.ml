type typ = TYPE_none
| TYPE_int
| TYPE_bool
| TYPE_char
| TYPE_double
| TYPE_array of {ttype: typ; size: int}
| TYPE_ptr of {ttype: typ; level: int}

let rec sizeOfType t =
   match t with
   | TYPE_int            -> 2
   | TYPE_bool           -> 1
   | TYPE_array {ttype=et; size=sz} -> sz * sizeOfType et
   | _                   -> 0

let rec equalType t1 t2 =
   match t1, t2 with
   | TYPE_array {ttype=et1; size=sz1}, TYPE_array {ttype=et2; size=sz2} -> equalType et1 et2
   | _                                            -> t1 = t2

let id2str x = "ji";