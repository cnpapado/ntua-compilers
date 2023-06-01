type typ = TYPE_none        (* no type (should not be used)       *)
         | TYPE_int         (* int                                *)
         | TYPE_double      (* double                             *)
         | TYPE_char        (* char                               *)
         | TYPE_bool        (* bool                               *)
         | TYPE_array of {ttype: typ; size: int}
         | TYPE_ptr of {ttype: typ; level: int}
         | TYPE_void        (* artificial type of void functions *) 

val sizeOfType : typ -> int
val equalType : typ -> typ -> bool
