type typ = TYPE_none        
         | TYPE_int         
         | TYPE_double      
         | TYPE_char        
         | TYPE_bool
         | TYPE_pointer        
         | TYPE_array of    
             typ *          
             int            
         | TYPE_proc        

let rec sizeOfType t =
   match t with
   | TYPE_int            -> 2
   | TYPE_double         -> 10
   | TYPE_char           -> 1
   | TYPE_bool           -> 1
   | TYPE_pointer        -> 2
   | TYPE_array (et, sz) -> sz * sizeOfType et
   | _                   -> 0

let rec equalType t1 t2 =
   match t1, t2 with
   | TYPE_array (et1, sz1), TYPE_array (et2, sz2) -> equalType et1 et2
   | _                                            -> t1 = t2
