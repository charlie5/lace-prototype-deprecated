package Impact.D3
--
--  Provides 3D physics simulations.
--
is
   pragma Pure;


   type Vector_3_array is array (Positive range <>) of aliased math.Vector_3;
   type Flags          is mod 2**64;


   --  C-ish types
   --
   --  (These may be removed during completion of Ada port).

   type     Real_array       is array (Natural  range <>) of aliased math.Real;     -- NB: the 'aliased'.

   subtype  c_Vector_3       is Real_array (0 .. 2);
   type     c_Vector_3_array is array (Positive range <>) of aliased c_Vector_3;


   function to_C    (From : in math.Vector_3) return    c_Vector_3;
   function to_Math (From : in    c_Vector_3) return math.Vector_3;

end Impact.D3;
