with
     ada.Containers.Vectors,
     interfaces.C.Pointers;

package impact.d3.Containers
--
--  Provides various common containers.
--
is

   --  Integer
   --
   package integer_Vectors     is new ada.Containers.Vectors (Positive, Integer);
   subtype integer_Vector      is     integer_Vectors.Vector;
   type    integer_Vector_view is access all integer_Vector;


   --  C.Unsigned
   --
   type    Unsigneds           is array (Positive range <>) of aliased Interfaces.C.Unsigned;

   use type Interfaces.C.Unsigned;
   package Unsigned_Vectors    is new ada.Containers.Vectors (Positive, Interfaces.C.Unsigned);
   subtype Unsigned_Vector     is Unsigned_Vectors.Vector;


   --  Interfaces.Unsigned_32;
   --
   use type Interfaces.Unsigned_32;
   package unsigned_32_Vectors is new ada.Containers.Vectors (Positive, Interfaces.Unsigned_32);
   subtype unsigned_32_Vector  is     unsigned_32_Vectors.Vector;


   --  Real
   --
   package real_Vectors     is new ada.Containers.Vectors (Positive, math.Real);
   subtype real_Vector      is     real_Vectors.Vector;
   type    real_Vector_view is access all real_Vector;


   --  Real Pointers
   --
   package real_Pointers is new interfaces.C.Pointers (Natural,  math.Real,  Real_array,  math.Real'First);
   subtype real_Pointer  is Real_Pointers.Pointer;


   --  Vector_3
   --
   use type math.Vector_3;
   package vector_3_Vectors is new ada.Containers.Vectors (Positive, math.Vector_3);
   subtype vector_3_Vector  is     vector_3_Vectors.Vector;


   --  Any Views
   --
   type Any_view is access all Any'Class;


end impact.d3.Containers;
