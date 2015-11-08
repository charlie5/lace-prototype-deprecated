with
     ada.Numerics.discrete_Random;


procedure lace.Containers.shuffle_Vector (the_Vector : in out Vectors.Vector)
is
   use type Vectors.Index_Type;

begin
   for i in reverse 2 .. Vectors.Index_Type (the_Vector.Length)    -- Start from 2, since swapping the first element with itself is useless.
   loop
      declare
         subtype Index is Vectors.Index_Type range Vectors.Index_Type'First
                                                .. Vectors.Index_Type'First + i - 1;

         package random_Index is new ada.Numerics.discrete_Random (Index);
         use     random_Index;

         the_Generator : random_Index.Generator;
      begin
         the_Vector.swap (Random (the_Generator),
                          Index'Last);
      end;
   end loop;
end lace.Containers.shuffle_Vector;
