with
     ada.Numerics.discrete_Random;


procedure lace.Containers.shuffle_Vector (the_Vector : in out vectors.Vector)
is
   use type vectors.Index_type;
begin
   for i in reverse 2 .. vectors.Index_type (the_Vector.Length)    -- Start from 2, since swapping the
   loop                                                            -- first element with itself is useless.
      declare
         subtype Index is vectors.Index_type range vectors.Index_type'First
                                                .. vectors.Index_type'First + i - 1;

         package random_Index is new ada.Numerics.discrete_Random (Index);
         use     random_Index;

         the_Generator : random_Index.Generator;
      begin
         the_Vector.swap (Random (the_Generator),
                          Index'Last);
      end;
   end loop;
end lace.Containers.shuffle_Vector;
