package body cached_Rotation
is
   use ada.Numerics,
       float_elementary_Functions;


   the_Cache       : array (0 .. slot_Count - 1) of aliased Matrix_2x2_type;

   Pi_x_2          : constant            := Pi * 2.0;
   last_slot_Index : constant Float_type := Float_type (slot_Count - 1);
   index_Factor    : constant Float_type := last_slot_Index / Pi_x_2;



   function to_Rotation (Angle : in Float_type) return access constant Matrix_2x2_type
   is
      the_Index : standard.Integer := standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if the_Index < 0
      then
         the_index := the_Index + slot_Count;
      end if;

      return the_Cache (the_Index)'Access;
   end to_Rotation;



begin
   for Each in the_Cache'Range
   loop
      declare
         Angle : constant Float_type := (  Float_type (Each) / Float_type (slot_Count - 1)
                                         * Pi_x_2);

         C : constant Float_type := Cos (Angle);
         S : constant Float_type := Sin (Angle);
      begin
         the_Cache (Each) := to_Matrix_2x2 (C, -S,
                                            S,  C);
      end;
   end loop;
end cached_Rotation;
