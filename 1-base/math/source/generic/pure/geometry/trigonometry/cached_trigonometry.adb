with
     ada.Numerics.generic_elementary_Functions;



package body cached_Trigonometry
is
   sin_Cache       : array (0 .. slot_Count - 1) of Float_Type;
   cos_Cache       : array (0 .. slot_Count - 1) of Float_Type;

   Pi_x_2          : constant            := 2.0 * ada.numerics.Pi;
   last_slot_Index : constant Float_Type := Float_Type (slot_Count - 1);
   index_Factor    : constant Float_Type := last_slot_Index / Pi_x_2;



   function cos (Angle : in Float_Type) return Float_Type
   is
      the_Index : Standard.Integer := Standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if the_Index < 0 then
         the_index := the_Index + slot_Count;
      end if;

      return cos_Cache (the_Index);
   end cos;



   function sin (Angle : in Float_Type) return Float_Type
   is
      the_Index : Standard.Integer := Standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if the_Index < 0 then
         the_index := the_Index + slot_Count;
      end if;

      return sin_Cache (the_Index);
   end sin;



   procedure get (Angle : in Float_Type;   the_Cos : out Float_Type;
                                           the_Sin : out Float_Type)
   is
      the_Index : Standard.Integer := Standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if the_Index < 0 then
         the_index := the_Index + slot_Count;
      end if;

      the_Sin := sin_Cache (the_Index);
      the_Cos := cos_Cache (the_Index);
   end get;




   -- tbd: tan, etc

   package float_elementary_Functions is new Ada.Numerics.generic_elementary_Functions (Float_Type);

begin
   for Each in cos_Cache'Range
   loop
      cos_Cache (Each) := float_elementary_Functions.cos (  Float_Type (Each) / Float_Type (slot_Count - 1)
                                                          * Pi_x_2);
   end loop;


   for Each in sin_Cache'Range
   loop
      sin_Cache (Each) := float_elementary_Functions.sin (  Float_Type (Each) / Float_Type (slot_Count - 1)
                                                          * Pi_x_2);
   end loop;
end cached_Trigonometry;
