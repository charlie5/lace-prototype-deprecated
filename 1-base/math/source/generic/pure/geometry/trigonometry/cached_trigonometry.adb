with
     ada.Numerics.generic_elementary_Functions;

package body cached_Trigonometry
is
   Sin_Cache       : array (0 .. slot_Count - 1) of Float_Type;
   Cos_Cache       : array (0 .. slot_Count - 1) of Float_Type;

   Pi_x_2          : constant            := ada.Numerics.Pi * 2.0;
   last_slot_Index : constant Float_Type := Float_Type (slot_Count - 1);
   index_Factor    : constant Float_Type := last_slot_Index / Pi_x_2;



   function Cos (Angle : in Float_Type) return Float_Type
   is
      Index : standard.Integer := standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if Index < 0 then
         Index := Index + slot_Count;
      end if;

      return Cos_Cache (Index);
   end Cos;



   function Sin (Angle : in Float_Type) return Float_Type
   is
      Index : standard.Integer := standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if Index < 0 then
         Index := Index + slot_Count;
      end if;

      return Sin_Cache (Index);
   end Sin;



   procedure get (Angle : in Float_Type;   the_Cos : out Float_Type;
                                           the_Sin : out Float_Type)
   is
      Index : standard.Integer := standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if Index < 0 then
         Index := Index + slot_Count;
      end if;

      the_Sin := Sin_Cache (Index);
      the_Cos := Cos_Cache (Index);
   end get;




   -- TODO: Tan, arcCos, etc


   package Functions is new Ada.Numerics.generic_elementary_Functions (Float_Type);

begin
   for Each in cos_Cache'Range
   loop
      cos_Cache (Each) := Functions.cos (  Float_Type (Each) / Float_Type (slot_Count - 1)
                                         * Pi_x_2);
   end loop;


   for Each in sin_Cache'Range
   loop
      sin_Cache (Each) := Functions.sin (  Float_Type (Each) / Float_Type (slot_Count - 1)
                                         * Pi_x_2);
   end loop;
end cached_Trigonometry;
