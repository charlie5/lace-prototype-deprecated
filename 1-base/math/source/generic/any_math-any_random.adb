with
     ada.Numerics.Float_random,
     ada.Numerics.Discrete_random;

package body any_Math.any_Random
is
   use ada.Numerics;

   package Boolean_random is new ada.numerics.discrete_Random (Boolean);

   real_Generator    : Float_random  .Generator;
   boolean_Generator : Boolean_random.Generator;



   function random_Boolean return Boolean
   is
   begin
      return Boolean_random.Random (boolean_Generator);
   end random_Boolean;



   function random_Real (Lower : in Real := Real'First;
                         Upper : in Real := Real'Last) return Real
   is
      base_Roll : constant Float := Float_random.Random (Real_Generator);
   begin
      return   Lower
             + Real (base_Roll) * (Upper - Lower);
   end random_Real;



   function random_Integer (Lower : in Integer := Integer'First;
                            Upper : in Integer := Integer'Last) return Integer
   is
      Modulus   : constant Positive := Upper - Lower + 1;
      base_Roll : constant Float    := Float_random.Random (Real_Generator);
   begin
      return   Lower
             + Integer (Float (Modulus) * base_Roll) mod Modulus;
   end random_Integer;



begin
   Boolean_random.reset (boolean_Generator);
   Float_random  .reset (   real_Generator);
end any_math.any_Random;
