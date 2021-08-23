with
     ada.Numerics.Float_random,
     ada.Numerics.Discrete_random;

package body any_Math.any_Random
is
   use ada.Numerics;

   package Integer_random is new Discrete_random (Integer);

   Integer_Generator : Integer_random.Generator;
   Real_Generator    :   Float_random.Generator;


   function random_Boolean return Boolean
   is
   begin
      return Boolean'Val (random_Integer (0, 1));
   end random_Boolean;


   function random_Real (Lower : in Real := Real'First;
                         Upper : in Real := Real'Last) return Real
   is
      base_Roll : Float;
      the_Roll  : Real;
   begin
      base_Roll := Float_random.Random (Real_Generator);
      the_Roll  := Real (base_Roll) * (Upper - Lower) + Lower;

      return the_Roll;
   end random_Real;


   function random_Integer (Lower : in Integer := Integer'First;
                            Upper : in Integer := Integer'Last) return Integer
   is
   begin
      return Integer_random.Random (Integer_Generator, Lower, Upper);
   end random_Integer;


begin
   Integer_random.reset (Integer_Generator);
   Float_random  .reset (   Real_Generator);
end any_math.any_Random;
