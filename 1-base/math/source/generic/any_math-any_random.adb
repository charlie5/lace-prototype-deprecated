with
     ada.Numerics.Float_random;


package body any_Math.any_Random
is
   use ada.Numerics.Float_random;

   the_Generator : ada.numerics.Float_random.Generator;


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
      base_Roll := ada.numerics.Float_random.Random (the_Generator);
      the_Roll  := Real (base_Roll) * (Upper - Lower) + Lower;

      return the_Roll;
   end random_Real;


   function random_Integer (Lower : in Integer := Integer'First;
                            Upper : in Integer := Integer'Last) return Integer
   is
   begin
      return Integer (Real'Adjacent (random_Real (0.0, 1.0) * (Real (Upper - Lower + 1))  -  0.5,
                                     0.0))
             + Lower;
   end random_Integer;


begin
   reset (the_Generator);
end any_math.any_Random;
