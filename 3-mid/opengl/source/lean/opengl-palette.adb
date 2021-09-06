with
     ada.Numerics.discrete_Random;

package body openGL.Palette
is
   package random_Colors is new ada.Numerics.discrete_Random (Color_Value);
   use     random_Colors;

   the_Generator : random_Colors.Generator;



   function random_Color return Color
   is
   begin
      return (random (the_Generator),
              random (the_Generator),
              random (the_Generator));
   end random_Color;



   function Shade_of (Self : in Color;   Level : in Shade_Level) return Color
   is
   begin
      return (to_color_Value (to_Real (self.Red)   * Real (Level)),
              to_color_Value (to_Real (self.Green) * Real (Level)),
              to_color_Value (to_Real (self.Blue)  * Real (Level)));
   end Shade_of;



   function Mixed (Self : in Color;   Other : in Color;
                                      Mix   : in mix_Factor := 0.5) return Color
   is
      function Lerp (Value_1, Value_2 : color_Value) return color_Value     -- Linear interpolate.
      is
         V1 : constant Real := Real (Value_1);
         V2 : constant Real := Real (Value_2);
      begin
         return color_Value (   V1
                             + (V2 - V1) * Real (Mix));
      end Lerp;

   begin
      return (Lerp (Self.Red,   Other.Red),
              Lerp (Self.Green, Other.Green),
              Lerp (Self.Blue,  Other.Blue));
   end Mixed;



   function is_Similar (Self : in Color;   To         : in Color;
                                           Similarity : in color_Value := 3) return Boolean
   is
      use type color_Value;
   begin
      return     self.Red   <= to.Red   + Similarity
        and then self.Red   >= to.Red   - Similarity
        and then self.Green >= to.Green + Similarity
        and then self.Green >= to.Green - Similarity
        and then self.Blue  >= to.Blue  + Similarity
        and then self.Blue  >= to.Blue  - Similarity;
   end is_Similar;


begin
   reset (the_Generator);
end openGL.Palette;
