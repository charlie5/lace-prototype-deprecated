with
     ada.Numerics.Generic_Elementary_Functions;


generic
   type Float_Type is digits <>;
   with package float_elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float_Type);

   slot_Count : Standard.Positive;

package cached_Trigonometry
--
-- Caches trig functions for speed at the cost of precision.
--
is
   pragma Optimize (Time);


   function cos (Angle : in Float_Type) return Float_Type;
   function sin (Angle : in Float_Type) return Float_Type;


   procedure get (Angle : in Float_Type;   the_Cos : out Float_Type;
                                           the_Sin : out Float_Type);

   -- tbd: tan, arccos, etc


private

   pragma Inline_Always (cos);
   pragma Inline_Always (sin);
   pragma Inline_Always (get);

end cached_Trigonometry;
