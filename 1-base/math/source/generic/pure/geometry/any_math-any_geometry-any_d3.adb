package body any_Math.any_Geometry.any_d3
is

   --------
   -- Plane
   --

   procedure normalise (the_Plane : in out Plane)
   is
      use Functions;
      inverse_Magnitude : constant Real := 1.0 / SqRt (  the_Plane (1) * the_Plane (1)
                                                       + the_Plane (2) * the_Plane (2)
                                                       + the_Plane (3) * the_Plane (3));
   begin
      the_Plane (1) := the_Plane (1) * inverse_Magnitude;
      the_Plane (2) := the_Plane (2) * inverse_Magnitude;
      the_Plane (3) := the_Plane (3) * inverse_Magnitude;
      the_Plane (4) := the_Plane (4) * inverse_Magnitude;
   end normalise;



   function Image (the_Model : in a_Model) return String
   is
   begin
      return
          "(Site_Count =>" & Integer'Image (the_Model.Site_Count) & ","
        & " Tri_Count =>"  & Integer'Image (the_Model. Tri_Count) & ")";

   exception
      when others =>
         return "<TODO>";
   end Image;



   ----------
   --  Bounds
   --

   function to_bounding_Box (Self : Sites) return bounding_Box
   is
      Bounds : bounding_Box := null_Bounds;
   begin
      for Each in Self'Range
      loop
         Bounds.Lower (1) := Real'Min  (Bounds.Lower (1),  Self (Each)(1));
         Bounds.Lower (2) := Real'Min  (Bounds.Lower (2),  Self (Each)(2));
         Bounds.Lower (3) := Real'Min  (Bounds.Lower (3),  Self (Each)(3));

         Bounds.Upper (1) := Real'Max  (Bounds.Upper (1),  Self (Each)(1));
         Bounds.Upper (2) := Real'Max  (Bounds.Upper (2),  Self (Each)(2));
         Bounds.Upper (3) := Real'Max  (Bounds.Upper (3),  Self (Each)(3));
      end loop;

      return Bounds;
   end to_bounding_Box;



   function Extent (Self : in bounding_Box;   Dimension : in Index) return Real
   is
   begin
      return Self.Upper (Dimension) - Self.Lower (Dimension);
   end Extent;



   function "or" (Left : in bounding_Box;   Right : in Site) return bounding_Box
   is
      Result : bounding_Box;
   begin
      for i in Right'Range
      loop
         if Right (i) < Left.Lower (i)
         then   Result.Lower (i) := Right (i);
         else   Result.Lower (i) := Left.Lower (i);
         end if;

         if Right (i) > Left.Upper (i)
         then   Result.Upper (i) := Right (i);
         else   Result.Upper (i) := Left.Upper (i);
         end if;
      end loop;

      return Result;
   end "or";



   function "or" (Left  : in bounding_Box;
                  Right : in bounding_Box) return bounding_Box
   is
      Result : bounding_Box := Left or Right.Lower;
   begin
      Result := Result or Right.Upper;
      return Result;
   end "or";



   function "+" (Left : in bounding_Box;   Right : in Vector_3) return bounding_Box
   is
   begin
      return (Left.Lower + Right,
              Left.Upper + Right);
   end "+";



   function Image (Self : bounding_Box) return String
   is
   begin
      return    "(lower => " & Image (Self.Lower)
             & ", upper => " & Image (Self.Upper) & ")";
   end Image;


end any_Math.any_Geometry.any_d3;
