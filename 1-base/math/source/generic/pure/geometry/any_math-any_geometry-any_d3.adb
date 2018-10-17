package body any_math.any_Geometry.any_d3
is

   --------
   -- Plane
   --

   procedure normalise (the_Plane : in out Plane)
   is
      use Functions;
      inv_Magnitude : constant Real := 1.0 / Sqrt (  the_Plane (1) * the_Plane (1)
                                                   + the_Plane (2) * the_Plane (2)
                                                   + the_Plane (3) * the_Plane (3));
   begin
      the_Plane (1) := the_Plane (1) * inv_Magnitude;
      the_Plane (2) := the_Plane (2) * inv_Magnitude;
      the_Plane (3) := the_Plane (3) * inv_Magnitude;
      the_Plane (4) := the_Plane (4) * inv_Magnitude;
   end normalise;



   function Image (the_Model : in a_Model) return String
   is
   begin
      return
          "(num_Sites =>" & Integer'Image (the_Model.site_Count) & ","
        & " num_Tris =>"  & Integer'Image (the_Model. tri_Count) & ")";

   exception
      when others =>
         return "<tbd>";
   end Image;



   ----------
   --  Bounds
   --

   function to_bounding_Box (Self : Sites) return bounding_Box
   is
      the_Bounds : bounding_Box := null_Bounds;
   begin
      for Each in Self'Range
      loop
         the_Bounds.Lower (1) := Real'Min  (the_Bounds.Lower (1),  Self (Each)(1));
         the_Bounds.Lower (2) := Real'Min  (the_Bounds.Lower (2),  Self (Each)(2));
         the_Bounds.Lower (3) := Real'Min  (the_Bounds.Lower (3),  Self (Each)(3));

         the_Bounds.Upper (1) := Real'Max  (the_Bounds.Upper (1),  Self (Each)(1));
         the_Bounds.Upper (2) := Real'Max  (the_Bounds.Upper (2),  Self (Each)(2));
         the_Bounds.Upper (3) := Real'Max  (the_Bounds.Upper (3),  Self (Each)(3));
      end loop;

      return the_Bounds;
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


end any_math.any_Geometry.any_d3;
