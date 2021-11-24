with
     ada.Strings.Hash,
     ada.unchecked_Conversion;

package body openGL
is
   ------------
   --  Profiles
   --

   function Profile return profile_Kind
     is separate;


   -----------
   --  Vectors
   --

   function Scaled (Self : in Vector_3;   By : in Vector_3) return Vector_3
   is
   begin
      return (Self (1) * By (1),
              Self (2) * By (2),
              Self (3) * By (3));
   end Scaled;



   function Scaled (Self : in Vector_3_array;   By : in Vector_3) return Vector_3_array
   is
      Result : Vector_3_array (Self'Range);
   begin
      for i in Result'Range
      loop
         Result (i) := Scaled (Self (i),  By);
      end loop;

      return Result;
   end Scaled;



   function to_Vector_3_array (Self : Vector_2_array) return Vector_3_array
   is
      the_Array : Vector_3_array (1 .. Self'Length);
   begin
      for i in Self'Range
      loop
         the_Array (Index_t (i)) := Vector_3 (Self (i) & 0.0);
      end loop;

      return the_Array;
   end to_Vector_3_array;


   ----------
   --  Colors
   --

   function to_color_Value (Self : in Primary) return color_Value
   is
      Value : constant Real := Real'Rounding (Real (Self) * 255.0);
   begin
      return color_Value (Value);
   end to_color_Value;



   function to_Primary (Self : in color_Value) return Primary
   is
   begin
      return Primary (Real (Self) / 255.0);
   end to_Primary;



   function to_Color (Red, Green, Blue : in Primary) return rgb_Color
   is
   begin
      return (to_color_Value (Red),
              to_color_Value (Green),
              to_color_Value (Blue));
   end to_Color;



   function to_lucid_Color (From : in rgba_Color) return lucid_Color
   is
   begin
      return (Primary => (to_Primary (From.Primary.Red),
                          to_Primary (From.Primary.Green),
                          to_Primary (From.Primary.Blue)),
              Opacity => Opaqueness (to_Primary (From.Alpha)));
   end to_lucid_Color;



   function to_rgba_Color (From : in lucid_Color) return rgba_Color
   is
   begin
      return (Primary => (to_color_Value (From.Primary.Red),
                          to_color_Value (From.Primary.Green),
                          to_color_Value (From.Primary.Blue)),
              Alpha   =>  to_color_Value (Primary (From.Opacity)));
   end to_rgba_Color;



   function to_Color (From : in rgb_Color) return Color
   is
   begin
      return (to_Primary (From.Red),
              to_Primary (From.Green),
              to_Primary (From.Blue));
   end to_Color;



   function to_rgb_Color (From : in Color) return rgb_Color
   is
   begin
      return (to_color_Value (From.Red),
              to_color_Value (From.Green),
              to_color_Value (From.Blue));
   end to_rgb_Color;




   -------------
   --  Heightmap
   --

   function Scaled (Self : in height_Map;   By : in Real) return height_Map
   is
   begin
      return Result : height_Map := Self
      do
         scale (Result, By);
      end return;
   end scaled;



   procedure scale (Self : in out height_Map;   By : in Real)
   is
   begin
      for Row in Self'Range (1)
      loop
         for Col in Self'Range (1)
         loop
            Self (Row, Col) := Self (Row, Col) * By;
         end loop;
      end loop;
   end scale;



   function height_Extent (Self : in height_Map) return Vector_2
   is
      Min : Real := Real'Last;
      Max : Real := Real'First;
   begin
      for Row in Self'Range (1)
      loop
         for Col in Self'Range (2)
         loop
            Min := Real'Min (Min,  Self (Row, Col));
            Max := Real'Max (Max,  Self (Row, Col));
         end loop;
      end loop;

      return (Min, Max);
   end height_Extent;



   function Region (Self : in height_Map;   Rows, Cols : in index_Pair) return height_Map
   is
      Width      : constant Index_t := Index_t (Rows (2) - Rows (1));
      Height     : constant Index_t := Index_t (Cols (2) - Cols (1));

      the_Region : openGL.height_Map (1 .. Width  + 1,
                                      1 .. Height + 1);
   begin
      for Row in the_Region'Range (1)
      loop
         for Col in the_Region'Range (2)
         loop
            the_Region (Row, Col) := Self (Row + Rows (1) - 1,
                                           Col + Cols (1) - 1);
         end loop;
      end loop;

      return the_Region;
   end Region;


   ----------
   --  Assets
   --

   function to_Asset (Self : in String) return asset_Name
   is
      the_Name : String (asset_Name'Range);
   begin
      the_Name (1               .. Self'Length)   := Self;
      the_Name (Self'Length + 1 .. the_Name'Last) := (others => ' ');

      return asset_Name (the_Name);
   end to_Asset;



   function to_String (Self : in asset_Name) return String
   is
   begin
      for Each in reverse Self'Range
      loop
         if Self (Each) /= ' '
         then
            return String (Self (1 .. Each));
         end if;
      end loop;

      return "";
   end to_String;



   function Hash (Self : in asset_Name) return ada.Containers.Hash_type
   is
   begin
      return ada.Strings.Hash (to_String (Self));
   end Hash;


   ---------
   -- Bounds
   --

   function bounding_Box_of (Self : Sites) return Bounds
   is
      Result : Bounds := null_Bounds;
   begin
      for Each in Self'Range
      loop
         Result.Box.Lower (1) := Real'Min  (Result.Box.Lower (1),  Self (Each)(1));
         Result.Box.Lower (2) := Real'Min  (Result.Box.Lower (2),  Self (Each)(2));
         Result.Box.Lower (3) := Real'Min  (Result.Box.Lower (3),  Self (Each)(3));

         Result.Box.Upper (1) := Real'Max  (Result.Box.Upper (1),  Self (Each)(1));
         Result.Box.Upper (2) := Real'Max  (Result.Box.Upper (2),  Self (Each)(2));
         Result.Box.Upper (3) := Real'Max  (Result.Box.Upper (3),  Self (Each)(3));

         Result.Ball := Real'Max (Result.Ball,
                                  abs Self (Each));
      end loop;

      return Result;
   end bounding_Box_of;



   procedure set_Ball_from_Box (Self : in out Bounds)
   is
   begin
      Self.Ball := Real'Max (abs Self.Box.Lower,
                             abs Self.Box.Upper);
   end set_Ball_from_Box;


   ---------
   -- Images
   --

   function to_Image (From : in lucid_Image) return Image
   is
      the_Image : Image (From'Range (1),
                         From'Range (2));
   begin
      for Row in From'Range (1)
      loop
         for Col in From'Range (2)
         loop
            the_Image (Row, Col) := From (Row, Col).Primary;
         end loop;
      end loop;

      return the_Image;
   end to_Image;


   ------------
   -- safe_Real
   --

   protected
   body safe_Real
   is
      procedure Value_is (Now : in Real)
      is
      begin
         the_Value := Now;
      end Value_is;

      function Value return Real
      is
      begin
         return the_Value;
      end Value;
   end safe_Real;

end openGL;
