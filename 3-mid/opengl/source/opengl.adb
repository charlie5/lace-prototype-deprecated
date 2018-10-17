with
     Ada.Strings.Hash,
     Ada.unchecked_Conversion;


package body openGL
is
--   use type Real;


   ------------
   --  Profiles
   --

   function Profile return profile_Kind
     is separate;



   --------
   -- Reals
   --

   --  'almost_Zero'
   --

   --  From the Ada 95 Quality and Style Guide, 7.2.7:
   --
   --  Tests for
   --
   --  (1) absolute "equality" to 0 in storage,
   --  (2) absolute "equality" to 0 in computation,
   --  (3) relative "equality" to 0 in storage, and
   --  (4) relative "equality" to 0 in computation:
   --
   --    abs X <= Float_Type'Model_Small                      -- (1)
   --    abs X <= Float_Type'Base'Model_Small                 -- (2)
   --    abs X <= abs X * Float_Type'Model_Epsilon            -- (3)
   --    abs X <= abs X * Float_Type'Base'Model_Epsilon       -- (4)
   --
   function almost_Zero (X : Real) return Boolean
   is
   begin
      return  abs X  <=  Real'Base'Model_Small;
   end Almost_zero;



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
      for Each in Result'Range
      loop
         Result (Each) := Scaled (Self (Each),  By);
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

   function to_color_Value (Self : in Real) return color_Value
   is
   begin
      if Self = 1.0
      then
         return 255;
      else
         return color_Value (Self * 256.0);
      end if;
   end to_color_Value;



   function to_Real (Self : in color_Value) return Real
   is
   begin
      return Real (Self) / 255.0;
   end to_Real;



   function to_Color (R, G, B : in Real) return Color
   is
      pragma Assert (R >= 0.0 and R <= 1.0);
      pragma Assert (G >= 0.0 and G <= 1.0);
      pragma Assert (B >= 0.0 and B <= 1.0);
   begin
      return (to_color_Value (R),
              to_color_Value (G),
              to_color_Value (B));
   end to_Color;



   -------------
   --  Heightmap
   --

   function scaled (Self : in height_Map;   By : in Real) return height_Map
   is
   begin
      return the_height_Map : height_Map := Self
      do
         scale (the_height_Map, by => By);
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
--      use type Index_t;

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



   function Hash (Self : in asset_Name) return ada.Containers.Hash_Type
   is
   begin
      return ada.Strings.Hash (to_String (Self));
   end Hash;



   ---------
   -- Bounds
   --

   function bounding_Box_of (Self : Sites) return Bounds
   is
      the_Bounds : Bounds := null_Bounds;
   begin
      for Each in Self'Range
      loop
         the_Bounds.Box.Lower (1) := Real'Min  (the_Bounds.Box.Lower (1),  Self (Each)(1));
         the_Bounds.Box.Lower (2) := Real'Min  (the_Bounds.Box.Lower (2),  Self (Each)(2));
         the_Bounds.Box.Lower (3) := Real'Min  (the_Bounds.Box.Lower (3),  Self (Each)(3));

         the_Bounds.Box.Upper (1) := Real'Max  (the_Bounds.Box.Upper (1),  Self (Each)(1));
         the_Bounds.Box.Upper (2) := Real'Max  (the_Bounds.Box.Upper (2),  Self (Each)(2));
         the_Bounds.Box.Upper (3) := Real'Max  (the_Bounds.Box.Upper (3),  Self (Each)(3));

         the_Bounds.Ball          := Real'Max  (the_Bounds.Ball,           abs (Self (Each)));
      end loop;

      return the_Bounds;
   end bounding_Box_of;



   procedure set_Ball_from_Box (Self : in out Bounds)
   is
   begin
      Self.Ball := Real'Max (abs (Self.Box.Lower),
                             abs (Self.Box.Upper));
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
