with
     openGL.Primitive.indexed;

package body openGL.Model.arrow.colored
is
   ---------
   --- Forge
   --

   function to_Arrow (Color      : in openGL.Color := Palette.White;
                      line_Width : in Real         := 1.0;
                      End_1,
                      End_2      : in Vector_3     := Origin_3D) return Item
   is
      Self : Model.arrow.colored.item;
   begin
      Self.Color      := Color;
      Self.line_Width := line_Width;

      Self.Vertices (1).Site := End_1;   -- Main line.
      Self.Vertices (2).Site := End_2;   --

      Self.Vertices (3).Site := End_2;   -- Side bits.
      Self.Vertices (4).Site := End_2;   --

      Self.set_side_Bits;

      return Self;
   end to_Arrow;


   function new_Arrow (Color      : in openGL.Color := Palette.White;
                       line_Width : in Real         := 1.0;
                       End_1,
                       End_2      : in Vector_3     := Origin_3D) return View
   is
   begin
      return new Arrow.colored.item' (to_Arrow (Color, line_Width, End_1, End_2));
   end new_Arrow;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Textures, Fonts);

      use openGL.Geometry.colored;

      Color         : constant openGL.rgb_Color := +Self.Color;
      indices_Count : constant long_Index_t     := 2;
      the_Indices   : aliased  Indices          := (1 .. indices_Count => <>);
      the_Primitive :          Primitive.indexed.view;
   begin
      Geometry.free (Self.Geometry);
      Self.Geometry := Geometry.colored.new_Geometry;

      set_Colors:
      begin
         Self.Vertices (1).Color := (primary => Color,  Alpha => opaque_Value);
         Self.Vertices (2).Color := (primary => Color,  Alpha => opaque_Value);
         Self.Vertices (3).Color := (primary => Color,  Alpha => opaque_Value);
         Self.Vertices (4).Color := (primary => Color,  Alpha => opaque_Value);
      end set_Colors;

      Self.Geometry.is_Transparent (False);
      Self.Geometry.Vertices_are   (Self.Vertices);

      -- Main line.
      --
      Self.Geometry.free_Primitives;

      the_Indices   := (1, 2);
      the_Primitive := Primitive.indexed.new_Primitive (Primitive.Lines, the_Indices, line_Width => Self.line_Width);
      Self.Geometry.add (Primitive.view (the_Primitive));

      -- Left bit.
      --
      the_Indices   := (2, 3);
      the_Primitive := Primitive.indexed.new_Primitive (Primitive.Lines, the_Indices, line_Width => Self.line_Width);
      Self.Geometry.add (Primitive.view (the_Primitive));

      -- Right bit.
      --
      the_Indices   := (2, 4);
      the_Primitive := Primitive.indexed.new_Primitive (Primitive.Lines, the_Indices, line_Width => Self.line_Width);
      Self.Geometry.add (Primitive.view (the_Primitive));

      Self.set_side_Bits;

      return (1 => Self.Geometry);
   end to_GL_Geometries;



   procedure set_side_Bits (Self : in out Item)
   is
      use linear_Algebra_3d;

      End_1 : Vector_3    renames Self.Vertices (1).Site;
      End_2 : Vector_3    renames Self.Vertices (2).Site;

      polar_Coords     : constant Geometry_2d.polar_Site := Geometry_2d.to_Polar (to_Vector_2 (End_2 - End_1));

      the_Angle        : constant Radians          := polar_Coords.Angle;
      bit_Length       : constant Real             := abs (End_2 - End_1) * 0.1;

      left_bit_Offset  : constant Geometry_2d.Site := Geometry_2d.to_Site ((Angle  => the_Angle + to_Radians (135.0),
                                                                            Extent => bit_Length));
      right_bit_Offset : constant Geometry_2d.Site := Geometry_2d.to_Site ((Angle  => the_Angle + to_Radians (135.0 + 90.0),
                                                                            Extent => bit_Length));

      left_bit_End     : constant Vector_3         := End_2 + to_Vector_3 ( left_bit_Offset);
      right_bit_End    : constant Vector_3         := End_2 + to_Vector_3 (right_bit_Offset);
   begin
      Self.Vertices (3).Site :=  left_bit_End;   -- Left bit.
      Self.Vertices (4).Site := right_bit_End;   -- Right bit.
   end set_side_Bits;



   function End_Site (Self : in  Item;   for_End : in Integer) return Vector_3
   is
   begin
      return Self.Vertices (Index_t (for_End)).Site;
   end End_Site;


   procedure End_Site_is (Self : in out Item;   Now     : in Vector_3;
                                                for_End : in Integer)
   is
   begin
      Self.Vertices (Index_t (for_End)).Site := Now;
      Self.set_side_Bits;
      Self.is_Modified := True;
   end End_Site_is;



   overriding
   procedure modify (Self : in out Item)
   is
   begin
      Self.Geometry.Vertices_are (Self.Vertices);
      Self.set_Bounds;
      Self.is_Modified := False;
   end modify;


   overriding
   function is_Modified (Self : in Item) return Boolean
   is
   begin
      return Self.is_Modified;
   end is_Modified;


end openGL.Model.arrow.colored;
