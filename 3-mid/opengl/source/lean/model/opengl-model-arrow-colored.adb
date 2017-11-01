with
     openGL.Primitive.indexed;
with Ada.Text_IO; use Ada.Text_IO;


package body openGL.Model.arrow.colored
is

   ---------
   --- Forge
   --
   package body Forge
   is
      function to_Arrow (Color      : in openGL.Color  := Palette.White;
                         line_Width : in openGL.Real   := 1.0;
                         End_1,
                         End_2      : in math.Vector_3 := Origin_3d) return Item
      is
         use openGL.Geometry.colored;
         Self : Model.arrow.colored.item;

      begin
         Self.Color      := Color;
         Self.line_Width := line_Width;

         Self.Vertices (1).Site := End_1;   -- Main line.
         Self.Vertices (2).Site := End_2;   --

         Self.Vertices (3).Site := End_2;   -- Side bits.
         Self.Vertices (4).Site := End_2;   --

--           Self.set_Bounds;
         Self.set_side_Bits;

         return Self;
      end to_Arrow;


      function new_Arrow (Color      : in openGL.Color  := Palette.White;
                          line_Width : in openGL.Real   := 1.0;
                          End_1,
                          End_2      : in math.Vector_3 := Origin_3d) return View
      is
      begin
         return new Arrow.colored.item' (to_Arrow (Color, line_Width, End_1, End_2));
      end new_Arrow;
   end Forge;



   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Textures, Fonts);

      use openGL.Geometry.colored;

      indices_Count : constant openGL.long_Index_t          := 2;
      the_Indices   : aliased  Indices                      := (1 .. indices_Count => <>);
      the_Primitive :          openGL.Primitive.indexed.view;

   begin
      declare
         the_Geometry : openGL.Geometry.view := openGL.Geometry.view (Self.Geometry);
      begin
         openGL.Geometry.free (the_Geometry);
         Self.Geometry := openGL.Geometry.colored.new_Geometry;
      end;


      set_Colors :
      declare
         use type math.Real;
      begin
         Self.Vertices (1).Color := (primary => Self.Color,  opacity => openGL.Opaque);
         Self.Vertices (2).Color := (primary => Self.Color,  opacity => openGL.Opaque);
         Self.Vertices (3).Color := (primary => Self.Color,  opacity => openGL.Opaque);
         Self.Vertices (4).Color := (primary => Self.Color,  opacity => openGL.Opaque);
      end set_Colors;


      Self.Geometry.is_Transparent (False);
      Vertices_are (Self.Geometry.all, Self.Vertices);

      -- Main line.
      --
      Self.Geometry.free_Primitives;

      the_Indices   := (1, 2);
      the_Primitive := openGL.Primitive.indexed.new_Primitive (openGL.primitive.Lines,  the_Indices, line_Width => Self.line_Width);
      Self.Geometry.add (openGL.Primitive.view (the_Primitive));

      -- Left bit.
      --
      the_Indices   := (2, 3);
      the_Primitive := openGL.Primitive.indexed.new_Primitive (openGL.primitive.Lines,  the_Indices, line_Width => Self.line_Width);
      Self.Geometry.add (openGL.Primitive.view (the_Primitive));

      -- Right bit.
      --
      the_Indices   := (2, 4);
      the_Primitive := openGL.Primitive.indexed.new_Primitive (openGL.primitive.Lines,  the_Indices, line_Width => Self.line_Width);
      Self.Geometry.add (openGL.Primitive.view (the_Primitive));

      Self.set_side_Bits;

      return (1 => Self.Geometry.all'Access);
   end to_GL_Geometries;



--     procedure set_Bounds (Self : in out Item)
--     is
--        use math.Geometry;
--     begin
--        Self.Bounds      := null_Bounds;
--
--        Self.Bounds.Box  := Self.Bounds.Box or Vector_3' (Self.Vertices (1).Site);
--        Self.Bounds.Box  := Self.Bounds.Box or Vector_3' (Self.Vertices (2).Site);
--
--        Self.Bounds.Ball := Real'Max (abs (Self.Vertices (1).Site),
--                                      abs (Self.Vertices (2).Site));
--     end set_Bounds;



   procedure set_side_Bits (Self : in out Item)
   is
      use math.Geometry;
      use linear_Algebra_3d;

      End_1           :          Vector_3    renames Self.Vertices (1).Site;
      End_2           :          Vector_3    renames Self.Vertices (2).Site;

      polar_Coords    : constant Geometry_2d.polar_Site := Geometry_2d.to_Polar (to_Vector_2 (End_2 - End_1));

      the_Angle       : constant Radians          := polar_Coords.Angle;
      bit_Length      : constant Real             := abs (End_2 - End_1) * 0.1;

      left_bit_Offst  : constant Geometry_2d.Site := Geometry_2d.to_Site ((angle  => the_Angle + to_Radians (135.0),
                                                                           extent => bit_Length));
      right_bit_Offst : constant Geometry_2d.Site := Geometry_2d.to_Site ((angle  => the_Angle + to_Radians (135.0 + 90.0),
                                                                           extent => bit_Length));

      left_bit_End    : constant Vector_3         := End_2 + to_Vector_3 (left_bit_Offst);
      right_bit_End   : constant Vector_3         := End_2 + to_Vector_3 (right_bit_Offst);

   begin
      Self.Vertices (3).Site :=  left_bit_End;   -- Left bit.
      Self.Vertices (4).Site := right_bit_End;   -- Right bit.
   end set_side_Bits;



   function Site (Self : in  Item;   for_End : in Integer) return math.Vector_3
   is
      use openGL.Geometry.colored;
   begin
      return Self.Vertices (openGL.Index_t (for_End)).Site;
   end Site;


   procedure Site_is (Self : in out Item;   Now     : in math.Vector_3;
                                            for_End : in Integer)
   is
      use openGL.Geometry.colored;
   begin
      Self.Vertices (openGL.Index_t (for_End)).Site := Now;
      Self.set_side_Bits;
--        Self.set_Bounds;

      Self.is_Modified := True;
   end Site_is;



--     overriding
--     function  Bounds (Self : in Item) return openGL.Bounds
--     is
--     begin
--        return Self.Geometry.Bounds;
--  --        return Self.Bounds;
--     end Bounds;



   overriding
   procedure modify (Self : in out Item)
   is
   begin
      Self.Geometry.Vertices_are (Self.Vertices);
      Self.set_Bounds;
      Self.is_Modified := False;
   end modify;


   overriding
   function  is_Modified (Self : in     Item) return Boolean
   is
   begin
      return Self.is_Modified;
   end is_Modified;


end openGL.Model.arrow.colored;
