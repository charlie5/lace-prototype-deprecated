with
     openGL.Primitive.indexed,
     openGL.Geometry.lit_textured,
     openGL.Model.hexagon;


package body openGL.Model.hexagon_Column.lit_textured_rounded
is
   ---------
   --- Forge
   --

   function new_hexagon_Column (Radius : in Real;
                                Height : in Real;
                                Upper,
                                Lower  : in hex_Face;
                                Shaft  : in shaft_Face) return View
   is
      Self : constant View := new Item;
   begin
      Self.Radius := Radius;
      Self.Height := Height;

      Self.upper_Face := Upper;
      Self.lower_Face := Lower;
      Self.Shaft      := Shaft;

      return Self;
   end new_hexagon_Column;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Fonts);

      use Geometry.lit_textured,
          Model.hexagon,
          Texture;

      shaft_Height  : constant Real     := Self.Height;
      height_Offset : constant Vector_3 := (0.0,  shaft_Height / 2.0,  0.0);

      mid_Sites     : constant hexagon.Sites   := vertex_Sites (Self.Radius);
      upper_Sites   :          hexagon.Sites   := mid_Sites;
      lower_Sites   :          hexagon.Sites   := mid_Sites;


      function new_hexagon_Face (Vertices : in Geometry.lit_textured.Vertex_array;
                                 Flip     : in Boolean := False) return Geometry.lit_textured.view
      is
         use Primitive;

         function the_Indices return Indices
         is
         begin
            if Flip
            then   return (1, 7, 6, 5, 4, 3, 2, 7);
            else   return (1, 2, 3, 4, 5, 6, 7, 2);
            end if;
         end the_Indices;

         the_Geometry  : constant Geometry.lit_textured.view
           := Geometry.lit_textured.new_Geometry;

         the_Primitive : constant Primitive.view
           := Primitive.indexed.new_Primitive (triangle_Fan,
                                               the_Indices).all'Access;

      begin
         the_Geometry.Vertices_are (Vertices);
         the_Geometry.add          (the_Primitive);

         return the_Geometry;
      end new_hexagon_Face;



      function new_shaft_Face (Vertices : in Geometry.lit_textured.Vertex_array)
                               return Geometry.lit_textured.view
      is
         use Primitive;

         the_Indices   : constant Indices := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2);

         the_Geometry  : constant Geometry.lit_textured.view
           := Geometry.lit_textured.new_Geometry;

         the_Primitive : constant Primitive.indexed.view
           := Primitive.indexed.new_Primitive (triangle_Strip,
                                               the_Indices);
      begin
         the_Geometry.Vertices_are (Vertices);
         the_Geometry.add          (Primitive.view (the_Primitive));

         return the_Geometry;
      end new_shaft_Face;


      upper_Face  : Geometry.lit_textured.view;
      lower_Face  : Geometry.lit_textured.view;
      shaft_Face  : Geometry.lit_textured.view;

   begin
      for i in mid_Sites'Range
      loop
         upper_Sites (i) := upper_Sites (i) + height_Offset;
         lower_Sites (i) := lower_Sites (i) - height_Offset;
      end loop;

      --  Upper
      --
      declare
         the_Vertices : constant Geometry.lit_textured.Vertex_array
           := (1 => (Site => height_Offset,    Normal => Normal,  Coords => (0.0, 0.0),   Shine => default_Shine),
               2 => (Site => upper_Sites (1),  Normal => Normal,  Coords => (0.0, 0.0),   Shine => default_Shine),
               3 => (Site => upper_Sites (2),  Normal => Normal,  Coords => (1.0, 0.0),   Shine => default_Shine),
               4 => (Site => upper_Sites (3),  Normal => Normal,  Coords => (1.0, 1.0),   Shine => default_Shine),
               5 => (Site => upper_Sites (4),  Normal => Normal,  Coords => (0.0, 1.0),   Shine => default_Shine),
               6 => (Site => upper_Sites (5),  Normal => Normal,  Coords => (0.0, 1.0),   Shine => default_Shine),
               7 => (Site => upper_Sites (6),  Normal => Normal,  Coords => (0.0, 1.0),   Shine => default_Shine));
      begin
         upper_Face := new_hexagon_Face (Vertices => the_Vertices);

         if Self.upper_Face.Texture /= null_Asset
         then
            upper_Face.Texture_is (Textures.fetch (Self.upper_Face.Texture));
         end if;
      end;

      --  Lower
      --
      declare
         the_Vertices : constant Geometry.lit_textured.Vertex_array
           := (1 => (Site => -height_Offset,     Normal => -Normal,   Coords => (0.0, 0.0),   Shine => default_Shine),
               2 => (Site =>  lower_Sites (1),   Normal => -Normal,   Coords => (0.0, 0.0),   Shine => default_Shine),
               3 => (Site =>  lower_Sites (2),   Normal => -Normal,   Coords => (1.0, 0.0),   Shine => default_Shine),
               4 => (Site =>  lower_Sites (3),   Normal => -Normal,   Coords => (1.0, 1.0),   Shine => default_Shine),
               5 => (Site =>  lower_Sites (4),   Normal => -Normal,   Coords => (0.0, 1.0),   Shine => default_Shine),
               6 => (Site =>  lower_Sites (5),   Normal => -Normal,   Coords => (0.0, 1.0),   Shine => default_Shine),
               7 => (Site =>  lower_Sites (6),   Normal => -Normal,   Coords => (0.0, 1.0),   Shine => default_Shine));
      begin
         lower_Face := new_hexagon_Face (Vertices => the_Vertices,
                                         flip     => True);

         if Self.lower_Face.Texture /= null_Asset
         then
            lower_Face.Texture_is (Textures.fetch (Self.lower_Face.Texture));
         end if;
      end;

      --- Shaft
      --
      declare
         type shaft_Normals is array (1 .. 6) of Vector_3;

         function get_Normals return shaft_Normals
         is
            use linear_Algebra_3D;

            Rotation   : constant Matrix_3x3 := y_Rotation_from (-math.to_Radians (60.0));
            the_Normal :          Vector_3   := (1.0, 0.0, 0.0);
            Result     :          shaft_Normals;
         begin
            Result (1) := the_Normal;

            the_Normal := Rotation * the_Normal;
            Result (2) := the_Normal;

            the_Normal := Rotation * the_Normal;
            Result (3) := the_Normal;

            the_Normal := (0.0, 0.0, 1.0);
            Result (4) := the_Normal;

            the_Normal := Rotation * the_Normal;
            Result (5) := the_Normal;

            the_Normal := Rotation * the_Normal;
            Result (6) := the_Normal;

            return Result;
         end get_Normals;

         Normals      : constant shaft_Normals := get_Normals;

         the_Vertices : constant Geometry.lit_textured.Vertex_array
           := ( 1 => (Site => upper_Sites (1),   Normal => Normals (1),   Coords => (0.0, 1.0),   Shine => default_Shine),
                2 => (Site => lower_Sites (1),   Normal => Normals (1),   Coords => (0.0, 0.0),   Shine => default_Shine),
                3 => (Site => upper_Sites (2),   Normal => Normals (2),   Coords => (0.2, 1.0),   Shine => default_Shine),
                4 => (Site => lower_Sites (2),   Normal => Normals (2),   Coords => (0.2, 0.0),   Shine => default_Shine),
                5 => (Site => upper_Sites (3),   Normal => Normals (3),   Coords => (0.4, 1.0),   Shine => default_Shine),
                6 => (Site => lower_Sites (3),   Normal => Normals (3),   Coords => (0.4, 0.0),   Shine => default_Shine),
                7 => (Site => upper_Sites (4),   Normal => Normals (4),   Coords => (0.6, 1.0),   Shine => default_Shine),
                8 => (Site => lower_Sites (4),   Normal => Normals (4),   Coords => (0.6, 0.0),   Shine => default_Shine),
                9 => (Site => upper_Sites (5),   Normal => Normals (5),   Coords => (0.8, 1.0),   Shine => default_Shine),
               10 => (Site => lower_Sites (5),   Normal => Normals (5),   Coords => (0.8, 0.0),   Shine => default_Shine),
               11 => (Site => upper_Sites (6),   Normal => Normals (6),   Coords => (1.0, 1.0),   Shine => default_Shine),
               12 => (Site => lower_Sites (6),   Normal => Normals (6),   Coords => (1.0, 0.0),   Shine => default_Shine));
      begin
         shaft_Face := new_shaft_Face (Vertices => the_Vertices);

         if Self.Shaft.Texture /= null_Asset
         then
            shaft_Face.Texture_is (Textures.fetch (Self.Shaft.Texture));
         end if;
      end;

      return (1 => upper_Face.all'Access,
              2 => lower_Face.all'Access,
              3 => shaft_Face.all'Access);
   end to_GL_Geometries;


end openGL.Model.hexagon_Column.lit_textured_rounded;
