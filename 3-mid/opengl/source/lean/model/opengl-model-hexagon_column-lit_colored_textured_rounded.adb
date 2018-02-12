with
     openGL.Primitive.indexed,
     openGL.Geometry.lit_colored_textured,
     openGL.Model.hexagon,

     float_math.Algebra.linear.d3;


package body openGL.Model.hexagon_Column.lit_colored_textured_rounded
is

   type Geometry_view is access all openGL.Geometry.lit_colored_textured.item'Class;


   ---------
   --- Forge
   --

   function new_hexagon_Column (Radius : in Real;
                                Height : in Real;
                                Upper,
                                Lower  : in lit_colored_textured_rounded.hex_Face;
                                Shaft  : in shaft_Face) return View
   is
      Self : constant View := new Item;
   begin
      Self.Radius := Radius;
      Self.Height := Height;

      Self.upper_Face := Upper;
      Self.lower_Face := Lower;
      Self.shaft_Face := Shaft;

      return Self;
   end new_hexagon_Column;



   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Fonts);

      use openGL.Geometry,
          openGL.Geometry.lit_colored_textured,
          openGL.Model.hexagon,
          openGL.Texture,
          math.Geometry;

      shaft_Height  : constant openGL.Real     := Self.Height;
      height_Offset : constant openGL.Vector_3 := (0.0,  shaft_Height / 2.0,  0.0);

      mid_Sites     : constant hexagon.Sites   := vertex_Sites (Self.Radius);
      upper_Sites   :          hexagon.Sites   := mid_Sites;
      lower_Sites   :          hexagon.Sites   := mid_Sites;


      function new_hexagon_Face (Vertices : in openGL.geometry.lit_colored_textured.Vertex_array;
                                 Flip     : in Boolean := False) return Geometry_view
      is
         use openGL.Primitive;

         function the_Indices return Indices
         is
         begin
            if Flip
            then   return (1, 7, 6, 5, 4, 3, 2, 7);
            else   return (1, 2, 3, 4, 5, 6, 7, 2);
            end if;
         end the_Indices;

         the_Geometry  : constant Geometry_view
           := openGL.Geometry.lit_colored_textured.new_Geometry (texture_is_Alpha => False).all'Access;

         the_Primitive : constant Primitive.view
           := Primitive.indexed.new_Primitive (triangle_Fan,
                                               the_Indices).all'Access;

      begin
         the_Geometry.Vertices_are (Vertices);
         the_Geometry.add          (the_Primitive);

         return the_Geometry;
      end new_hexagon_Face;



      function new_shaft_Face (Vertices : in openGL.geometry.lit_colored_textured.Vertex_array)
                               return Geometry_view
      is
         use openGL.Primitive;

         the_Indices   : constant Indices                := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2);
         the_Geometry  : constant Geometry_view          := openGL.Geometry.lit_colored_textured.new_Geometry
                                                                                            (texture_is_Alpha => False).all'Access;
         the_Primitive : constant Primitive.indexed.view := Primitive.indexed.new_Primitive (triangle_Strip,
                                                                                             the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices);
         the_Geometry.add          (Primitive.view (the_Primitive));

         return the_Geometry;
      end new_shaft_Face;


      upper_Face  : Geometry_view;
      lower_Face  : Geometry_view;
      shaft_Face  : Geometry_view;

   begin
      for Each in mid_Sites'Range
      loop
         upper_Sites (Each) := upper_Sites (Each) + height_Offset;
         lower_Sites (Each) := lower_Sites (Each) - height_Offset;
      end loop;


      --  Upper
      --
      declare
         the_Vertices : constant openGL.Geometry.lit_colored_textured.Vertex_array
           := (1 => (site => height_Offset,    normal => Normal,  color => Self.upper_Face.center_Color,  coords => (0.0, 0.0)),
               2 => (site => upper_Sites (1),  normal => Normal,  color => Self.upper_Face.Colors (1),    coords => (0.0, 0.0)),
               3 => (site => upper_Sites (2),  normal => Normal,  color => Self.upper_Face.Colors (2),    coords => (1.0, 0.0)),
               4 => (site => upper_Sites (3),  normal => Normal,  color => Self.upper_Face.Colors (3),    coords => (1.0, 1.0)),
               5 => (site => upper_Sites (4),  normal => Normal,  color => Self.upper_Face.Colors (4),    coords => (0.0, 1.0)),
               6 => (site => upper_Sites (5),  normal => Normal,  color => Self.upper_Face.Colors (5),    coords => (0.0, 1.0)),
               7 => (site => upper_Sites (6),  normal => Normal,  color => Self.upper_Face.Colors (6),    coords => (0.0, 1.0)));
      begin
         upper_Face := new_hexagon_Face (vertices => the_Vertices);

         if Self.upper_Face.Texture /= null_Asset
         then
            upper_Face.Texture_is (Textures.fetch (Self.upper_Face.Texture));
         end if;
      end;


      --  Lower
      --
      declare
         the_Vertices : constant openGL.Geometry.lit_colored_textured.Vertex_array
           := (1 => (site => -height_Offset,     normal => -Normal,   color => Self.lower_Face.center_Color,   coords => (0.0, 0.0)),
               2 => (site =>  lower_Sites (1),   normal => -Normal,   color => Self.lower_Face.Colors (1),     coords => (0.0, 0.0)),
               3 => (site =>  lower_Sites (2),   normal => -Normal,   color => Self.lower_Face.Colors (2),     coords => (1.0, 0.0)),
               4 => (site =>  lower_Sites (3),   normal => -Normal,   color => Self.lower_Face.Colors (3),     coords => (1.0, 1.0)),
               5 => (site =>  lower_Sites (4),   normal => -Normal,   color => Self.lower_Face.Colors (4),     coords => (0.0, 1.0)),
               6 => (site =>  lower_Sites (5),   normal => -Normal,   color => Self.lower_Face.Colors (5),     coords => (0.0, 1.0)),
               7 => (site =>  lower_Sites (6),   normal => -Normal,   color => Self.lower_Face.Colors (6),     coords => (0.0, 1.0)));
      begin
         lower_Face := new_hexagon_Face (vertices => the_Vertices,
                                         flip     => True);

         if Self.lower_Face.Texture /= null_Asset
         then
            lower_Face.Texture_is (Textures.fetch (Self.lower_Face.Texture));
         end if;
      end;


      --- Shaft
      --
      declare
         type shaft_Normals is array (1 .. 6) of openGL.Vector_3;


         function get_Normals return shaft_Normals
         is
            use math.Algebra.linear.d3;

            Rotation   : constant math.Matrix_3x3 := y_Rotation_from (-math.to_Radians (60.0));
            the_Normal :          math.Vector_3   := (1.0, 0.0, 0.0);

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

         the_Vertices : constant openGL.Geometry.lit_colored_textured.Vertex_array
           := ( 1 => (site => upper_Sites (1),   normal => Normals (1),   color => Self.shaft_Face.Color,   coords => (0.0, 1.0)),
                2 => (site => lower_Sites (1),   normal => Normals (1),   color => Self.shaft_Face.Color,   coords => (0.0, 0.0)),
                3 => (site => upper_Sites (2),   normal => Normals (2),   color => Self.shaft_Face.Color,   coords => (0.2, 1.0)),
                4 => (site => lower_Sites (2),   normal => Normals (2),   color => Self.shaft_Face.Color,   coords => (0.2, 0.0)),
                5 => (site => upper_Sites (3),   normal => Normals (3),   color => Self.shaft_Face.Color,   coords => (0.4, 1.0)),
                6 => (site => lower_Sites (3),   normal => Normals (3),   color => Self.shaft_Face.Color,   coords => (0.4, 0.0)),
                7 => (site => upper_Sites (4),   normal => Normals (4),   color => Self.shaft_Face.Color,   coords => (0.6, 1.0)),
                8 => (site => lower_Sites (4),   normal => Normals (4),   color => Self.shaft_Face.Color,   coords => (0.6, 0.0)),
                9 => (site => upper_Sites (5),   normal => Normals (5),   color => Self.shaft_Face.Color,   coords => (0.8, 1.0)),
               10 => (site => lower_Sites (5),   normal => Normals (5),   color => Self.shaft_Face.Color,   coords => (0.8, 0.0)),
               11 => (site => upper_Sites (6),   normal => Normals (6),   color => Self.shaft_Face.Color,   coords => (1.0, 1.0)),
               12 => (site => lower_Sites (6),   normal => Normals (6),   color => Self.shaft_Face.Color,   coords => (1.0, 0.0)));
      begin
         shaft_Face := new_shaft_Face (vertices => the_Vertices);

         if Self.shaft_Face.Texture /= null_Asset
         then
            shaft_Face.Texture_is (Textures.fetch (Self.shaft_Face.Texture));
         end if;
      end;

      return (1 => upper_Face.all'Access,
              2 => lower_Face.all'Access,
              3 => shaft_Face.all'Access);
   end to_GL_Geometries;


end openGL.Model.hexagon_Column.lit_colored_textured_rounded;
