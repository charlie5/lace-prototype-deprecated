with
     openGL.Primitive.indexed,
     openGL.Geometry.lit_colored_textured,
     openGL.Model.hexagon,

     float_math.Algebra.linear.d3;


package body openGL.Model.Hexagon_Column.lit_colored_textured_faceted
is

   type Geometry_view is access all openGL.Geometry.lit_colored_textured.item'class;


   ---------
   --- Forge
   --

   function new_hexagon_Column (Radius : in Real;
                                Height : in Real;
                                Upper,
                                Lower  : in lit_colored_textured_faceted.hex_Face;
                                Shaft  : in shaft_Face) return View
   is
      Self : constant View := new Item;
   begin
      Self.Radius     := Radius;
      Self.Height     := Height;
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


      function new_hexagon_Face (Vertices : access openGL.geometry.lit_colored_textured.Vertex_array;
                                 Flip     : in     Boolean := False) return Geometry_view
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

         the_Primitive : constant Primitive.indexed.view
           := Primitive.indexed.new_Primitive (triangle_Fan,
                                               the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices.all);
         the_Geometry.add          (openGL.Primitive.view (the_Primitive));

         return the_Geometry;
      end new_hexagon_Face;


      function new_shaft_Face (Vertices : access openGL.geometry.lit_colored_textured.Vertex_array)
                               return Geometry_view
      is
         use openGL.Primitive;

         the_Indices   : constant Indices := (1, 2, 3, 4);

         the_Geometry  : constant Geometry_view
           := openGL.Geometry.lit_colored_textured.new_Geometry (texture_is_Alpha => False).all'Access;

         the_Primitive : constant Primitive.view
           := Primitive.indexed.new_Primitive (triangle_Strip,  the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices.all);
         the_Geometry.add          (the_Primitive);

         return the_Geometry;
      end new_shaft_Face;


      upper_Face  : Geometry_view;
      lower_Face  : Geometry_view;

      shaft_Faces : array (1 .. 6) of Geometry_view;

   begin
      for Each in mid_Sites'Range
      loop
         upper_Sites (Each) := upper_Sites (Each) + height_Offset;
         lower_Sites (Each) := lower_Sites (Each) - height_Offset;
      end loop;


      --  Upper
      --
      declare
         the_Vertices : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1 => (site => height_Offset,     normal => Normal,   color => self.upper_Face.center_Color,   coords => (0.0, 0.0)),
               2 => (site => upper_Sites (1),   normal => Normal,   color => self.upper_Face.Colors (1),     coords => (0.0, 0.0)),
               3 => (site => upper_Sites (2),   normal => Normal,   color => self.upper_Face.Colors (2),     coords => (1.0, 0.0)),
               4 => (site => upper_Sites (3),   normal => Normal,   color => self.upper_Face.Colors (3),     coords => (1.0, 1.0)),
               5 => (site => upper_Sites (4),   normal => Normal,   color => self.upper_Face.Colors (4),     coords => (0.0, 1.0)),
               6 => (site => upper_Sites (5),   normal => Normal,   color => self.upper_Face.Colors (5),     coords => (0.0, 1.0)),
               7 => (site => upper_Sites (6),   normal => Normal,   color => self.upper_Face.Colors (6),     coords => (0.0, 1.0)));
      begin
         upper_Face := new_hexagon_Face (vertices => the_Vertices'Access);

         if Self.upper_Face.Texture /= null_Asset
         then
            upper_Face.Texture_is (Textures.fetch (Self.upper_Face.Texture));
         end if;
      end;


      --  Lower
      --
      declare
         the_Vertices : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1 => (site => -height_Offset,    normal => -Normal,   color => self.upper_Face.center_Color,   coords => (0.0, 0.0)),
               2 => (site =>  lower_Sites (1),  normal => -Normal,   color => self.upper_Face.Colors (1),     coords => (0.0, 0.0)),
               3 => (site =>  lower_Sites (2),  normal => -Normal,   color => self.upper_Face.Colors (2),     coords => (1.0, 0.0)),
               4 => (site =>  lower_Sites (3),  normal => -Normal,   color => self.upper_Face.Colors (3),     coords => (1.0, 1.0)),
               5 => (site =>  lower_Sites (4),  normal => -Normal,   color => self.upper_Face.Colors (4),     coords => (0.0, 1.0)),
               6 => (site =>  lower_Sites (5),  normal => -Normal,   color => self.upper_Face.Colors (5),     coords => (0.0, 1.0)),
               7 => (site =>  lower_Sites (6),  normal => -Normal,   color => self.upper_Face.Colors (6),     coords => (0.0, 1.0)));
      begin
         lower_Face := new_hexagon_Face (vertices => the_Vertices'Access,
                                        flip      => True);

         if Self.upper_Face.Texture /= null_Asset
         then
            lower_Face.Texture_is (Textures.fetch (Self.lower_Face.Texture));
         end if;
      end;


      --  Shaft
      --
      declare
         type shaft_Normals is array (1 .. 6) of openGL.Vector_3;


         function get_Normals return shaft_Normals
         is
            use math.Algebra.linear.d3;

            Rotation   : constant math.Matrix_3x3 := y_Rotation_from (math.to_Radians (60.0));
            the_Normal :          math.Vector_3   := (0.0, 0.0, -1.0);
            Result     :          shaft_Normals;
         begin
            Result (2) := the_Normal;

            the_Normal := Rotation * the_Normal;
            Result (3) := the_Normal;

            the_Normal := Rotation * the_Normal;
            Result (4) := the_Normal;

            the_Normal := (0.0, 0.0, 1.0);
            Result (5) := the_Normal;

            the_Normal := Rotation * the_Normal;
            Result (6) := the_Normal;

            the_Normal := Rotation * the_Normal;
            Result (1) := the_Normal;

            return Result;
         end get_Normals;


         Normals        : constant shaft_Normals := get_Normals;

         s_Delta        : constant := 1.0 / 6.0;

         the_Vertices_1 : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1  => (site => upper_Sites (1),   normal => Normals (1),   color => self.shaft_Face.Color,   coords => (0.0,           1.0)),
               2  => (site => lower_Sites (1),   normal => Normals (1),   color => self.shaft_Face.Color,   coords => (0.0,           0.0)),
               3  => (site => upper_Sites (2),   normal => Normals (1),   color => self.shaft_Face.Color,   coords => (s_Delta,       1.0)),
               4  => (site => lower_Sites (2),   normal => Normals (1),   color => self.shaft_Face.Color,   coords => (s_Delta,       0.0)));

         the_Vertices_2 : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1  => (site => upper_Sites (2),   normal => Normals (2),   color => self.shaft_Face.Color,   coords => (s_Delta,       1.0)),
               2  => (site => lower_Sites (2),   normal => Normals (2),   color => self.shaft_Face.Color,   coords => (s_Delta,       0.0)),
               3  => (site => upper_Sites (3),   normal => Normals (2),   color => self.shaft_Face.Color,   coords => (s_Delta * 2.0, 1.0)),
               4  => (site => lower_Sites (3),   normal => Normals (2),   color => self.shaft_Face.Color,   coords => (s_Delta * 2.0, 0.0)));

         the_Vertices_3 : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1  => (site => upper_Sites (3),   normal => Normals (3),   color => self.shaft_Face.Color,   coords => (s_Delta * 2.0, 1.0)),
               2  => (site => lower_Sites (3),   normal => Normals (3),   color => self.shaft_Face.Color,   coords => (s_Delta * 2.0, 0.0)),
               3  => (site => upper_Sites (4),   normal => Normals (3),   color => self.shaft_Face.Color,   coords => (s_Delta * 3.0, 1.0)),
               4  => (site => lower_Sites (4),   normal => Normals (3),   color => self.shaft_Face.Color,   coords => (s_Delta * 3.0, 0.0)));

         the_Vertices_4 : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1  => (site => upper_Sites (4),   normal => Normals (4),   color => self.shaft_Face.Color,   coords => (s_Delta * 3.0, 1.0)),
               2  => (site => lower_Sites (4),   normal => Normals (4),   color => self.shaft_Face.Color,   coords => (s_Delta * 3.0, 0.0)),
               3  => (site => upper_Sites (5),   normal => Normals (4),   color => self.shaft_Face.Color,   coords => (s_Delta * 4.0, 1.0)),
               4  => (site => lower_Sites (5),   normal => Normals (4),   color => self.shaft_Face.Color,   coords => (s_Delta * 4.0, 0.0)));

         the_Vertices_5 : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1  => (site => upper_Sites (5),   normal => Normals (5),   color => self.shaft_Face.Color,   coords => (s_Delta * 4.0, 1.0)),
               2  => (site => lower_Sites (5),   normal => Normals (5),   color => self.shaft_Face.Color,   coords => (s_Delta * 4.0, 0.0)),
               3  => (site => upper_Sites (6),   normal => Normals (5),   color => self.shaft_Face.Color,   coords => (s_Delta * 5.0, 1.0)),
               4  => (site => lower_Sites (6),   normal => Normals (5),   color => self.shaft_Face.Color,   coords => (s_Delta * 5.0, 0.0)));

         the_Vertices_6 : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1  => (site => upper_Sites (6),   normal => Normals (6),   color => self.shaft_Face.Color,   coords => (s_Delta * 5.0, 1.0)),
               2  => (site => lower_Sites (6),   normal => Normals (6),   color => self.shaft_Face.Color,   coords => (s_Delta * 5.0, 0.0)),
               3  => (site => upper_Sites (1),   normal => Normals (6),   color => self.shaft_Face.Color,   coords => (1.0,           1.0)),
               4  => (site => lower_Sites (1),   normal => Normals (6),   color => self.shaft_Face.Color,   coords => (1.0,           0.0)));

         the_Vertices  : constant array (1 .. 6) of access openGL.geometry.lit_colored_textured.Vertex_array
           := (the_Vertices_1'Access,
               the_Vertices_2'Access,
               the_Vertices_3'Access,
               the_Vertices_4'Access,
               the_Vertices_5'Access,
               the_Vertices_6'Access);

      begin
         for Each in shaft_Faces'Range
         loop
            shaft_Faces (Each) := new_shaft_Face (vertices => the_Vertices (Each));

            if Self.shaft_Face.Texture /= null_Asset
            then
               shaft_Faces (Each).Texture_is (Textures.fetch (Self.shaft_Face.Texture));
            end if;
         end loop;
      end;

      return (1 => upper_Face     .all'Access,
              2 => lower_Face     .all'Access,
              3 => shaft_Faces (1).all'Access,
              4 => shaft_Faces (2).all'Access,
              5 => shaft_Faces (3).all'Access,
              6 => shaft_Faces (4).all'Access,
              7 => shaft_Faces (5).all'Access,
              8 => shaft_Faces (6).all'Access);

   end to_GL_Geometries;


end openGL.Model.Hexagon_Column.lit_colored_textured_faceted;
