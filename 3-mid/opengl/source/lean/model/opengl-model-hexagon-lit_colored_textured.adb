with
     openGL.Geometry.lit_colored_textured,
     openGL.Primitive.indexed;


package body openGL.Model.Hexagon.lit_colored_textured
is

   type Geometry_view is access all openGL.Geometry.lit_colored_textured.item'class;


   ---------
   --- Forge
   --

   function new_Hexagon (Radius : in Real;
                         Face   : in lit_colored_textured.Face) return View
   is
      Self : constant View := new Item;
   begin
      Self.Radius := Radius;
      Self.Face   := Face;

      return Self;
   end new_Hexagon;


   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Textures, Fonts);

      use openGL.Geometry.lit_colored_textured,
          openGL.Texture;

      the_Sites    :         constant hexagon.Sites := vertex_Sites (Self.Radius);
      the_Indices  : aliased constant Indices       := (1, 2, 3, 4, 5, 6, 7, 2);


      function new_Face (Vertices : in openGL.geometry.lit_colored_textured.Vertex_array) return Geometry_view
      is
         use openGL.Primitive;

         the_Geometry  : constant Geometry_view
           := openGL.Geometry.lit_colored_textured.new_Geometry (texture_is_Alpha => False).all'unchecked_Access;

         the_Primitive : constant Primitive.indexed.view
           := Primitive.indexed.new_Primitive (triangle_Fan,  the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices);
         the_Geometry.add          (Primitive.view (the_Primitive));

         return the_Geometry;
      end new_Face;


      upper_Face : Geometry_view;

   begin
      --  Upper Face
      --
      declare
         the_Vertices : constant openGL.Geometry.lit_colored_textured.Vertex_array
           := (1 => (site => (0.0, 0.0, 0.0), normal => Normal, color => self.Face.center_Color, coords => (0.0, 0.0)),
               2 => (site =>   the_Sites (1), normal => Normal, color => self.Face.Colors (1),   coords => (0.0, 0.0)),
               3 => (site =>   the_Sites (2), normal => Normal, color => self.Face.Colors (2),   coords => (1.0, 0.0)),
               4 => (site =>   the_Sites (3), normal => Normal, color => self.Face.Colors (3),   coords => (1.0, 1.0)),
               5 => (site =>   the_Sites (4), normal => Normal, color => self.Face.Colors (4),   coords => (0.0, 1.0)),
               6 => (site =>   the_Sites (5), normal => Normal, color => self.Face.Colors (5),   coords => (0.0, 1.0)),
               7 => (site =>   the_Sites (6), normal => Normal, color => self.Face.Colors (6),   coords => (0.0, 1.0)));
      begin
         upper_Face := new_Face (vertices => the_Vertices);

         if Self.Face.Texture /= null_Object
         then
            upper_Face.Texture_is (Self.Face.Texture);
         end if;
      end;

      return (1 => upper_Face.all'Access);
   end to_GL_Geometries;


end openGL.Model.Hexagon.lit_colored_textured;
