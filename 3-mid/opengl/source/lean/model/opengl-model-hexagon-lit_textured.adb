with
     openGL.Geometry.lit_textured,
     openGL.Primitive.indexed;


package body openGL.Model.hexagon.lit_textured
is
   ---------
   --- Forge
   --

   function new_Hexagon (Radius : in Real;
                         Face   : in lit_textured.Face) return View
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
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Textures, Fonts);

      use Geometry.lit_textured,
          Texture;

      the_Sites    :         constant hexagon.Sites := vertex_Sites (Self.Radius);
      the_Indices  : aliased constant Indices       := (1, 2, 3, 4, 5, 6, 7, 2);


      function new_Face (Vertices : in geometry.lit_textured.Vertex_array) return Geometry.lit_textured.view
      is
         use Primitive;

         the_Geometry  : constant Geometry.lit_textured.view
           := Geometry.lit_textured.new_Geometry;

         the_Primitive : constant Primitive.indexed.view
           := Primitive.indexed.new_Primitive (triangle_Fan, the_Indices);
      begin
         the_Geometry.Vertices_are (Vertices);
         the_Geometry.add          (Primitive.view (the_Primitive));

         return the_Geometry;
      end new_Face;


      upper_Face : Geometry.lit_textured.view;

   begin
      --  Upper Face
      --
      declare
         the_Vertices : constant Geometry.lit_textured.Vertex_array
           := (1 => (Site => (0.0, 0.0, 0.0),  Normal => Normal,  Coords => (0.0, 0.0),  Shine => default_Shine),
               2 => (Site =>   the_Sites (1),  Normal => Normal,  Coords => (0.0, 0.0),  Shine => default_Shine),
               3 => (Site =>   the_Sites (2),  Normal => Normal,  Coords => (1.0, 0.0),  Shine => default_Shine),
               4 => (Site =>   the_Sites (3),  Normal => Normal,  Coords => (1.0, 1.0),  Shine => default_Shine),
               5 => (Site =>   the_Sites (4),  Normal => Normal,  Coords => (0.0, 1.0),  Shine => default_Shine),
               6 => (Site =>   the_Sites (5),  Normal => Normal,  Coords => (0.0, 1.0),  Shine => default_Shine),
               7 => (Site =>   the_Sites (6),  Normal => Normal,  Coords => (0.0, 1.0),  Shine => default_Shine));
      begin
         upper_Face := new_Face (Vertices => the_Vertices);

         if Self.Face.Texture /= null_Object
         then
            upper_Face.Texture_is (Self.Face.Texture);
         end if;
      end;

      return (1 => upper_Face.all'Access);
   end to_GL_Geometries;


end openGL.Model.hexagon.lit_textured;
