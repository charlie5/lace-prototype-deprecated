with
     openGL.Geometry.lit_colored_textured,
     openGL.Primitive.indexed;


package body openGL.Model.box.lit_colored_textured
is
   type Geometry_view is access all openGL.Geometry.lit_colored_textured.item'class;


   ---------
   --- Forge
   --

   function new_Box (Size  : in math.Vector_3;
                     Faces : in lit_colored_textured.Faces) return View
   is
      Self : constant View := new Item;
   begin
      Self.Faces := Faces;
      Self.Size  := Size;

      return Self;
   end new_Box;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Fonts);

      use openGL.Geometry.lit_colored_textured,
          openGL.Texture;

      the_Sites    :         constant box.Sites := Self.vertex_Sites;
      the_Indices  : aliased constant Indices   := (1, 2, 3, 4);


      function new_Face (Vertices : access openGL.geometry.lit_colored_textured.Vertex_array) return Geometry_view
      is
         use openGL.Primitive;

         the_Geometry  : constant Geometry_view  := openGL.Geometry.lit_colored_textured.new_Geometry
                                                      (texture_is_Alpha => False).all'unchecked_Access;
         the_Primitive : constant Primitive.view := Primitive.indexed.new_Primitive (triangle_Fan,
                                                                                     the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices.all);
         the_Geometry.add          (the_Primitive);

         return the_Geometry;
      end new_Face;


      front_Face : Geometry_view;
      rear_Face  : Geometry_view;
      upper_Face : Geometry_view;
      lower_Face : Geometry_view;
      left_Face  : Geometry_view;
      right_Face : Geometry_view;

   begin
      --  Front
      --
      declare
         the_Vertices : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1 => (site => the_Sites (left_lower_front),    normal => front_Normal,   color => Self.Faces (Front).Colors (1),   coords => (0.0, 0.0)),
               2 => (site => the_Sites (right_lower_front),   normal => front_Normal,   color => Self.Faces (Front).Colors (2),   coords => (1.0, 0.0)),
               3 => (site => the_Sites (right_upper_front),   normal => front_Normal,   color => Self.Faces (Front).Colors (3),   coords => (1.0, 1.0)),
               4 => (site => the_Sites (left_upper_front),    normal => front_Normal,   color => Self.Faces (Front).Colors (4),   coords => (0.0, 1.0)));
      begin
         front_Face := new_Face (vertices => the_Vertices'Access);

         if Self.Faces (Front).texture_Name /= null_Asset
         then
            front_Face.Texture_is (Textures.fetch (Self.Faces (Front).texture_Name));
            front_Face.is_Transparent (now => front_Face.Texture.is_Transparent);

--           elsif Self.Faces (Front).texture_Object /= null_Object
--           then
--              front_Face.Texture_is (Self.Faces (Front).texture_Object);
         end if;
      end;


      --  Rear
      --
      declare
         the_Vertices : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1 => (site => the_Sites (Right_Lower_Rear),   normal => rear_Normal,   color => Self.Faces (Rear).Colors (1),   coords => (0.0, 0.0)),
               2 => (site => the_Sites (Left_Lower_Rear),    normal => rear_Normal,   color => Self.Faces (Rear).Colors (2),   coords => (1.0, 0.0)),
               3 => (site => the_Sites (Left_Upper_Rear),    normal => rear_Normal,   color => Self.Faces (Rear).Colors (3),   coords => (1.0, 1.0)),
               4 => (site => the_Sites (Right_Upper_Rear),   normal => rear_Normal,   color => Self.Faces (Rear).Colors (4),   coords => (0.0, 1.0)));
      begin
         rear_Face := new_Face (vertices => the_Vertices'Access);

         if Self.Faces (Rear).texture_Name /= null_Asset
         then
            rear_Face.Texture_is (Textures.fetch (Self.Faces (Rear).texture_Name));
            rear_Face.is_Transparent (now => rear_Face.Texture.is_Transparent);
         end if;
      end;


      --  Upper
      --
      declare
         the_Vertices : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1 => (site => the_Sites (Left_Upper_Front),    normal => upper_Normal,   color => Self.Faces (Upper).Colors (1),   coords => (0.0, 0.0)),
               2 => (site => the_Sites (Right_Upper_Front),   normal => upper_Normal,   color => Self.Faces (Upper).Colors (2),   coords => (1.0, 0.0)),
               3 => (site => the_Sites (Right_Upper_Rear),    normal => upper_Normal,   color => Self.Faces (Upper).Colors (3),   coords => (1.0, 1.0)),
               4 => (site => the_Sites (Left_Upper_Rear),     normal => upper_Normal,   color => Self.Faces (Upper).Colors (4),   coords => (0.0, 1.0)));
      begin
         upper_Face := new_Face (vertices => the_Vertices'Access);

         if Self.Faces (Upper).texture_Name /= null_Asset
         then
            upper_Face.Texture_is (Textures.fetch (Self.Faces (Upper).texture_Name));
            upper_Face.is_Transparent (now => upper_Face.Texture.is_Transparent);
         end if;
      end;


      --  Lower
      --
      declare
         the_Vertices : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1 => (site => the_Sites (Right_Lower_Front),   normal => lower_Normal,   color => Self.Faces (Lower).Colors (1),   coords => (0.0, 0.0)),
               2 => (site => the_Sites (Left_Lower_Front),    normal => lower_Normal,   color => Self.Faces (Lower).Colors (2),   coords => (1.0, 0.0)),
               3 => (site => the_Sites (Left_Lower_Rear),     normal => lower_Normal,   color => Self.Faces (Lower).Colors (3),   coords => (1.0, 1.0)),
               4 => (site => the_Sites (Right_Lower_Rear),    normal => lower_Normal,   color => Self.Faces (Lower).Colors (4),   coords => (0.0, 1.0)));
      begin
         lower_Face := new_Face (vertices => the_Vertices'Access);

         if Self.Faces (Lower).texture_Name /= null_Asset
         then
            lower_Face.Texture_is (Textures.fetch (Self.Faces (Lower).texture_Name));
            lower_Face.is_Transparent (now => lower_Face.Texture.is_Transparent);
         end if;
      end;


      --  Left
      --
      declare
         the_Vertices : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1 => (site => the_Sites (Left_Lower_Rear),    normal => left_Normal,   color => Self.Faces (Left).Colors (1),   coords => (0.0, 0.0)),
               2 => (site => the_Sites (Left_Lower_Front),   normal => left_Normal,   color => Self.Faces (Left).Colors (2),   coords => (1.0, 0.0)),
               3 => (site => the_Sites (Left_Upper_Front),   normal => left_Normal,   color => Self.Faces (Left).Colors (3),   coords => (1.0, 1.0)),
               4 => (site => the_Sites (Left_Upper_Rear),    normal => left_Normal,   color => Self.Faces (Left).Colors (4),   coords => (0.0, 1.0)));
      begin
         left_Face := new_Face (vertices => the_Vertices'Access);

         if Self.Faces (Left).texture_Name /= null_Asset
         then
            left_Face.Texture_is (Textures.fetch (Self.Faces (Left).texture_Name));
            left_Face.is_Transparent (now => left_Face.Texture.is_Transparent);
         end if;
      end;


      --  Right
      --
      declare
         the_Vertices : aliased openGL.Geometry.lit_colored_textured.Vertex_array
           := (1 => (site => the_Sites (Right_Lower_Front),   normal => right_Normal,   color => Self.Faces (Right).Colors (1),   coords => (0.0, 0.0)),
               2 => (site => the_Sites (Right_Lower_Rear),    normal => right_Normal,   color => Self.Faces (Right).Colors (2),   coords => (1.0, 0.0)),
               3 => (site => the_Sites (Right_Upper_Rear),    normal => right_Normal,   color => Self.Faces (Right).Colors (3),   coords => (1.0, 1.0)),
               4 => (site => the_Sites (Right_Upper_Front),   normal => right_Normal,   color => Self.Faces (Right).Colors (4),   coords => (0.0, 1.0)));
      begin
         right_Face := new_Face (vertices => the_Vertices'Access);

         if Self.Faces (Right).texture_Name /= null_Asset
         then
            right_Face.Texture_is (Textures.fetch (Self.Faces (Right).texture_Name));
            right_Face.is_Transparent (now => right_Face.Texture.is_Transparent);
         end if;
      end;


      return (1 => front_Face.all'Access,
              2 =>  rear_Face.all'Access,
              3 => upper_Face.all'Access,
              4 => lower_Face.all'Access,
              5 =>  left_Face.all'Access,
              6 => right_Face.all'Access);
   end to_GL_Geometries;


end openGL.Model.box.lit_colored_textured;
