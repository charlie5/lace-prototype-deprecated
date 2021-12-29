with
     openGL.Geometry.lit_textured,
     openGL.Primitive.indexed;


package body openGL.Model.box.lit_textured
is
   ---------
   --- Forge
   --

   function new_Box (Size  : in Vector_3;
                     Faces : in lit_textured.Faces) return View
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
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Fonts);

      use Geometry.lit_textured;

      the_Sites    :         constant box.Sites := Self.vertex_Sites;
      the_Indices  : aliased constant Indices   := (1, 2, 3, 4);


      function new_Face (Vertices : in geometry.lit_textured.Vertex_array) return Geometry.lit_textured.view
      is
         use openGL.Primitive;

         the_Geometry  : constant Geometry.lit_textured.view  := Geometry.lit_textured.new_Geometry;
         the_Primitive : constant Primitive.view              := Primitive.indexed.new_Primitive
                                                                   (triangle_Fan,
                                                                    the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices);
         the_Geometry.add          (the_Primitive);

         return the_Geometry;
      end new_Face;


      front_Face : Geometry.lit_textured.view;
      rear_Face  : Geometry.lit_textured.view;
      upper_Face : Geometry.lit_textured.view;
      lower_Face : Geometry.lit_textured.view;
      left_Face  : Geometry.lit_textured.view;
      right_Face : Geometry.lit_textured.view;

   begin
      --  Front
      --
      declare
         the_Vertices : constant Geometry.lit_textured.Vertex_array
           := (1 => (Site => the_Sites ( Left_Lower_Front),   Normal => front_Normal,   Coords => (0.0, 0.0),   Shine => default_Shine),
               2 => (Site => the_Sites (Right_Lower_Front),   Normal => front_Normal,   Coords => (1.0, 0.0),   Shine => default_Shine),
               3 => (Site => the_Sites (right_upper_front),   Normal => front_Normal,   Coords => (1.0, 1.0),   Shine => default_Shine),
               4 => (Site => the_Sites ( Left_Upper_Front),   Normal => front_Normal,   Coords => (0.0, 1.0),   Shine => default_Shine));
      begin
         front_Face := new_Face (Vertices => the_Vertices);

         if Self.Faces (Front).texture_Name /= null_Asset
         then
            front_Face.Texture_is (Textures.fetch (Self.Faces (Front).texture_Name));
            front_Face.is_Transparent (now => front_Face.Texture.is_Transparent);
         end if;
      end;


      --  Rear
      --
      declare
         the_Vertices : constant Geometry.lit_textured.Vertex_array
           := (1 => (Site => the_Sites (Right_Lower_Rear),   Normal => rear_Normal,   Coords => (0.0, 0.0),   Shine => default_Shine),
               2 => (Site => the_Sites ( Left_Lower_Rear),   Normal => rear_Normal,   Coords => (1.0, 0.0),   Shine => default_Shine),
               3 => (Site => the_Sites ( Left_Upper_Rear),   Normal => rear_Normal,   Coords => (1.0, 1.0),   Shine => default_Shine),
               4 => (Site => the_Sites (Right_Upper_Rear),   Normal => rear_Normal,   Coords => (0.0, 1.0),   Shine => default_Shine));
      begin
         rear_Face := new_Face (Vertices => the_Vertices);

         if Self.Faces (Rear).texture_Name /= null_Asset
         then
            rear_Face.Texture_is (Textures.fetch (Self.Faces (Rear).texture_Name));
            rear_Face.is_Transparent (now => rear_Face.Texture.is_Transparent);
         end if;
      end;


      --  Upper
      --
      declare
         the_Vertices : constant Geometry.lit_textured.Vertex_array
           := (1 => (Site => the_Sites ( Left_Upper_Front),   Normal => upper_Normal,   Coords => (0.0, 0.0),   Shine => default_Shine),
               2 => (Site => the_Sites (Right_Upper_Front),   Normal => upper_Normal,   Coords => (1.0, 0.0),   Shine => default_Shine),
               3 => (Site => the_Sites (Right_Upper_Rear),    Normal => upper_Normal,   Coords => (1.0, 1.0),   Shine => default_Shine),
               4 => (Site => the_Sites ( Left_Upper_Rear),    Normal => upper_Normal,   Coords => (0.0, 1.0),   Shine => default_Shine));
      begin
         upper_Face := new_Face (Vertices => the_Vertices);

         if Self.Faces (Upper).texture_Name /= null_Asset
         then
            upper_Face.Texture_is (Textures.fetch (Self.Faces (Upper).texture_Name));
            upper_Face.is_Transparent (now => upper_Face.Texture.is_Transparent);
         end if;
      end;


      --  Lower
      --
      declare
         the_Vertices : constant Geometry.lit_textured.Vertex_array
           := (1 => (Site => the_Sites (Right_Lower_Front),   Normal => lower_Normal,   Coords => (0.0, 0.0),   Shine => default_Shine),
               2 => (Site => the_Sites ( Left_Lower_Front),   Normal => lower_Normal,   Coords => (1.0, 0.0),   Shine => default_Shine),
               3 => (Site => the_Sites ( Left_Lower_Rear),    Normal => lower_Normal,   Coords => (1.0, 1.0),   Shine => default_Shine),
               4 => (Site => the_Sites (Right_Lower_Rear),    Normal => lower_Normal,   Coords => (0.0, 1.0),   Shine => default_Shine));
      begin
         lower_Face := new_Face (Vertices => the_Vertices);

         if Self.Faces (Lower).texture_Name /= null_Asset
         then
            lower_Face.Texture_is (Textures.fetch (Self.Faces (Lower).texture_Name));
            lower_Face.is_Transparent (now => lower_Face.Texture.is_Transparent);
         end if;
      end;


      --  Left
      --
      declare
         the_Vertices : constant Geometry.lit_textured.Vertex_array
           := (1 => (Site => the_Sites (Left_Lower_Rear),    Normal => left_Normal,   Coords => (0.0, 0.0),   Shine => default_Shine),
               2 => (Site => the_Sites (Left_Lower_Front),   Normal => left_Normal,   Coords => (1.0, 0.0),   Shine => default_Shine),
               3 => (Site => the_Sites (Left_Upper_Front),   Normal => left_Normal,   Coords => (1.0, 1.0),   Shine => default_Shine),
               4 => (Site => the_Sites (Left_Upper_Rear),    Normal => left_Normal,   Coords => (0.0, 1.0),   Shine => default_Shine));
      begin
         left_Face := new_Face (Vertices => the_Vertices);

         if Self.Faces (Left).texture_Name /= null_Asset
         then
            left_Face.Texture_is (Textures.fetch (Self.Faces (Left).texture_Name));
            left_Face.is_Transparent (now => left_Face.Texture.is_Transparent);
         end if;
      end;


      --  Right
      --
      declare
         the_Vertices : constant Geometry.lit_textured.Vertex_array
           := (1 => (Site => the_Sites (Right_Lower_Front),   Normal => right_Normal,   Coords => (0.0, 0.0),   Shine => default_Shine),
               2 => (Site => the_Sites (Right_Lower_Rear),    Normal => right_Normal,   Coords => (1.0, 0.0),   Shine => default_Shine),
               3 => (Site => the_Sites (Right_Upper_Rear),    Normal => right_Normal,   Coords => (1.0, 1.0),   Shine => default_Shine),
               4 => (Site => the_Sites (Right_Upper_Front),   Normal => right_Normal,   Coords => (0.0, 1.0),   Shine => default_Shine));
      begin
         right_Face := new_Face (Vertices => the_Vertices);

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


end openGL.Model.box.lit_textured;
