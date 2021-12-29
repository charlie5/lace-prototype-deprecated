with
     openGL.Geometry.lit_colored,
     openGL.Primitive.indexed;


package body openGL.Model.box.lit_colored
is
   ---------
   --- Forge
   --

   function new_Box (Size  : in Vector_3;
                     Faces : in lit_colored.Faces) return View
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
      pragma unreferenced (Fonts, Textures);

      use Geometry.lit_colored;

      the_Sites    :         constant box.Sites := Self.vertex_Sites;
      the_Indices  : aliased constant Indices   := (1, 2, 3, 4);


      function new_Face (Vertices : access geometry.lit_colored.Vertex_array) return Geometry.lit_colored.view
      is
         use openGL.Primitive;

         the_Geometry  : constant Geometry.lit_colored.view := Geometry.lit_colored.new_Geometry;
         the_Primitive : constant Primitive.view            := Primitive.indexed.new_Primitive
                                                                 (triangle_Fan,
                                                                  the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices.all);
         the_Geometry.add          (the_Primitive);

         return the_Geometry;
      end new_Face;


      front_Face : Geometry.lit_colored.view;
      rear_Face  : Geometry.lit_colored.view;
      upper_Face : Geometry.lit_colored.view;
      lower_Face : Geometry.lit_colored.view;
      left_Face  : Geometry.lit_colored.view;
      right_Face : Geometry.lit_colored.view;

   begin
      --  Front
      --
      declare
         the_Vertices : aliased Geometry.lit_colored.Vertex_array
           := (1 => (Site => the_Sites ( Left_Lower_Front),   Normal => front_Normal,   Color => +Self.Faces (Front).Colors (1),   Shine => default_Shine),
               2 => (Site => the_Sites (Right_Lower_Front),   Normal => front_Normal,   Color => +Self.Faces (Front).Colors (2),   Shine => default_Shine),
               3 => (Site => the_Sites (right_upper_front),   Normal => front_Normal,   Color => +Self.Faces (Front).Colors (3),   Shine => default_Shine),
               4 => (Site => the_Sites ( Left_Upper_Front),   Normal => front_Normal,   Color => +Self.Faces (Front).Colors (4),   Shine => default_Shine));
      begin
         front_Face := new_Face (Vertices => the_Vertices'Access);
      end;


      --  Rear
      --
      declare
         the_Vertices : aliased Geometry.lit_colored.Vertex_array
           := (1 => (Site => the_Sites (Right_Lower_Rear),   Normal => rear_Normal,   Color => +Self.Faces (Rear).Colors (1),   Shine => default_Shine),
               2 => (Site => the_Sites ( Left_Lower_Rear),   Normal => rear_Normal,   Color => +Self.Faces (Rear).Colors (2),   Shine => default_Shine),
               3 => (Site => the_Sites ( Left_Upper_Rear),   Normal => rear_Normal,   Color => +Self.Faces (Rear).Colors (3),   Shine => default_Shine),
               4 => (Site => the_Sites (Right_Upper_Rear),   Normal => rear_Normal,   Color => +Self.Faces (Rear).Colors (4),   Shine => default_Shine));
      begin
         rear_Face := new_Face (Vertices => the_Vertices'Access);
      end;


      --  Upper
      --
      declare
         the_Vertices : aliased Geometry.lit_colored.Vertex_array
           := (1 => (Site => the_Sites ( Left_Upper_Front),   Normal => upper_Normal,   Color => +Self.Faces (Upper).Colors (1),   Shine => default_Shine),
               2 => (Site => the_Sites (Right_Upper_Front),   Normal => upper_Normal,   Color => +Self.Faces (Upper).Colors (2),   Shine => default_Shine),
               3 => (Site => the_Sites (Right_Upper_Rear),    Normal => upper_Normal,   Color => +Self.Faces (Upper).Colors (3),   Shine => default_Shine),
               4 => (Site => the_Sites ( Left_Upper_Rear),    Normal => upper_Normal,   Color => +Self.Faces (Upper).Colors (4),   Shine => default_Shine));
      begin
         upper_Face := new_Face (Vertices => the_Vertices'Access);
      end;


      --  Lower
      --
      declare
         the_Vertices : aliased Geometry.lit_colored.Vertex_array
           := (1 => (Site => the_Sites (Right_Lower_Front),   Normal => lower_Normal,   Color => +Self.Faces (Lower).Colors (1),   Shine => default_Shine),
               2 => (Site => the_Sites ( Left_Lower_Front),   Normal => lower_Normal,   Color => +Self.Faces (Lower).Colors (2),   Shine => default_Shine),
               3 => (Site => the_Sites ( Left_Lower_Rear),    Normal => lower_Normal,   Color => +Self.Faces (Lower).Colors (3),   Shine => default_Shine),
               4 => (Site => the_Sites (Right_Lower_Rear),    Normal => lower_Normal,   Color => +Self.Faces (Lower).Colors (4),   Shine => default_Shine));
      begin
         lower_Face := new_Face (Vertices => the_Vertices'Access);
      end;


      --  Left
      --
      declare
         the_Vertices : aliased Geometry.lit_colored.Vertex_array
           := (1 => (Site => the_Sites (Left_Lower_Rear),    Normal => left_Normal,   Color => +Self.Faces (Left).Colors (1),   Shine => default_Shine),
               2 => (Site => the_Sites (Left_Lower_Front),   Normal => left_Normal,   Color => +Self.Faces (Left).Colors (2),   Shine => default_Shine),
               3 => (Site => the_Sites (Left_Upper_Front),   Normal => left_Normal,   Color => +Self.Faces (Left).Colors (3),   Shine => default_Shine),
               4 => (Site => the_Sites (Left_Upper_Rear),    Normal => left_Normal,   Color => +Self.Faces (Left).Colors (4),   Shine => default_Shine));
      begin
         left_Face := new_Face (Vertices => the_Vertices'Access);
      end;


      --  Right
      --
      declare
         the_Vertices : aliased Geometry.lit_colored.Vertex_array
           := (1 => (Site => the_Sites (Right_Lower_Front),   Normal => right_Normal,   Color => +Self.Faces (Right).Colors (1),   Shine => default_Shine),
               2 => (Site => the_Sites (Right_Lower_Rear),    Normal => right_Normal,   Color => +Self.Faces (Right).Colors (2),   Shine => default_Shine),
               3 => (Site => the_Sites (Right_Upper_Rear),    Normal => right_Normal,   Color => +Self.Faces (Right).Colors (3),   Shine => default_Shine),
               4 => (Site => the_Sites (Right_Upper_Front),   Normal => right_Normal,   Color => +Self.Faces (Right).Colors (4),   Shine => default_Shine));
      begin
         right_Face := new_Face (Vertices => the_Vertices'Access);
      end;


      return (1 => front_Face.all'Access,
              2 =>  rear_Face.all'Access,
              3 => upper_Face.all'Access,
              4 => lower_Face.all'Access,
              5 =>  left_Face.all'Access,
              6 => right_Face.all'Access);
   end to_GL_Geometries;


end openGL.Model.box.lit_colored;
