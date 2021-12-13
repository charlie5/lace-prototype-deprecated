with
     openGL.Primitive.indexed,
     openGL.Geometry.colored;


package body openGL.Model.box.colored
is
   ---------
   --- Forge
   --

   function new_Box (Size  : in Vector_3;
                     Faces : in colored.Faces) return View
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
      pragma unreferenced (Textures, Fonts);
      use Geometry;

      the_Sites    :         constant box.Sites := Self.vertex_Sites;
      the_Indices  : aliased constant Indices   := (1, 2, 3, 4);


      function new_Face (Vertices : access Geometry.colored.Vertex_array) return Geometry.colored.view
      is
         use Geometry.colored,
             Primitive;

         the_Geometry  : constant Geometry.colored .view := Geometry.colored.new_Geometry;
         the_Primitive : constant Primitive.indexed.view := Primitive.indexed.new_Primitive (triangle_Fan,
                                                                                             the_Indices);
      begin
         the_Geometry.Vertices_are   (Vertices.all);
         the_Geometry.add            (Primitive.view (the_Primitive));
         the_Geometry.is_Transparent (now => False);

         return the_Geometry;
      end new_Face;


      front_Face : Geometry.colored.view;
      rear_Face  : Geometry.colored.view;
      upper_Face : Geometry.colored.view;
      lower_Face : Geometry.colored.view;
      left_Face  : Geometry.colored.view;
      right_Face : Geometry.colored.view;

   begin
      --  Front
      --
      declare
         the_Vertices : aliased Geometry.colored.Vertex_array
           := (1 => (Site => the_Sites ( Left_Lower_Front),  Color => +Self.Faces (Front).Colors (1)),
               2 => (Site => the_Sites (Right_Lower_Front),  Color => +Self.Faces (Front).Colors (2)),
               3 => (Site => the_Sites (Right_Upper_Front),  Color => +Self.Faces (Front).Colors (3)),
               4 => (Site => the_Sites ( Left_Upper_Front),  Color => +Self.Faces (Front).Colors (4)));
      begin
         front_Face := new_Face (Vertices => the_Vertices'Access);
      end;

      --  Rear
      --
      declare
         the_Vertices : aliased Geometry.colored.Vertex_array
           := (1 => (Site => the_Sites (Right_Lower_Rear),   Color => +Self.Faces (Rear).Colors (1)),
               2 => (Site => the_Sites ( Left_Lower_Rear),   Color => +Self.Faces (Rear).Colors (2)),
               3 => (Site => the_Sites ( Left_Upper_Rear),   Color => +Self.Faces (Rear).Colors (3)),
               4 => (Site => the_Sites (Right_Upper_Rear),   Color => +Self.Faces (Rear).Colors (4)));
      begin
         rear_Face := new_Face (Vertices => the_Vertices'Access);
      end;

      --  Upper
      --
      declare
         the_Vertices : aliased Geometry.colored.Vertex_array
           := (1 => (Site => the_Sites ( Left_Upper_Front),  Color => +Self.Faces (Upper).Colors (1)),
               2 => (Site => the_Sites (Right_Upper_Front),  Color => +Self.Faces (Upper).Colors (2)),
               3 => (Site => the_Sites (Right_Upper_Rear),   Color => +Self.Faces (Upper).Colors (3)),
               4 => (Site => the_Sites ( Left_Upper_Rear),   Color => +Self.Faces (Upper).Colors (4)));
      begin
         upper_Face := new_Face (Vertices => the_Vertices'Access);
      end;

      --  Lower
      --
      declare
         the_Vertices : aliased Geometry.colored.Vertex_array
           := (1 => (Site => the_Sites (Right_Lower_Front),  Color => +Self.Faces (Lower).Colors (1)),
               2 => (Site => the_Sites ( Left_Lower_Front),  Color => +Self.Faces (Lower).Colors (2)),
               3 => (Site => the_Sites ( Left_Lower_Rear),   Color => +Self.Faces (Lower).Colors (3)),
               4 => (Site => the_Sites (Right_Lower_Rear),   Color => +Self.Faces (Lower).Colors (4)));
      begin
         lower_Face := new_Face (Vertices => the_Vertices'Access);
      end;

      --  Left
      --
      declare
         the_Vertices : aliased Geometry.colored.Vertex_array
           := (1 => (Site => the_Sites (Left_Lower_Rear),    Color => +Self.Faces (Left).Colors (1)),
               2 => (Site => the_Sites (Left_Lower_Front),   Color => +Self.Faces (Left).Colors (2)),
               3 => (Site => the_Sites (Left_Upper_Front),   Color => +Self.Faces (Left).Colors (3)),
               4 => (Site => the_Sites (Left_Upper_Rear),    Color => +Self.Faces (Left).Colors (4)));
      begin
         left_Face := new_Face (Vertices => the_Vertices'Access);
      end;

      --  Right
      --
      declare
         the_Vertices : aliased Geometry.colored.Vertex_array
           := (1 => (Site => the_Sites (Right_Lower_Front),  Color => +Self.Faces (Right).Colors (1)),
               2 => (Site => the_Sites (Right_Lower_Rear),   Color => +Self.Faces (Right).Colors (2)),
               3 => (Site => the_Sites (Right_Upper_Rear),   Color => +Self.Faces (Right).Colors (3)),
               4 => (Site => the_Sites (Right_Upper_Front),  Color => +Self.Faces (Right).Colors (4)));
      begin
         right_Face := new_Face (Vertices => the_Vertices'Access);
      end;

      return (Geometry.view (front_Face),
              Geometry.view ( rear_Face),
              Geometry.view (upper_Face),
              Geometry.view (lower_Face),
              Geometry.view ( left_Face),
              Geometry.view (right_Face));
   end to_GL_Geometries;


end openGL.Model.box.colored;
