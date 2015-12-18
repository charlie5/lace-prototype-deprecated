with
     openGL.Geometry.textured,
     openGL.Primitive.indexed,

     ada.unchecked_Deallocation;


package body openGL.Model.box.textured
is

   type Geometry_view is access all openGL.Geometry.textured.item'class;


   ---------
   --- Forge
   --

   package body Forge
   is
      function new_Box (Scale     : in math.Vector_3;
                        Faces     : in textured.Faces;
                        is_Skybox : in Boolean       := False) return View
      is
         Self : constant View := new Item;
      begin
         Self.Faces     := Faces;
         Self.is_Skybox := is_Skybox;

         Self.define (Scale);
         Self.set_Bounds;

         return Self;
      end new_Box;
   end Forge;


   procedure free (Self : in out view)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;



   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views
   is
      pragma Unreferenced (Fonts);

      use openGL.Geometry,
          openGL.Geometry.textured,
          openGL.Texture,
          math.Geometry;

      left_Offset  : constant Real := -0.5 * Real (self.Scale (1));
      right_Offset : constant Real :=  0.5 * Real (self.Scale (1));

      lower_Offset : constant Real := -0.5 * Real (self.Scale (2));
      upper_Offset : constant Real :=  0.5 * Real (self.Scale (2));

      front_Offset : constant Real :=  0.5 * Real (self.Scale (3));
      rear_Offset  : constant Real := -0.5 * Real (self.Scale (3));


      the_Sites    :          box.Sites := Self.vertex_Sites;
      the_Indices  : aliased  Indices   := (1, 2, 3, 4);


      function new_Face (Vertices : access openGL.geometry.textured.Vertex_array;
                         Bounds   : in     openGL.Bounds) return Geometry_view
      is
         use openGL.Primitive;

         the_Geometry  : constant Geometry_view  := openGL.Geometry.textured.new_Geometry.all'Access;
         the_Primitive : constant Primitive.view := Primitive.indexed.new_Primitive (triangle_Fan,
                                                                                     the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices);
         the_Geometry.add          (the_Primitive);
         the_Geometry.Bounds_are   (Bounds);

         return the_Geometry;
      end new_Face;


      front_Face : Geometry_view;
      rear_Face  : Geometry_view;
      upper_Face : Geometry_view;
      lower_Face : Geometry_view;
      left_Face  : Geometry_view;
      right_Face : Geometry_view;

   begin
      if Self.is_Skybox
      then
         the_Indices := (4, 3, 2, 1);
      end if;



      --  Scale sites.
      --
      for i in the_Sites'Range
      loop
         the_Sites (i) := Scaled (the_Sites (i), Self.Scale);
      end loop;


      --  Front
      --
      declare
         the_Vertices : aliased openGL.Geometry.textured.Vertex_array
           := (1 => (site => the_Sites (left_lower_front),    coords => (0.0, 0.0)),
               2 => (site => the_Sites (right_lower_front),   coords => (1.0, 0.0)),
               3 => (site => the_Sites (right_upper_front),   coords => (1.0, 1.0)),
               4 => (site => the_Sites (left_upper_front),    coords => (0.0, 1.0)));
      begin
         front_Face := new_Face (vertices => the_Vertices'Access,
                                 bounds   => (ball => abs (the_Sites (left_lower_front)),
                                              box  => (lower => (left_Offset,
                                                                 lower_Offset,
                                                                 front_Offset),
                                                       upper => (right_Offset,
                                                                 upper_Offset,
                                                                 front_Offset))));

         if Self.Faces (Front).texture_Name /= null_Asset
         then
            front_Face.Texture_is (Textures.fetch (to_String (Self.Faces (Front).texture_Name)));
            front_Face.is_Transparent (now => front_Face.Texture.is_Transparent);
         end if;
      end;


      --  Rear
      --
      declare
         the_Vertices : aliased openGL.Geometry.textured.Vertex_array
           := (1 => (site => the_Sites (Right_Lower_Rear),   coords => (0.0, 0.0)),
               2 => (site => the_Sites (Left_Lower_Rear),    coords => (1.0, 0.0)),
               3 => (site => the_Sites (Left_Upper_Rear),    coords => (1.0, 1.0)),
               4 => (site => the_Sites (Right_Upper_Rear),   coords => (0.0, 1.0)));
      begin
         rear_Face := new_Face (vertices => the_Vertices'Access,
                                bounds   => (ball => abs (the_Sites (Right_Lower_Rear)),
                                             box  => (lower => (left_Offset,
                                                                lower_Offset,
                                                                rear_Offset),
                                                      upper => (right_Offset,
                                                                upper_Offset,
                                                                rear_Offset))));
         if Self.Faces (Rear).texture_Name /= null_Asset
         then
            rear_Face.Texture_is     (Textures.fetch (to_String (Self.Faces (Front).texture_Name)));
            rear_Face.is_Transparent (now => rear_Face.Texture.is_Transparent);
         end if;
      end;


      --  Upper
      --
      declare
         the_Vertices : aliased openGL.Geometry.textured.Vertex_array
           := (1 => (site => the_Sites (Left_Upper_Front),    coords => (0.0, 0.0)),
               2 => (site => the_Sites (Right_Upper_Front),   coords => (1.0, 0.0)),
               3 => (site => the_Sites (Right_Upper_Rear),    coords => (1.0, 1.0)),
               4 => (site => the_Sites (Left_Upper_Rear),     coords => (0.0, 1.0)));
      begin
         upper_Face := new_Face (vertices => the_Vertices'Access,
                                 bounds   => (ball => abs (the_Sites (Left_Upper_Front)),
                                              box  => (lower => (left_Offset,
                                                                 upper_Offset,
                                                                 rear_Offset),
                                                       upper => (right_Offset,
                                                                 upper_Offset,
                                                                 front_Offset))));
         if Self.Faces (Upper).texture_Name /= null_Asset
         then
            upper_Face.Texture_is     (Textures.fetch (to_String (Self.Faces (Front).texture_Name)));
            upper_Face.is_Transparent (now => upper_Face.Texture.is_Transparent);
         end if;
      end;


      --  Lower
      --
      declare
         the_Vertices : aliased openGL.Geometry.textured.Vertex_array
           := (1 => (site => the_Sites (Right_Lower_Front),   coords => (0.0, 0.0)),
               2 => (site => the_Sites (Left_Lower_Front),    coords => (1.0, 0.0)),
               3 => (site => the_Sites (Left_Lower_Rear),     coords => (1.0, 1.0)),
               4 => (site => the_Sites (Right_Lower_Rear),    coords => (0.0, 1.0)));
      begin
         lower_Face := new_Face (vertices => the_Vertices'Access,
                                 bounds   => (ball => abs (the_Sites (Right_Lower_Front)),
                                              box  => (lower => (left_Offset,
                                                                 lower_Offset,
                                                                 rear_Offset),
                                                       upper => (right_Offset,
                                                                 lower_Offset,
                                                                 front_Offset))));
         if Self.Faces (Lower).texture_Name /= null_Asset
         then
            lower_Face.Texture_is     (Textures.fetch (to_String (Self.Faces (Front).texture_Name)));
            lower_Face.is_Transparent (now => lower_Face.Texture.is_Transparent);
         end if;
      end;


      --  Left
      --
      declare
         the_Vertices : aliased openGL.Geometry.textured.Vertex_array
           := (1 => (site => the_Sites (Left_Lower_Rear),    coords => (0.0, 0.0)),
               2 => (site => the_Sites (Left_Lower_Front),   coords => (1.0, 0.0)),
               3 => (site => the_Sites (Left_Upper_Front),   coords => (1.0, 1.0)),
               4 => (site => the_Sites (Left_Upper_Rear),    coords => (0.0, 1.0)));
      begin
         left_Face := new_Face (vertices => the_Vertices'Access,
                                bounds   => (ball => abs (the_Sites (Left_Lower_Rear)),
                                             box  => (lower => (left_Offset,
                                                                lower_Offset,
                                                                rear_Offset),
                                                      upper => (left_Offset,
                                                                upper_Offset,
                                                                front_Offset))));
         if Self.Faces (Left).texture_Name /= null_Asset
         then
            left_Face.Texture_is     (Textures.fetch (to_String (Self.Faces (Front).texture_Name)));
            left_Face.is_Transparent (now => left_Face.Texture.is_Transparent);
         end if;
      end;


      --  Right
      --
      declare
         the_Vertices : aliased openGL.Geometry.textured.Vertex_array
           := (1 => (site => the_Sites (Right_Lower_Front),   coords => (0.0, 0.0)),
               2 => (site => the_Sites (Right_Lower_Rear),    coords => (1.0, 0.0)),
               3 => (site => the_Sites (Right_Upper_Rear),    coords => (1.0, 1.0)),
               4 => (site => the_Sites (Right_Upper_Front),   coords => (0.0, 1.0)));
      begin
         right_Face := new_Face (vertices => the_Vertices'Access,
                                 bounds   => (ball => abs (the_Sites (Right_Lower_Front)),
                                              box  => (lower => (right_Offset,
                                                                 lower_Offset,
                                                                 rear_Offset),
                                                       upper => (right_Offset,
                                                                 upper_Offset,
                                                                 front_Offset))));
         if Self.Faces (Right).texture_Name /= null_Asset
         then
            right_Face.Texture_is     (Textures.fetch (to_String (Self.Faces (Front).texture_Name)));
            right_Face.is_Transparent (now => right_Face.Texture.is_Transparent);
         end if;
      end;


      return (1 => front_Face.all'Access,
              2 => rear_Face .all'Access,
              3 => upper_Face.all'Access,
              4 => lower_Face.all'Access,
              5 => left_Face .all'Access,
              6 => right_Face.all'Access);
   end to_GL_Geometries;


end openGL.Model.box.textured;
