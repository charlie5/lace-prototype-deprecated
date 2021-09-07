with
     openGL.Program.lit_colored_textured,
     openGL.Palette,
     openGL.Shader,
     openGL.Buffer.general,
     openGL.Attribute,
     openGL.Texture,
     openGL.Tasks,

     GL.Binding,
     GL.lean,
     GL.Pointers,

     Interfaces.C.Strings,
     System.storage_Elements;


package body openGL.Geometry.lit_colored_textured
is
   use GL.lean,
       GL.Pointers,
       Interfaces,
       System;


   ------------------
   --  Shader Program
   --

   type program_Id is (rgba_Texture, alpha_Texture);

   type Program is
      record
         vertex_Shader   : aliased openGL.Shader.item;
         fragment_Shader : aliased openGL.Shader.item;
         Program         :         openGL.Program.lit_colored_textured.view;
      end record;

   type Programs is array (program_Id) of aliased Program;


   -----------
   --- Globals
   --

   the_Programs         :                  Programs;

   Attribute_1_Name     : aliased          C.char_array        := "aSite";
   Attribute_1_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_1_Name'Access);

   Attribute_2_Name     : aliased          C.char_array        := "aNormal";
   Attribute_2_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_2_Name'Access);

   Attribute_3_Name     : aliased          C.char_array        := "aColor";
   Attribute_3_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_3_Name'Access);

   Attribute_4_Name     : aliased          C.char_array        := "aCoords";
   Attribute_4_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_4_Name'Access);

   white_Texture        :                  openGL.Texture.Object;


   ---------
   --  Forge
   --

   type Geometry_view is access all Geometry.lit_colored_textured.item'Class;

   function new_Geometry (texture_is_Alpha : in Boolean) return access Geometry.lit_colored_textured.item'Class
   is
      use type openGL.Program.lit_colored_textured.view;

      check_is_OK : constant Boolean       :=     openGL.Tasks.Check;   pragma Unreferenced (check_is_OK);
      Self        : constant Geometry_view := new Geometry.lit_colored_textured.item;

      procedure define (the_Program         : access Program;
                        use_fragment_Shader : in     String)
      is
         use openGL.Palette,
             system.Storage_Elements;

         sample_Vertex : Vertex;

         Attribute_1   : openGL.Attribute.view;
         Attribute_2   : openGL.Attribute.view;
         Attribute_3   : openGL.Attribute.view;
         Attribute_4   : openGL.Attribute.view;

         white_Image   : constant openGL.Image := (1 .. 2 => (1 .. 2 => White));

      begin
         white_Texture       := openGL.Texture.Forge.to_Texture (white_Image);
         the_Program.Program := new openGL.Program.lit_colored_textured.item;

         the_Program.  vertex_Shader.define (openGL.Shader.Vertex,   "assets/opengl/shader/lit_colored_textured.vert");
         the_Program.fragment_Shader.define (openGL.Shader.Fragment, use_fragment_Shader);

         the_Program.Program.define (the_Program.vertex_Shader  'Access,
                                     the_Program.fragment_Shader'Access);
         the_Program.Program.enable;

         Attribute_1 := attribute.Forge.new_Attribute
                          (name        => "aSite",
                           gl_location => the_Program.Program.attribute_Location ("aSite"),
                           size        => 3,
                           data_kind   => attribute.GL_FLOAT,
                           stride      => lit_colored_textured.Vertex'Size / 8,
                           offset      => 0,
                           normalized  => False);

         Attribute_2 := attribute.Forge.new_Attribute
                          (name        => "aNormal",
                           gl_location => the_Program.Program.attribute_Location ("aNormal"),
                           size        => 3,
                           data_kind   => attribute.GL_FLOAT,
                           stride      => lit_colored_textured.Vertex'Size / 8,
                           offset      =>   sample_Vertex.Normal (1)'Address
                                          - sample_Vertex.Site   (1)'Address,
                           normalized  => False);

         Attribute_3 := attribute.Forge.new_Attribute
                          (name        => "aColor",
                           gl_location => the_Program.Program.attribute_Location ("aColor"),
                           size        => 4,
                           data_kind   => attribute.GL_UNSIGNED_BYTE,
                           stride      => lit_colored_textured.Vertex'Size / 8,
                           offset      =>   sample_Vertex.Color.Primary.Red'Address
                                          - sample_Vertex.Site (1)         'Address,
                           normalized  => True);

         Attribute_4 := attribute.Forge.new_Attribute
                          (name        => "aCoords",
                           gl_location => the_Program.Program.attribute_Location ("aCoords"),
                           size        => 2,
                           data_kind   => attribute.GL_FLOAT,
                           stride      => lit_colored_textured.Vertex'Size / 8,
                           offset      =>   sample_Vertex.Coords.S'Address
                                          - sample_Vertex.Site (1)'Address,
                           normalized  => False);

         the_Program.Program.add (Attribute_1);
         the_Program.Program.add (Attribute_2);
         the_Program.Program.add (Attribute_3);
         the_Program.Program.add (Attribute_4);

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => "aSite").gl_Location,
                               name    => +Attribute_1_Name_ptr);

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => "aNormal").gl_Location,
                               name    => +Attribute_2_Name_ptr);

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => "aColor").gl_Location,
                               name    => +Attribute_3_Name_ptr);

         glBindAttribLocation (program =>  the_Program.Program.gl_Program,
                               index   =>  the_Program.Program.Attribute (named => "aCoords").gl_Location,
                               name    => +Attribute_4_Name_ptr);
      end define;

   begin
      --  Define the shaders and program, if required.
      --
      if texture_is_Alpha
      then
         if the_Programs (alpha_Texture).Program = null
         then
            define (the_Programs (alpha_Texture)'Access,
                    use_fragment_shader => "assets/opengl/shader/lit_colored_textured-text.frag");
         end if;
      else
         if the_Programs (rgba_Texture).Program = null
         then
            define (the_Programs (rgba_Texture)'Access,
                    use_fragment_shader => "assets/opengl/shader/lit_colored_textured.frag");
         end if;
      end if;

      if texture_is_Alpha
      then   Self.Program_is (openGL.Program.view (the_Programs (alpha_Texture).Program));
      else   Self.Program_is (openGL.Program.view (the_Programs (rgba_Texture ).Program));
      end if;

      return Self;
   end new_Geometry;


   ----------
   --  Vertex
   --

   function is_Transparent (Self : in Vertex_array) return Boolean
   is
      function get_Color (Index : in Index_t) return lucid_Color
      is (Self (Index).Color);

      function my_Transparency is new get_Transparency (any_Index_t => Index_t,
                                                        get_Color   => get_Color);
   begin
      return my_Transparency (count => Self'Length);
   end is_Transparent;


   --------------
   --  Attributes
   --

   package openGL_Buffer_of_geometry_Vertices is new openGL.Buffer.general (base_object   => openGL.Buffer.array_Object,
                                                                            index         => Index_t,
                                                                            element       => Vertex,
                                                                            element_array => Vertex_array);

   procedure Vertices_are (Self : in out Item;   Now : in Vertex_array)
   is
      use      openGL_Buffer_of_geometry_Vertices;
      use type openGL.Buffer.view;
   begin
      if Self.Vertices = null
      then
         self.Vertices := new openGL_Buffer_of_geometry_Vertices.Object' (Forge.to_Buffer (Now,
                                                                                           usage => openGL.buffer.static_Draw));
      else
         set (openGL_Buffer_of_geometry_Vertices.Object (Self.Vertices.all),
              to => Now);
      end if;

      Self.is_Transparent := is_Transparent (Now);

      -- Set the bounds.
      --
      declare
         function get_Site (Index : in Index_t) return Vector_3
         is (Now (Index).Site);

         function bBox is new get_Bounds (Index_t, get_Site);
      begin
         Self.Bounds_are (bBox (count => Now'Length));
      end;
   end Vertices_are;


   overriding
   procedure Indices_are  (Self : in out Item;   Now       : in Indices;
                                                 for_Facia : in Positive)
   is
   begin
      raise Program_Error with "TBD";
   end Indices_are;


   overriding
   procedure enable_Texture (Self : in Item)
   is
      use GL,
          GL.Binding,
          openGL.Texture;
      check_is_OK : constant Boolean := openGL.Tasks.Check;   pragma Unreferenced (check_is_OK);
   begin
      glActiveTexture (gl.GL_TEXTURE0);

      if Self.Texture = openGL.Texture.null_Object
      then   enable (white_Texture);
      else   enable (Self.Texture);
      end if;
   end enable_Texture;


end openGL.Geometry.lit_colored_textured;
