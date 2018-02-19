with
     openGL.Palette,
     openGL.Texture,
     openGL.Geometry.lit_textured_skinned,
     openGL.Tasks,
     openGL.Conversions,

     GL.lean,
     GL.Pointers,

     ada.Strings.fixed,
     interfaces.c.Strings,
     system.Storage_Elements;


package body openGL.Program.lit_textured_skinned
is

   the_vertex_Shader   : aliased openGL.Shader.item;
   the_fragment_Shader : aliased openGL.Shader.item;

   white_Texture       :         openGL.Texture.Object;



--     overriding
--     procedure define (Self : in out Item)
--     is
--        use openGL.Palette,
--            GL.lean,
--            GL.Pointers,
--            Interfaces,
--            system.Storage_Elements;
--
--        check_is_OK          : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
--
--        sample_Vertex        :          Geometry.lit_textured_skinned.Vertex;
--
--        Attribute_1_Name     : aliased          C.char_array        := "aSite";
--        Attribute_1_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_1_Name'Unchecked_Access);
--
--        Attribute_2_Name     : aliased          C.char_array        := "aNormal";
--        Attribute_2_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_2_Name'Unchecked_Access);
--
--        Attribute_3_Name     : aliased          C.char_array        := "aColor";
--        Attribute_3_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_3_Name'Unchecked_Access);
--
--        Attribute_4_Name     : aliased          C.char_array        := "aCoords";
--        Attribute_4_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_4_Name'Unchecked_Access);
--
--        Attribute_5_Name     : aliased          C.char_array        := "bone_Ids";
--        Attribute_5_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_5_Name'Unchecked_Access);
--
--        Attribute_6_Name     : aliased          C.char_array        := "bone_Weights";
--        Attribute_6_Name_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (Attribute_6_Name'Unchecked_Access);
--
--        Attribute_1 : openGL.Attribute.view;
--        Attribute_2 : openGL.Attribute.view;
--        Attribute_3 : openGL.Attribute.view;
--        Attribute_4 : openGL.Attribute.view;
--        Attribute_5 : openGL.Attribute.view;
--        Attribute_6 : openGL.Attribute.view;
--
--        white_Image : constant openGL.Image := (1 .. 2 => (1 .. 2 => White));
--
--     begin
--        white_Texture := openGL.Texture.to_Texture (white_Image);
--
--        the_vertex_Shader  .define (openGL.Shader.Vertex,   "assets/opengl/shader/lit_textured_skinned.vert");
--        the_fragment_Shader.define (openGL.Shader.Fragment, "assets/opengl/shader/lit_textured_skinned.frag");
--
--        Self.define (the_vertex_Shader  'Access,
--                     the_fragment_Shader'Access);
--
--        Self.enable;
--
--        Attribute_1 := openGL.Attribute.Forge.new_Attribute
--          (name        => "aSite",
--           gl_location => Self.attribute_Location ("aSite"),
--           size        => 3,
--           data_kind   => openGL.Attribute.GL_FLOAT,
--           stride      => Geometry.lit_textured_skinned.Vertex'Size / 8,
--           offset      => 0,
--           normalized  => False);
--
--        Attribute_2 := openGL.Attribute.Forge.new_Attribute
--          (name        => "aNormal",
--           gl_location => Self.attribute_Location ("aNormal"),
--           size        => 3,
--           data_kind   => openGL.Attribute.GL_FLOAT,
--           stride      => Geometry.lit_textured_skinned.Vertex'Size / 8,
--           offset      =>   sample_Vertex.Normal (1)'Address
--                          - sample_Vertex.Site   (1)'Address,
--           normalized  => False);
--
--        Attribute_3 := openGL.Attribute.Forge.new_Attribute
--          (name        => "aColor",
--           gl_location => Self.attribute_Location ("aColor"),
--           size        => 4,
--           data_kind   => openGL.Attribute.GL_UNSIGNED_BYTE,
--           stride      => Geometry.lit_textured_skinned.Vertex'Size / 8,
--           offset      =>   sample_Vertex.Color.Primary.Red'Address
--                          - sample_Vertex.Site (1)         'Address,
--           normalized  => True);
--
--        Attribute_4 := openGL.Attribute.Forge.new_Attribute
--          (name        => "aCoords",
--           gl_location => Self.attribute_Location ("aCoords"),
--           size        => 2,
--           data_kind   => openGL.Attribute.GL_FLOAT,
--           stride      => Geometry.lit_textured_skinned.Vertex'Size / 8,
--           offset      =>   sample_Vertex.Coords.S'Address
--                          - sample_Vertex.Site (1)'Address,
--           normalized  => False);
--
--        Attribute_5 := openGL.Attribute.Forge.new_Attribute
--          (name        => "bone_Ids",
--           gl_location => Self.attribute_Location ("bone_Ids"),
--           size        => 4,
--           data_kind   => openGL.Attribute.GL_FLOAT,
--           stride      => Geometry.lit_textured_skinned.Vertex'Size / 8,
--           offset      =>   sample_Vertex.bone_Ids (1)'Address
--                          - sample_Vertex.Site (1)'Address,
--           normalized  => False);
--
--        Attribute_6 := openGL.Attribute.Forge.new_Attribute
--          (name        => "bone_Weights",
--           gl_location => Self.attribute_Location ("bone_Weights"),
--           size        => 4,
--           data_kind   => openGL.Attribute.GL_FLOAT,
--           stride      => Geometry.lit_textured_skinned.Vertex'Size / 8,
--           offset      =>   sample_Vertex.bone_Weights (1)'Address
--                          - sample_Vertex.Site (1)'Address,
--           normalized  => False);
--
--        Self.add (Attribute_1);
--        Self.add (Attribute_2);
--        Self.add (Attribute_3);
--        Self.add (Attribute_4);
--        Self.add (Attribute_5);
--        Self.add (Attribute_6);
--
--        glBindAttribLocation (program => Self.gl_Program,
--                              index   => Self.Attribute (named => "aSite").gl_Location,
--                              name    => +Attribute_1_Name_ptr);
--
--        glBindAttribLocation (program => Self.gl_Program,
--                              index   => Self.Attribute (named => "aNormal").gl_Location,
--                              name    => +Attribute_2_Name_ptr);
--
--        glBindAttribLocation (program => Self.gl_Program,
--                              index   => Self.Attribute (named => "aColor").gl_Location,
--                              name    => +Attribute_3_Name_ptr);
--
--        glBindAttribLocation (program => Self.gl_Program,
--                              index   => Self.Attribute (named => "aCoords").gl_Location,
--                              name    => +Attribute_4_Name_ptr);
--
--        glBindAttribLocation (program => Self.gl_Program,
--                              index   => Self.Attribute (named => "bone_Ids").gl_Location,
--                              name    => +Attribute_5_Name_ptr);
--
--        glBindAttribLocation (program => Self.gl_Program,
--                              index   => Self.Attribute (named => "bone_Weights").gl_Location,
--                              name    => +Attribute_6_Name_ptr);
--     end define;



   overriding
   procedure define  (Self : in out Item;   use_vertex_Shader   : in openGL.Shader.view;
                                            use_fragment_Shader : in openGL.Shader.view)
   is
      use ada.Strings,
          ada.Strings.fixed;
   begin
      openGL.Program.item (Self).define (use_vertex_Shader,
                                         use_fragment_Shader);   -- Define base class.

      for Each in Self.bone_transform_Uniforms'Range
      loop
         Self.bone_transform_Uniforms (Each).define (Self'Access,
                                                     "bone_Matrices[" & Trim (Integer'Image (Each - 1), Left) & "]");
      end loop;
   end define;



   overriding
   procedure set_Uniforms (Self : in Item)
   is
      the_inverse_modelview_matrix_Uniform : constant openGL.Variable.uniform.mat3
        := Self.uniform_Variable ("inv_modelview_Matrix");

      the_shine_Uniform : constant openGL.Variable.uniform.float
        := Self.uniform_Variable ("uShine");

   begin
      Self.set_mvp_Uniform;

      the_shine_Uniform.Value_is (Self.Shine);

      the_inverse_modelview_matrix_Uniform.Value_is (Self.inverse_modelview_Matrix);

      Light_1:
      declare
         use openGL.Conversions;

         the_Light : openGL.Light.directional.item renames Self.directional_Light (1);

         the_light_direction_Uniform          : constant openGL.Variable.uniform.vec3
           := Self.uniform_Variable ("uLight_1.direction");
         the_light_halfplane_Uniform          : constant openGL.Variable.uniform.vec3
           := Self.uniform_Variable ("uLight_1.halfplane");

         the_light_ambient_color_Uniform      : constant openGL.Variable.uniform.vec4
           := Self.uniform_Variable ("uLight_1.ambient_color");
         the_light_diffuse_color_Uniform      : constant openGL.Variable.uniform.vec4
           := Self.uniform_Variable ("uLight_1.diffuse_color");
         the_light_specular_color_Uniform     : constant openGL.Variable.uniform.vec4
           := Self.uniform_Variable ("uLight_1.specular_color");
      begin
         the_light_direction_Uniform         .Value_is (the_Light.Direction);
         the_light_halfplane_Uniform         .Value_is (the_Light.halfplane_Vector);

         the_light_ambient_color_Uniform     .Value_is (the_Light.ambient_Color);
         the_light_diffuse_color_Uniform     .Value_is (the_Light.diffuse_Color);
         the_light_specular_color_Uniform    .Value_is (the_Light.specular_Color);
      end Light_1;

      Light_2:
      declare
         use openGL.Conversions;

         the_Light : openGL.Light.directional.item renames Self.directional_Light (2);

         the_light_direction_Uniform          : constant openGL.Variable.uniform.vec3
           := Self.uniform_Variable ("uLight_2.direction");
         the_light_halfplane_Uniform          : constant openGL.Variable.uniform.vec3
           := Self.uniform_Variable ("uLight_2.halfplane");

         the_light_ambient_color_Uniform      : constant openGL.Variable.uniform.vec4
           := Self.uniform_Variable ("uLight_2.ambient_color");
         the_light_diffuse_color_Uniform      : constant openGL.Variable.uniform.vec4
           := Self.uniform_Variable ("uLight_2.diffuse_color");
         the_light_specular_color_Uniform     : constant openGL.Variable.uniform.vec4
           := Self.uniform_Variable ("uLight_2.specular_color");
      begin
         the_light_direction_Uniform         .Value_is (the_Light.Direction);
         the_light_halfplane_Uniform         .Value_is (the_Light.halfplane_Vector);

         the_light_ambient_color_Uniform     .Value_is (the_Light.ambient_Color);
         the_light_diffuse_color_Uniform     .Value_is (the_Light.diffuse_Color);
         the_light_specular_color_Uniform    .Value_is (the_Light.specular_Color);
      end Light_2;

      declare
         the_sampler_Uniform : constant openGL.Variable.uniform.int := Self.uniform_Variable ("sTexture");
      begin
         the_sampler_Uniform.Value_is (0);
      end;
   end set_Uniforms;



   procedure bone_Transform_is (Self : in Item;   Which : in Integer;
                                                  Now   : in Matrix_4x4)
   is
   begin
      Self.bone_transform_Uniforms (Which).Value_is (Now);
   end bone_Transform_is;


end openGL.Program.lit_textured_skinned;
