with
     openGL.Conversions;


package body openGL.Program.lit_colored
is

   overriding
   procedure set_Uniforms (Self : in Item)
   is
      the_inverse_modelview_matrix_Uniform : constant openGL.Variable.uniform.mat3
        := Self.uniform_Variable ("inv_modelview_Matrix");

      the_scale_Uniform : constant openGL.Variable.uniform.vec3
        := Self.uniform_Variable ("uScale");

   begin
      Self.set_mvp_Uniform;
      the_scale_Uniform.Value_is (Self.Scale);

      the_inverse_modelview_matrix_Uniform.Value_is (Self.inverse_modelview_Matrix);

      Light_1:
      declare
         use openGL.Conversions;

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

         the_Light : openGL.Light.directional.item  renames Self.directional_Light (1);
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

         the_Light : openGL.Light.directional.item  renames Self.directional_Light (2);
      begin
         the_light_direction_Uniform         .Value_is (the_Light.Direction);
         the_light_halfplane_Uniform         .Value_is (the_Light.halfplane_Vector);

         the_light_ambient_color_Uniform     .Value_is (the_Light.ambient_Color);
         the_light_diffuse_color_Uniform     .Value_is (the_Light.diffuse_Color);
         the_light_specular_color_Uniform    .Value_is (the_Light.specular_Color);
      end Light_2;
   end set_Uniforms;


end openGL.Program.lit_colored;
