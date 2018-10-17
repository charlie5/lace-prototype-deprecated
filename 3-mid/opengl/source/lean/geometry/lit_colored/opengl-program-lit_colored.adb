with
     Ada.Strings.fixed;


package body openGL.Program.lit_colored
is

   overriding
   procedure set_Uniforms (Self : in Item)
   is
      the_inverse_modelview_matrix_Uniform : constant openGL.Variable.uniform.mat3
        := Self.uniform_Variable ("inv_modelview_Matrix");

      the_scale_Uniform : constant openGL.Variable.uniform.vec3
        := Self.uniform_Variable ("uScale");

      the_shine_Uniform : constant openGL.Variable.uniform.float
        := Self.uniform_Variable ("uShine");

   begin
      Self.set_mvp_Uniform;

      the_scale_Uniform.Value_is (Self.Scale);
      the_shine_Uniform.Value_is (Self.Shine);

      the_inverse_modelview_matrix_Uniform.Value_is (Self.inverse_modelview_Matrix);

      -- Lights
      --
      for i in Self.directional_Light'Range
      loop
         declare
            Light : openGL.Light.directional.item renames Self.directional_Light (i);

            function light_Name return String is
               use Ada.Strings, Ada.Strings.fixed;
            begin
               return "uLights[" & Trim (Integer'Image (i - 1), Left) & "]";
            end light_Name;

            the_light_on_Uniform : constant openGL.Variable.uniform.bool
              := Self.uniform_Variable (light_Name & ".is_on");
         begin
            the_light_on_Uniform.Value_is (Light.is_On);

            if Light.is_On
            then
               declare
                  the_light_direction_Uniform      : constant openGL.Variable.uniform.vec3
                    := Self.uniform_Variable (light_Name & ".direction");
                  the_light_halfplane_Uniform      : constant openGL.Variable.uniform.vec3
                    := Self.uniform_Variable (light_Name & ".halfplane");

                  the_light_ambient_color_Uniform  : constant openGL.Variable.uniform.vec4
                    := Self.uniform_Variable (light_Name & ".ambient_color");
                  the_light_diffuse_color_Uniform  : constant openGL.Variable.uniform.vec4
                    := Self.uniform_Variable (light_Name & ".diffuse_color");
                  the_light_specular_color_Uniform : constant openGL.Variable.uniform.vec4
                    := Self.uniform_Variable (light_Name & ".specular_color");
               begin
                  the_light_direction_Uniform     .Value_is (Light.Direction);
                  the_light_halfplane_Uniform     .Value_is (Light.halfplane_Vector);

                  the_light_ambient_color_Uniform .Value_is (Light.ambient_Color);
                  the_light_diffuse_color_Uniform .Value_is (Light.diffuse_Color);
                  the_light_specular_color_Uniform.Value_is (Light.specular_Color);
               end;
            end if;
         end;
      end loop;
   end set_Uniforms;


end openGL.Program.lit_colored;
