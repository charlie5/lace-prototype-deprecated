with
     openGL.Tasks,
     openGL.Conversions,
     ada.Strings.fixed;


package body openGL.Program.lit_textured
is

   overriding
   procedure set_Uniforms (Self : in Item)
   is
      inverse_modelview_matrix_Uniform
                    : constant Variable.uniform.mat3  := Self.uniform_Variable ("inv_modelview_Matrix");
      scale_Uniform : constant Variable.uniform.vec3  := Self.uniform_Variable ("Scale");
      shine_Uniform : constant Variable.uniform.float := Self.uniform_Variable ("uShine");

   begin
      Tasks.check;

      --  openGL.Program.item (Self).set_Uniforms;
      Self.set_mvp_Uniform;

      scale_Uniform.Value_is (Self.Scale);
      shine_Uniform.Value_is (Self.Shine);

      inverse_modelview_matrix_Uniform.Value_is (Self.inverse_modelview_Matrix);

      -- Lights
      --
      for i in Self.directional_Light'Range
      loop
         declare
            Light : openGL.Light.directional.item renames Self.directional_Light (i);

            function light_Name return String
            is
               use ada.Strings,
                   ada.Strings.fixed;
            begin
               return "uLights[" & Trim (Integer'Image (i - 1), Left) & "]";
            end light_Name;

            light_on_Uniform : constant Variable.uniform.bool := Self.uniform_Variable (light_Name & ".is_on");
         begin
            light_on_Uniform.Value_is (Light.is_On);

            if Light.is_On
            then
               declare
                  use openGL.Conversions;

                  light_direction_Uniform      : constant Variable.uniform.vec3 := Self.uniform_Variable (light_Name & ".direction");
                  light_halfplane_Uniform      : constant Variable.uniform.vec3 := Self.uniform_Variable (light_Name & ".halfplane");

                  light_ambient_color_Uniform  : constant Variable.uniform.vec4 := Self.uniform_Variable (light_Name & ".ambient_color");
                  light_diffuse_color_Uniform  : constant Variable.uniform.vec4 := Self.uniform_Variable (light_Name & ".diffuse_color");
                  light_specular_color_Uniform : constant Variable.uniform.vec4 := Self.uniform_Variable (light_Name & ".specular_color");
               begin
                  light_direction_Uniform     .Value_is (Light.Direction);
                  light_halfplane_Uniform     .Value_is (Light.halfplane_Vector);

                  light_ambient_color_Uniform .Value_is (to_Vector_4 (Light.ambient_Color));
                  light_diffuse_color_Uniform .Value_is (to_Vector_4 (Light.diffuse_Color));
                  light_specular_color_Uniform.Value_is (to_Vector_4 (Light.specular_Color));
               end;
            end if;
         end;
      end loop;

   end set_Uniforms;


end openGL.Program.lit_textured;
