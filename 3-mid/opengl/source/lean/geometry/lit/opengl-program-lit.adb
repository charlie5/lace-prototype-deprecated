with
     ada.Strings.fixed;


package body openGL.Program.lit
is

   overriding
   procedure set_Uniforms (Self : in Item)
   is
      use linear_Algebra_3d;

      the_scale_Uniform                  : constant Variable.uniform.vec3 := Self.uniform_Variable ("Scale");
      the_num_lights_Uniform             : constant Variable.uniform.int  := Self.uniform_Variable ("numLights");
      the_camera_position_Uniform        : constant Variable.uniform.vec3 := Self.uniform_Variable ("cameraPosition");
      the_model_matrix_Uniform           : constant Variable.uniform.mat4 := Self.uniform_Variable ("model_Matrix");
      the_inverse_model_rotation_Uniform : constant Variable.uniform.mat3 := Self.uniform_Variable ("inverse_model_Rotation");
   begin
      --  openGL.Program.item (Self).set_Uniforms;
      Self.set_mvp_Uniform;


      the_num_lights_Uniform            .Value_is (1);
      the_scale_Uniform                 .Value_is (Self.Scale);
      the_camera_position_Uniform       .Value_is (Self.camera_Position);
      the_model_matrix_Uniform          .Value_is (Self.model_Matrix);
      the_inverse_model_rotation_Uniform.Value_is (Inverse (get_Rotation (Self.model_Matrix)));

      -- Lights.
      --
      for i in Self.diffuse_Lights'Range
      loop
         declare
            Light : openGL.Light.diffuse.item renames Self.diffuse_Lights (i);

            function light_Name return String is
               use ada.Strings,
                   ada.Strings.fixed;
            begin
               return "uLights[" & Trim (Integer'Image (i - 1), Left) & "]";
            end light_Name;

            site_Uniform                : constant Variable.uniform.vec4  := Self.uniform_Variable (light_Name & ".Site");
            color_Uniform               : constant Variable.uniform.vec3  := Self.uniform_Variable (light_Name & ".Color");
            attenuation_Uniform         : constant Variable.uniform.float := Self.uniform_Variable (light_Name & ".Attenuation");
            ambient_coefficient_Uniform : constant Variable.uniform.float := Self.uniform_Variable (light_Name & ".ambient_Coefficient");
            cone_angle_Uniform          : constant Variable.uniform.float := Self.uniform_Variable (light_Name & ".cone_Angle");
            cone_direction_Uniform      : constant Variable.uniform.vec3  := Self.uniform_Variable (light_Name & ".cone_Direction");
         begin
            site_Uniform               .Value_is (Vector_4 (Light.Position & 1.0));
            color_Uniform              .Value_is (          Light.Intensities);
            attenuation_Uniform        .Value_is (          Light.Attenuation);
            ambient_coefficient_Uniform.Value_is (          Light.ambient_Coefficient);
            cone_angle_Uniform         .Value_is (Real     (Light.cone_Angle));
            cone_direction_Uniform     .Value_is (          Light.cone_Direction);
         end;
      end loop;
   end set_Uniforms;


end openGL.Program.lit;
