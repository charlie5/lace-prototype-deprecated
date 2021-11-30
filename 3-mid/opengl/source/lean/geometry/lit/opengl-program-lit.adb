with
     ada.Strings.fixed;


package body openGL.Program.lit
is

   overriding
   procedure set_Uniforms (Self : in Item)
   is
      the_scale_Uniform : constant Variable.uniform.vec3 := Self.uniform_Variable ("uScale");
      the_num_lights_Uniform :  constant Variable.uniform.int := Self.uniform_Variable ("numLights");
      the_camera_position_Uniform :  constant Variable.uniform.vec3 := Self.uniform_Variable ("cameraPosition");
      the_model_Uniform : constant Variable.uniform.mat4  := Self.uniform_Variable ("model");
   begin
      --  openGL.Program.item (Self).set_Uniforms;
      Self.set_mvp_Uniform;


      the_num_lights_Uniform     .Value_is (1);
      the_scale_Uniform          .Value_is (Self.Scale);
      the_camera_position_Uniform.Value_is (Self.camera_Position);
      the_model_Uniform          .Value_is (Transpose (Self.model_Matrix));
      --  the_model_Uniform          .Value_is (Self.model_Matrix));

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

            the_light_on_Uniform : constant Variable.uniform.bool := Self.uniform_Variable (light_Name & ".is_on");
         begin
            the_light_on_Uniform.Value_is (Light.is_On);

            if Light.is_On
            then
               declare
                  position_Uniform            : constant Variable.uniform.vec4  := Self.uniform_Variable (light_Name & ".position");
                  intensities_Uniform         : constant Variable.uniform.vec3  := Self.uniform_Variable (light_Name & ".intensities");
                  attenuation_Uniform         : constant Variable.uniform.float := Self.uniform_Variable (light_Name & ".attenuation");
                  ambient_Coefficient_Uniform : constant Variable.uniform.float := Self.uniform_Variable (light_Name & ".ambientCoefficient");
                  cone_angle_Uniform          : constant Variable.uniform.float := Self.uniform_Variable (light_Name & ".coneAngle");
                  cone_direction_Uniform      : constant Variable.uniform.vec3  := Self.uniform_Variable (light_Name & ".coneDirection");
               begin
                  position_Uniform           .Value_is (Light.Position);
                  intensities_Uniform        .Value_is (Light.Intensities);
                  attenuation_Uniform        .Value_is (Light.Attenuation);
                  ambient_Coefficient_Uniform.Value_is (Light.ambient_Coefficient);
                  cone_angle_Uniform         .Value_is (Real (Light.cone_Angle));
                  cone_direction_Uniform     .Value_is (Light.cone_Direction);
               end;
            end if;
         end;
      end loop;
   end set_Uniforms;


end openGL.Program.lit;
