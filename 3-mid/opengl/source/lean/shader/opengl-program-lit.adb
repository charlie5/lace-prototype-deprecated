with
     openGL.Conversions,
     ada.Strings.fixed;


package body openGL.Program.lit
is

   overriding
   procedure Lights_are (Self : in out Item;   Now : in Light.items)
   is
   begin
      Self.light_Count              := Now'Length;
      Self.Lights (1 .. Now'Length) := Now;
   end Lights_are;



   overriding
   procedure set_Uniforms (Self : in Item)
   is
      use openGL.Conversions;

      the_light_count_Uniform    : constant Variable.uniform.int  := Self.uniform_Variable ("light_Count");
      the_specular_color_Uniform : constant Variable.uniform.vec3 := Self.uniform_Variable ("specular_Color");
   begin
      openGL.Program.item (Self).set_Uniforms;
      Self.set_mvp_Uniform;

      -- Lights.
      --
      the_light_count_Uniform   .Value_is (Self.light_Count);
      the_specular_color_Uniform.Value_is (to_Vector_3 (Self.specular_Color));

      for i in 1 .. Self.light_Count
      loop
         declare
            use Light;

            Light : openGL.Light.item renames Self.Lights (i);

            function light_Name return String
            is
               use ada.Strings,
                   ada.Strings.fixed;
            begin
               return "Lights[" & Trim (Integer'Image (i - 1), Left) & "]";
            end light_Name;

            site_Uniform                : constant Variable.uniform.vec4  := Self.uniform_Variable (light_Name & ".Site");
            color_Uniform               : constant Variable.uniform.vec3  := Self.uniform_Variable (light_Name & ".Color");
            attenuation_Uniform         : constant Variable.uniform.float := Self.uniform_Variable (light_Name & ".Attenuation");
            ambient_coefficient_Uniform : constant Variable.uniform.float := Self.uniform_Variable (light_Name & ".ambient_Coefficient");
            cone_angle_Uniform          : constant Variable.uniform.float := Self.uniform_Variable (light_Name & ".cone_Angle");
            cone_direction_Uniform      : constant Variable.uniform.vec3  := Self.uniform_Variable (light_Name & ".cone_Direction");
         begin
            case Light.Kind
            is
            when Diffuse =>   site_Uniform.Value_is (Vector_4 (Light.Site & 1.0));
            when Direct  =>   site_Uniform.Value_is (Vector_4 (Light.Site & 0.0));
            end case;

            color_Uniform              .Value_is (to_Vector_3 (Light.Color));
            attenuation_Uniform        .Value_is (             Light.Attenuation);
            ambient_coefficient_Uniform.Value_is (             Light.ambient_Coefficient);
            cone_angle_Uniform         .Value_is (Real        (Light.cone_Angle));
            cone_direction_Uniform     .Value_is (             Light.cone_Direction);
         end;
      end loop;
   end set_Uniforms;



   procedure specular_Color_is (Self : in out Item;   Now : in Color)
   is
   begin
      Self.specular_Color := Now;
   end specular_Color_is;



end openGL.Program.lit;
