with
     openGL.Conversions,
     ada.Strings.fixed;


package body openGL.Program.lit_colored_textured
is

   overriding
   procedure set_Uniforms (Self : in Item)
   is
      use linear_Algebra_3d,
          openGL.Conversions;

      the_scale_Uniform                  : constant Variable.uniform.vec3 := Self.uniform_Variable ("Scale");
      the_light_count_Uniform            : constant Variable.uniform.int  := Self.uniform_Variable ("light_Count");
      the_specular_color_Uniform         : constant Variable.uniform.vec3 := Self.uniform_Variable ("specular_Color");
      the_camera_site_Uniform            : constant Variable.uniform.vec3 := Self.uniform_Variable ("camera_Site");
      the_model_transform_Uniform        : constant Variable.uniform.mat4 := Self.uniform_Variable ("model_Transform");
      the_inverse_model_rotation_Uniform : constant Variable.uniform.mat3 := Self.uniform_Variable ("inverse_model_Rotation");
   begin
      --  openGL.Program.item (Self).set_Uniforms;
      Self.set_mvp_Uniform;
      openGL.Program.item (Self).set_Uniforms;

      the_light_count_Uniform           .Value_is (1);
      the_specular_color_Uniform        .Value_is (to_Vector_3 (Self.specular_Color));
      the_scale_Uniform                 .Value_is (Self.Scale);
      the_camera_site_Uniform           .Value_is (Self.camera_Site);
      the_model_transform_Uniform       .Value_is (Self.model_Transform);
      the_inverse_model_rotation_Uniform.Value_is (Inverse (get_Rotation (Self.model_Transform)));
   end set_Uniforms;



   procedure specular_Color_is (Self : in out Item;   Now : in Color)
   is
   begin
      Self.specular_Color := Now;
   end specular_Color_is;


end openGL.Program.lit_colored_textured;
