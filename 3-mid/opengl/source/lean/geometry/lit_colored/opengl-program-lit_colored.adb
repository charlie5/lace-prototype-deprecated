with
     openGL.Conversions,
     ada.Strings.fixed;


package body openGL.Program.lit_colored
is
   overriding
   procedure set_Uniforms (Self : in Item)
   is
      the_scale_Uniform : constant Variable.uniform.vec3
        := Self.uniform_Variable ("Scale");

   begin
      Self.set_mvp_Uniform;
      openGL.Program.item (Self).set_Uniforms;

      the_scale_Uniform.Value_is (Self.Scale);
   end set_Uniforms;


end openGL.Program.lit_colored;
