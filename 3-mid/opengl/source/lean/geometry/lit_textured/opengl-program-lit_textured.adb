with
     openGL.Tasks,
     openGL.Conversions,
     ada.Strings.fixed;


package body openGL.Program.lit_textured
is

   overriding
   procedure set_Uniforms (Self : in Item)
   is
      scale_Uniform : constant Variable.uniform.vec3  := Self.uniform_Variable ("Scale");
   begin
      Tasks.check;

      openGL.Program.item (Self).set_Uniforms;
      Self.set_mvp_Uniform;
      scale_Uniform.Value_is (Self.Scale);

   end set_Uniforms;


end openGL.Program.lit_textured;
