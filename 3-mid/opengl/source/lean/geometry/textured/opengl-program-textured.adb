package body openGL.Program.textured
is

   overriding
   procedure set_Uniforms (Self : in Item)
   is
      scale_Uniform : constant Variable.uniform.vec3 := Self.uniform_Variable ("Scale");
   begin
      Self.set_mvp_Uniform;
      scale_Uniform.Value_is (Self.Scale);
   end set_Uniforms;

end openGL.Program.textured;
