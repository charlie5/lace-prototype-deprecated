package body openGL.Program.colored
is

   overriding
   procedure set_Uniforms (Self : in Item)
   is
      the_scale_Uniform : constant Variable.uniform.vec3
        := Self.uniform_Variable ("uScale");
   begin
      Self.set_mvp_Uniform;
      the_scale_Uniform.Value_is (Self.Scale);
   end set_Uniforms;

end openGL.Program.colored;
