package body openGL.Program.colored_textured
is

   overriding
   procedure set_Uniforms (Self : in Item)
   is
   begin
      Self.set_mvp_Uniform;
   end set_Uniforms;

end openGL.Program.colored_textured;
