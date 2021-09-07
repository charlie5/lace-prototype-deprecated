package openGL.Program.lit_textured
--
--  Provides a program for lit and textured vertices.
--
is
   type Item is new openGL.Program.item with null record;
   type View is access all Item'Class;

   overriding
   procedure set_Uniforms (Self : in Item);

end openGL.Program.lit_textured;
