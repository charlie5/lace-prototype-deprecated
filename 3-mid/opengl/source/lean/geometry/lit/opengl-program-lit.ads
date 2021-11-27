package openGL.Program.lit
--
--  Provides a program for lit, colored and textured GL vertices.
--
is
   type Item is new openGL.Program.item with private;
   type View is access all Item'Class;

   overriding
   procedure set_Uniforms (Self : in Item);



private

   type Item is new openGL.Program.item with
      record
         null;
      end record;

end openGL.Program.lit;
