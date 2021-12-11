private
with
     openGL.Palette;


package openGL.Program.lit
--
--  Provides a program for lit, colored and textured GL vertices.
--
is
   type Item is new openGL.Program.item with private;
   type View is access all Item'Class;

   overriding
   procedure set_Uniforms (Self : in Item);


   procedure specular_Color_is (Self : in out Item;   Now : in Color);


private

   type Item is new openGL.Program.item with
      record
         null;
--           specular_Color : Color := Palette.Grey;     -- The materials specular color.
      end record;

end openGL.Program.lit;
