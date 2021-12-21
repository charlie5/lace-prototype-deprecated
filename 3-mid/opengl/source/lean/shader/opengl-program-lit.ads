with
     openGL.Palette,
     openGL.Light;


package openGL.Program.lit
--
--  Models an openGL program which uses lighting.
--
is
   type Item is new openGL.Program.item with private;
   type View is access all Item'Class;


   ------------
   --  Uniforms
   --

   overriding
   procedure Lights_are   (Self : in out Item;   Now : in Light.items);

   overriding
   procedure set_Uniforms (Self : in     Item);

   procedure specular_Color_is (Self : in out Item;   Now : in Color);



private

   type Item is new openGL.Program.item with
      record
         Lights         : Light.items (1 .. 50);
         light_Count    : Natural := 0;
         specular_Color : Color   := Palette.Grey;     -- The materials specular color.
      end record;


end openGL.Program.lit;
