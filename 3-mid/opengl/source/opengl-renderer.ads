package openGL.Renderer
--
-- Provides a base class for all renderers.
--
is

   type Item is abstract tagged limited private;
   type View is access all Item'Class;


   --  Attributes
   --
   procedure Background_is (Self : in out Item;   Now : in openGL.Color);


   --  Operations
   --
   procedure clear_Frame (Self : in Item);




private

   type Item is abstract tagged limited
      record
         Background : openGL.Color;
      end record;

end openGL.Renderer;
