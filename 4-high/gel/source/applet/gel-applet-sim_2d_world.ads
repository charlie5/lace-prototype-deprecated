with
     gel.World,
     gel.Camera,
     gel.Window;


package gel.Applet.sim_2d_world
--
--  Provides a 'ready' gel system, configured with a single window and a single 2d world.
--
is

   type Item is new gel.Applet.item with private;
   type View is access all Item'Class;


   package Forge
   is
      function new_Applet (Name       : in String;
                           use_Window : in gel.Window.view) return View;
   end Forge;


   function sim_World  (Self : in Item) return gel.World .view;
   function sim_Camera (Self : in Item) return gel.Camera.view;



private

   type Item is new gel.Applet.item with
      record
         null;
      end record;

end gel.Applet.sim_2d_world;
