with
     mmi.World,
     mmi.Camera,
     mmi.Window;


package mmi.Applet.sim_2d_world
--
--  Provides a 'ready' mmi system, configured with a single window and a single 2d world.
--
is

   type Item is new mmi.Applet.item with private;
   type View is access all Item'Class;


   package Forge
   is
      function new_Applet (Name       : in String;
                           use_Window : in mmi.Window.view) return View;
   end Forge;


   function sim_World  (Self : in Item) return mmi.World .view;
   function sim_Camera (Self : in Item) return mmi.Camera.view;



private

   type Item is new mmi.Applet.item with
      record
         null;
      end record;

end mmi.Applet.sim_2d_world;
