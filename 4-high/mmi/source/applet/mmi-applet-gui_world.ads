with
     mmi.World,
     mmi.Camera,
     mmi.Window,

     physics.Forge;


package mmi.Applet.gui_world
--
--  Provides a 'ready' mmi system, configured with a single window and a single GUI world.
--
is

   type Item is new mmi.Applet.item with private;
   type View is access all Item'Class;


   package Forge
   is
      function new_Applet (Name       : in String;
                           use_Window : in mmi.Window.view;
                           space_Kind : in physics.Forge.space_Kind) return mmi.Applet.gui_world.view;
   end Forge;

   procedure free (Self : in out View);


   gui_world_Id  : constant  world_Id := 1;
   gui_camera_Id : constant camera_Id := 1;

   function gui_World  (Self : in Item) return mmi.World .view;
   function gui_Camera (Self : in Item) return mmi.Camera.view;



private

   type Item is new mmi.Applet.item with
      record
         null;
      end record;

end mmi.Applet.gui_world;
