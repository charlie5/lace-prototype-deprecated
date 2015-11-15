with
     mmi.World,
     mmi.Camera,
     mmi.Window;


package mmi.Applet.gui_and_sim_world
--
-- Provides a 'ready' mmi system, configured with a single window and
-- two worlds (generally a simulation world and a gui world).
--
is

   type Item is limited new mmi.Applet.item with private;
   type View is access all Item'Class;



   package Forge
   is
      function  to_Applet (Name       : in String;
                           use_Window : in mmi.Window.view) return Item;
      function new_Applet (Name       : in String;
                           use_Window : in mmi.Window.view) return View;
   end Forge;


   gui_world_Id  : constant mmi. world_Id := 1;
   gui_camera_Id : constant mmi.camera_Id := 1;

   sim_world_Id  : constant mmi. world_Id := 2;
   sim_camera_Id : constant mmi.camera_Id := 1;



   function gui_World  (Self : in Item) return mmi.World .view;
   function gui_Camera (Self : in Item) return mmi.Camera.view;

   function sim_World  (Self : in Item) return mmi.World .view;
   function sim_Camera (Self : in Item) return mmi.Camera.view;



private

   type Item is limited new mmi.Applet.item with
      record
         null;
      end record;

end mmi.Applet.gui_and_sim_world;
