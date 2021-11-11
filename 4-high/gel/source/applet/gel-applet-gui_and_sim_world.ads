with
     gel.World,
     gel.Camera,
     gel.Window;


package gel.Applet.gui_and_sim_world
--
-- Provides an applet configured with a single window and
-- two worlds (generally a simulation world and a gui world).
--
is
   type Item is limited new gel.Applet.item with private;
   type View is access all Item'Class;


   package Forge
   is
      function  to_Applet (Name       : in String;
                           use_Window : in gel.Window.view) return Item;
      function new_Applet (Name       : in String;
                           use_Window : in gel.Window.view) return View;
   end Forge;


   gui_world_Id  : constant gel. world_Id := 1;
   gui_camera_Id : constant gel.camera_Id := 1;

   sim_world_Id  : constant gel. world_Id := 2;
   sim_camera_Id : constant gel.camera_Id := 1;


   function gui_World  (Self : in Item) return gel.World .view;
   function gui_Camera (Self : in Item) return gel.Camera.view;

   function sim_World  (Self : in Item) return gel.World .view;
   function sim_Camera (Self : in Item) return gel.Camera.view;



private

   type Item is limited new gel.Applet.item with
      record
         null;
      end record;

end gel.Applet.gui_and_sim_world;
