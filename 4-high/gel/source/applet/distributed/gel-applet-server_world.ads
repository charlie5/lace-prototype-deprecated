with
     gel.Camera,
     gel.World.server,
     gel.Window;


package gel.Applet.server_world
--
--  Provides a gel applet configured with a single window and a single server world.
--
is
   type Item is new gel.Applet.item with private;
   type View is access all Item'Class;


   package Forge
   is
      function new_Applet (Name       : in String;
                           use_Window : in gel.Window.view;
                           space_Kind : in physics.space_Kind) return gel.Applet.server_world.view;
   end Forge;

   procedure free (Self : in out View);


   server_world_Id  : constant  world_Id := 1;
   server_camera_Id : constant camera_Id := 1;

   function server_World  (Self : in Item) return gel.World.server.view;
   function server_Camera (Self : in Item) return gel.Camera.view;



private

   type Item is new gel.Applet.item with
      record
         null;
      end record;

end gel.Applet.server_world;
