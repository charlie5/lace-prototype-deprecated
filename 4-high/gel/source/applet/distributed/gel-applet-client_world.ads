with
     gel.World.client,
     gel.Camera,
     gel.Window;

package gel.Applet.client_world
--
--  Provides a gel applet configured with a single window and a single client world.
--
is
   type Item is new gel.Applet.item with private;
   type View is access all Item'Class;


   package Forge
   is
      function new_Applet (Name       : in String;
                           use_Window : in gel.Window.view;
                           space_Kind : in physics.space_Kind) return gel.Applet.client_world.view;
   end Forge;

   procedure free (Self : in out View);


   client_world_Id  : constant  world_Id := 1;
   client_camera_Id : constant camera_Id := 1;

   function client_World  (Self : in Item) return gel.World.client.view;
   function client_Camera (Self : in Item) return gel.Camera      .view;



private

   type Item is new gel.Applet.item with
      record
         null;
      end record;

end gel.Applet.client_world;
