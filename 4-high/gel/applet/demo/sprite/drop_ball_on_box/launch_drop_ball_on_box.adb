with
     mmi.Window.setup,
     mmi.Applet.gui_world,
     mmi.Forge,
     mmi.Sprite,
     mmi.World,
     mmi.Camera,

     Physics,

     Ada.Text_IO,
     Ada.Exceptions;

pragma Unreferenced (mmi.Window.setup);


procedure launch_drop_Ball_on_Box
--
--  Drops a ball onto a box 'terrain'.
--
is
   use mmi.Applet,
       mmi.Applet.gui_world,
       Ada.Text_IO;

   the_Applet :  mmi.Applet.gui_world.view := mmi.Forge.new_gui_Applet  ("drop Ball on Box",
                                                                         space_Kind => physics.Bullet);

   function gui_World return mmi.World.view
   is
   begin
      return the_Applet.World (mmi.Applet.gui_world.gui_world_Id);
   end gui_World;


   function gui_Camera return mmi.Camera.view
   is
   begin
      return the_Applet.Camera (mmi.Applet.gui_world.gui_world_Id,
                                mmi.Applet.gui_world.gui_camera_Id);
   end gui_Camera;


   the_Ball   : constant mmi.Sprite.view     := mmi.Forge.new_ball_Sprite (gui_World);
   the_Box    : constant mmi.Sprite.view     := mmi.Forge.new_box_Sprite  (gui_World,
                                                                           mass => 0.0,
                                                                           size => (20.0, 1.0, 20.0));
begin
   gui_Camera.Site_is ((0.0, 2.0, 20.0));      -- Position the camera.

   the_Applet.enable_simple_Dolly (1);         -- Enable user camera control via keyboards.

   gui_World.Gravity_is ((0.0, -9.8, 0.0));

   gui_World.add (the_Box);                    -- Add box.
   gui_World.add (the_Ball);                   -- Add ball.
--
   the_Ball.Site_is ((0.0, 10.0, 0.0));

   while the_Applet.is_open
   loop
      gui_World.evolve (by => 1.0 / 60.0);
      the_Applet.freshen;                      -- Handle any new events and update the screen.
   end loop;

   free (the_Applet);

exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main task !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_drop_Ball_on_Box;
