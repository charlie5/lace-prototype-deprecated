with
     mmi.Window.setup,
     mmi.Applet.gui_world,
     mmi.Forge,
     mmi.World,
     mmi.Camera,

     Physics;

pragma Unreferenced (mmi.Window.setup);


procedure launch_Pong_Tute
--
--  Basic pong game.
--
is
   use mmi.Applet,
       mmi.Applet.gui_world,
       mmi.Math;

   --- Applet
   --
   the_Applet : mmi.Applet.gui_world.view
     := mmi.Forge.new_gui_Applet (Named         => "Pong",
                                  window_Width  => 800,
                                  window_Height => 600,
                                  space_Kind    => physics.Box2d);
   --- Controls
   --
   Cycle : Natural := 0;

begin
   the_Applet.Camera.Site_is   ((0.0, 0.0, 20.0));

   --- Main loop.
   --
   while the_Applet.is_open
   loop
      Cycle := Cycle + 1;

      the_Applet.World.evolve (by => 1.0 / 60.0);     -- Advance the world by 1/60th of a second.
      the_Applet.freshen;                             -- Handle any new events and update the screen.
   end loop;

   free (the_Applet);
end launch_Pong_Tute;
