with
     gel.Window.setup,
     gel.Applet.gui_world,
     gel.Forge,
     gel.World,
     gel.Camera,

     Physics;

pragma Unreferenced (gel.Window.setup);


procedure launch_Pong_Tute
--
--  Basic pong game.
--
is
   use gel.Applet,
       gel.Applet.gui_world;

   --- Applet
   --
   the_Applet : gel.Applet.gui_world.view
     := gel.Forge.new_gui_Applet (Named         => "Pong Tutorial",
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

      the_Applet.World.evolve;     -- Advance the world.
      the_Applet.freshen;          -- Handle any new events and update the screen.
   end loop;

   free (the_Applet);
end launch_Pong_Tute;
