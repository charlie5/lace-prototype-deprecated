with
     ada.Streams.stream_IO;

with gel.Forge,
     gel.Window.sdl,
     gel.Applet.gui_world,
     gel.Camera;

with gel.Sprite,
     openGL.Model.box.lit_colored_textured,
     openGL.Model.sphere;

with opengl.Palette,
     float_Math,
     ada.Calendar;


procedure launch_add_rid_sprite_Test
--
-- drops a ball onto a box terrain.
--
--
is
   use gel.Applet,
       openGL,
       opengl.Palette,
       ada.Calendar;

   package Math renames float_Math;

   use type math.Real;

   the_Applet : constant gel.Applet.gui_world.view
     := gel.forge.new_gui_Applet ("memory leak Test", 500, 500); --the_Window);

   the_Box : constant gel.Sprite.view
     := gel.forge.new_box_Sprite (the_Applet.gui_World, mass => 0.0);

   the_Balls : gel.Sprite.views (1 .. 1)
     := (others => gel.forge.new_ball_Sprite (the_Applet.gui_World, mass => 1.0));

   next_render_Time : ada.calendar.Time;

begin
   the_Applet.gui_Camera.Site_is ((0.0, 5.0, 15.0));     -- Position the camera
   the_Applet.enable_simple_Dolly (1);                   -- Enable user camera control via keyboards
   the_Applet.enable_Mouse (detect_Motion => False);     -- Enable mouse events.


   the_Applet.gui_World.add (the_Box);                   -- Add the ground box.
   the_Box.Site_is ((0.0,  0.0,  0.0));


   for Each in the_Balls'range
   loop
      the_Applet.gui_World.add (the_Balls (Each));       -- Add ball.
      the_Balls (Each).Site_is ((0.0,  10.0,  0.0));
   end loop;

   for Each in 1 .. 100
   loop
      the_Applet.gui_World.evolve (by => 1.0 / 60.0);    -- Evolve the world.
      the_Applet.freshen;                                -- Handle any new events and update the screen.
   end loop;

   for Each in the_Balls'range
   loop
      the_Applet.gui_World.rid (the_Balls (Each));        -- Rid ball.
      gel.Sprite.free (the_Balls (Each));
   end loop;



   next_render_Time := ada.Calendar.clock;

   while the_Applet.is_open
   loop
      the_Applet.gui_World.evolve (by => 1.0/60.0);       -- Evolve the world.
      the_Applet.freshen;                                 -- Handle any new events and update the screen.

      next_render_Time := next_render_Time + 1.0/60.0;
      delay until next_render_Time;
   end loop;


   the_Applet.destroy;

end launch_add_rid_sprite_Test;
