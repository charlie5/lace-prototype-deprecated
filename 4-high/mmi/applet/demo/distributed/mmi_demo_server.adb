with
     mmi.Applet,
     mmi.World,
     mmi.Camera,
     mmi.Forge,
     mmi.Sprite,
     openGL.Model;

with opengl.Palette,
     physics.Forge,
     float_Math,
     ada.Calendar;

with ada.Calendar;            use ada.Calendar;
with ada.text_IO;             use ada.text_IO;
with ada.Exceptions;



package body mmi_demo_Server
is
   package Math renames float_Math;


   task body Item
   is
      use mmi.Applet;
      use type math.Real;

      the_World : mmi.World.view;

   begin
      accept start
      do
         the_World        := mmi.World.forge.new_World ("server", 1, physics.Bullet, null);
         the_server_World := the_World.all'Access;
      end;
      the_World.start;

      declare
--           use mmi.box_Model, openGL, opengl.Palette, Math;
         use type math.Real, math.Index;

         the_Box          : mmi.Sprite.view := mmi.Forge.new_box_Sprite (the_World,
                                                                         size => (20.0, 1.0, 20.0),
                                                                         mass => 0.0);
         the_Ball         : mmi.Sprite.view := mmi.Forge.new_ball_Sprite (the_World,
                                                                          mass => 1.0);

         next_render_Time : ada.calendar.Time;
         Counter          : Natural          := 0;
      begin
         --- setup
         --
         the_World.Gravity_is ((0.0, -10.0, 0.0));

         the_World.add     (the_Ball, and_Children => False);
         the_Ball .Site_is ((0.0,  10.0,  0.0));

         the_World.add     (the_Box,  and_Children => False);
         the_Box  .Site_is ((0.0,  -1.0,  0.0));


         --- begin processing
         --
         next_render_Time := ada.Calendar.clock;

         while True
         loop
            the_World.evolve (by => 1.0/60.0);
            the_World.wait_on_evolve;

            Counter := Counter + 1;

            if Counter = 5 * 60 
            then
               Counter := 0;
               the_Ball.Site_is  ((0.0,  25.0,  0.0));
            end if;

--              put_Line ("Ball: " & math.Image (the_Ball.Site));

            next_render_Time := next_render_Time + 1.0/60.0;
            delay until next_render_Time;
         end loop;


         --- close
         --
         the_World.destroy;
      end;


   exception
      when E : others =>
         put_Line ("Server unhandled exception ...");
         put_Line (ada.exceptions.Exception_Information (E));
         put_Line ("Server has terminated !");
   end Item;


end mmi_demo_Server;
