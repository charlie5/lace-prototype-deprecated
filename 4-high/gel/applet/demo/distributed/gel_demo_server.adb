with
     gel.Applet,
     gel.World.server,
     gel.Camera,
     gel.Forge,
     gel.Sprite,
     
     openGL.Model,
     opengl.Palette,
     
     physics.Forge,
     float_Math,
     
     ada.Calendar,
     ada.Text_IO,
     ada.Exceptions;


package body gel_demo_Server
is
   use ada.Calendar,
       ada.Text_IO,
       ada.Exceptions;


   package Math renames float_Math;


   task body Item
   is
      use gel.Applet;
      use type math.Real;

      the_World : gel.World.server.view;

   begin
      accept start
      do
         the_World        := gel.World.server.forge.new_World ("Server", 1, physics.Bullet, null);
         the_server_World := the_World.all'Access;
      end;
      
      the_World.start;

      declare
--           use gel.box_Model, openGL, opengl.Palette, Math;
         use type math.Real, math.Index;

         the_Box          : gel.Sprite.view := gel.Forge. new_box_Sprite (the_World.all'Access,
                                                                          Site => math.Origin_3D,
                                                                          Size => (20.0, 1.0, 20.0),
                                                                          Mass => 0.0);

         --  the_Ball          : gel.Sprite.view := gel.Forge. new_box_Sprite (the_World.all'Access,
         --                                                                   Site => math.Origin_3D,
         --                                                                   Size => (20.0, 1.0, 20.0),
         --                                                                   Mass => 1.0);

         the_Ball         : gel.Sprite.view := gel.Forge.new_ball_Sprite (the_World.all'Access,
                                                                          Mass => 1.0);

         next_render_Time : ada.calendar.Time;
         Counter          : Natural          := 0;
      begin
         --- Setup.
         --
         the_World.Gravity_is ((0.0, -10.0, 0.0));

         the_World.add     (the_Ball, and_Children => False);
         the_Ball .Site_is ((0.0,  10.0,  0.0));
         the_Ball.Solid.activate;
         
         --  the_World.add     (the_Box,  and_Children => False);
         --  the_Box  .Site_is ((0.0,  -1.0,  0.0));


         --- Begin processing.
         --
         next_render_Time := ada.Calendar.clock;

         delay 1.0;
         
         while True
         loop
            the_World.evolve (by => 1.0/60.0);
            --  the_World.wait_on_evolve;

            Counter := Counter + 1;

            --  if Counter = 5 * 60
            --  then
            --     Counter := 0;
            --     the_Ball.Site_is  ((0.0,  25.0,  0.0));
            --  end if;

            --  put_Line ("Ball: " & math.Image (the_Ball.Site));

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


end gel_demo_Server;
