with
     gel.Forge,
     gel.Sprite,

     Physics,

     float_Math,

     ada.Calendar,
     ada.Text_IO,
     ada.Exceptions;


package body gel_demo_Server
is
   use ada.Calendar,
       ada.Text_IO;


   package Math renames float_Math;


   task body Item
   is
      the_World : gel.World.server.view;

   begin
      accept start
      do
         the_World        := gel.World.server.forge.new_World ("Server", 1, physics.Bullet, null);
         the_server_World := the_World.all'Access;
      end start;

      the_World.start;

      declare
         --  use type math.Real;

         the_Box  : constant gel.Sprite.view := gel.Forge. new_box_Sprite (the_World.all'Access,
                                                                           Site => math.Origin_3D,
                                                                           Size => (20.0, 1.0, 20.0),
                                                                           Mass => 0.0);

         the_Ball : constant gel.Sprite.view := gel.Forge.new_ball_Sprite (the_World.all'Access,
                                                                           Mass => 1.0);
         next_render_Time : ada.calendar.Time;
         Counter          : Natural := 0;
         Done             : Boolean := False;
      begin
         --- Setup.
         --
         the_World.Gravity_is ((0.0, -10.0, 0.0));

         the_World.add     (the_Ball, and_Children => False);
         the_Ball .Site_is ((0.0, 10.0, 0.0));
         the_Ball.Solid.activate;

         the_World.add   (the_Box, and_Children => False);
         the_Box.Site_is ((0.0, -1.0, 0.0));


         --- Begin processing.
         --
         next_render_Time := ada.Calendar.clock;

         delay 1.0;

         while not Done
         loop
            select
               accept stop
               do
                  Done := True;
               end stop;

            else
               null;
            end select;

            the_World.evolve;

            Counter := Counter + 1;

            if Counter = 5 * 60
            then
               Counter := 0;
               the_Ball.Site_is  ((0.0,  25.0,  0.0));
            end if;

            next_render_Time := next_render_Time + gel.World.evolve_Period;
            delay until next_render_Time;
         end loop;


         --- Close
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
