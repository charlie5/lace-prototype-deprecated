with
     openGL.Visual,
     openGL.Model.arrow.colored,
     openGL.Demo,

     Ada.Text_IO,
     Ada.Exceptions;


procedure launch_render_Arrows
--
--  Exercise the render of arrow models.
--
is
   use openGL,
       openGL.Model,
       openGL.Math,
       openGL.linear_Algebra_3d,
       ada.Text_IO;
begin
   Demo.define ("openGL 'render Arrows' Demo");
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));
   declare
      --  The Models.
      --
      the_arrow_Model         : constant openGL.Model.arrow.colored.view
        := openGL.Model.arrow.colored.Forge.new_Arrow (End_2 => (0.0, 5.0, 0.0));

      the_spinner_arrow_Model : constant openGL.Model.arrow.colored.view
        := openGL.Model.arrow.colored.Forge.new_Arrow (End_1 => (0.0, -2.5, 0.0),
                                                       End_2 => (0.0,  2.5, 0.0));
      --  The Sprites.
      --
      use openGL.Visual.Forge;

      the_Sprites : constant openGL.Visual.views := (new_Visual (        the_arrow_Model.all'Access),
                                                     new_Visual (the_spinner_arrow_Model.all'Access));
      Angle : Radians := 0.0;

   begin
      --  Main loop.
      --
      while not Demo.Done
      loop
         the_arrow_Model.Site_is (Now     => math.Vector_3 (Geometry_2d.to_Site (Geometry_2d.polar_Site' (angle  => Angle,
                                                                                                          extent => 5.0)) & 0.0),
                                  for_End => 2);

         the_Sprites (2).Spin_is (linear_Algebra_3d.to_Rotation (axis  => (0.0, 0.0, 1.0),
                                                                 angle => Angle));

         -- Handle user commands.
         --
         Demo.Dolly.evolve;
         Demo.Done := Demo.Dolly.quit_Requested;

         --  Render the sprites.
         --
         Demo.Camera.render (the_Sprites);

         while not Demo.Camera.cull_Completed
         loop
            delay Duration'Small;
         end loop;

         Demo.Renderer.render;
         Demo.FPS_Counter.increment;    -- Frames per second display.

         Angle := Angle + 0.001;

         if Angle >= to_Radians (Degrees' (360.0))
         then
            Angle := 0.0;
         end if;
      end loop;
   end;

   Demo.destroy;
   new_Line;

exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main thread !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_render_Arrows;
