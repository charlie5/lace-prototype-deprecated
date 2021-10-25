with
     openGL.Visual,
     openGL.Model.Arrow.colored,
     openGL.Demo;


procedure launch_render_Arrows
--
--  Exercise the render of arrow models.
--
is
   use openGL,
       openGL.Model,
       openGL.Math,
       openGL.linear_Algebra_3d;
begin
   Demo.print_Usage;
   Demo.define ("openGL 'Render Arrows' Demo");
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));
   declare
      --  The Models.
      --
      the_Arrow_Model         : constant Model.Arrow.colored.view
        := Model.Arrow.colored.new_Arrow (End_2 => (0.0, 5.0, 0.0));

      the_spinner_Arrow_Model : constant Model.Arrow.colored.view
        := Model.Arrow.colored.new_Arrow (End_1 => (0.0, -2.5, 0.0),
                                          End_2 => (0.0,  2.5, 0.0));
      --  The Sprites.
      --
      use openGL.Visual.Forge;

      the_Sprites : constant openGL.Visual.views := (new_Visual (        the_Arrow_Model.all'Access),
                                                     new_Visual (the_spinner_Arrow_Model.all'Access));
      Angle : Radians := 0.0;
      Site  : openGL.Vector_2;

      use openGL.Geometry_2d;
   begin
      --  Main loop.
      --
      while not Demo.Done
      loop
         Site := to_Site (polar_Site' (Angle  => Angle,
                                       Extent => 5.0));

         the_Arrow_Model.End_Site_is (Now     => math.Vector_3 (Site & 0.0),
                                      for_End => 2);

         the_Sprites (2).Spin_is (to_Rotation (Axis  => (0.0, 0.0, 1.0),
                                               Angle => Angle));
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
end launch_render_Arrows;
