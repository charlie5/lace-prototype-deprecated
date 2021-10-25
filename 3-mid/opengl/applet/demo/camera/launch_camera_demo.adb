with
     openGL.Renderer.lean,
     openGL.Camera,
     openGL.Visual,
     openGL.Palette,
     openGL.Model.box.colored,
     openGL.Demo;

procedure launch_Camera_Demo
--
--  Exercise the camera.
--
is
   use openGL,
       openGL.Model,
       openGL.Model.box,
       openGL.Palette,
       openGL.Math,
       openGL.linear_Algebra_3d;

begin
   Demo.print_Usage;
   Demo.define ("openGL 'Camera' Demo");

   --  Setup the camera.
   --
   Demo.Camera.Position_is ((5.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));

   declare
      --  The Model.
      --
      the_box_Model : constant openGL.Model.Box.colored.view
        := openGL.Model.Box.colored.new_Box (size  => (0.5, 0.5, 0.5),
                                             faces => (front => (colors => (others => (Blue,         Opaque))),
                                                       rear  => (colors => (others => (light_Blue,   Opaque))),
                                                       upper => (colors => (others => (Green,        Opaque))),
                                                       lower => (colors => (others => (forest_Green, Opaque))),
                                                       left  => (colors => (others => (Dark_Red,     Opaque))),
                                                       right => (colors => (others => (Red,          Opaque)))));

      the_Sprite : constant openGL.Visual.view
        := openGL.Visual.Forge.new_Visual (the_box_Model.all'Access);

   begin
      the_Sprite.Site_is ((10.0, 0.0, 0.0));

      --  Main loop.
      --
      while not Demo.Done
      loop
         Demo.Dolly.evolve;
         Demo.Done := Demo.Dolly.quit_Requested;

         --  Render all sprites.
         --
         Demo.Camera.render (Visuals => (1 => the_Sprite));

         while not Demo.Camera.cull_Completed
         loop
            delay Duration'Small;
         end loop;

         Demo.Renderer.render;
      end loop;
   end;

   Demo.destroy;
end launch_Camera_Demo;
