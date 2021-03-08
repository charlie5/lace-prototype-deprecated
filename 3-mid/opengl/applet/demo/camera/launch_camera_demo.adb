with
     openGL.Renderer.lean,
     openGL.Camera,
     openGL.Visual,
     openGL.Palette,
     openGL.Server,
     openGL.Model.box.colored,
     openGL.Demo,

     lumen.Window,

     ada.Text_IO;


procedure launch_camera_Demo
--
--  Exercise the camera.
--
is
   use openGL,
       openGL.Model,
       openGL.Model.box,
       openGL.Palette,
       openGL.Math,
       openGL.linear_Algebra_3d,
       ada.Text_IO;

--     Win          :          Lumen .Window.Window_Handle;
--     the_Camera   :          openGL.Camera.item;
--     the_Renderer : aliased  openGL.Renderer.lean.item;

begin
   openGL.Demo.define ("openGL 'Camera' Demo");

--     --  Create Lumen window (which provides a current GL context).
--     --
--     Lumen.Window.Create (Win,
--                          width    => 1000,
--                          height   => 1000,
--                          name     => "openGL 'Camera' Demo",
--                          animated => True);
--
--     put_Line ("openGL Server: " & openGL.Server.Version);


   --  Setup the camera.
   --
--     the_Camera.define;
--     the_Camera.Renderer_is (the_Renderer'unchecked_Access);
   Demo.Camera.Position_is ((5.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));

   declare
      --  The Models.
      --
      the_box_Model_1 : constant openGL.Model.box.colored.view
        := openGL.Model.box.colored.new_Box (size  => (0.5, 0.5, 0.5),
                                             faces => (front => (colors => (others => (Blue,            Opaque))),
                                                       rear  => (colors => (others => (light_Blue,      Opaque))),
                                                       upper => (colors => (others => (Green,           Opaque))),
                                                       lower => (colors => (others => (forest_Green,    Opaque))),
                                                       left  => (colors => (others => (Dark_Red,        Opaque))),
                                                       right => (colors => (others => (Red,             Opaque)))));

      the_Sprite     : constant openGL.Visual.view
        := openGL.Visual.Forge.new_Visual (the_box_Model_1.all'Access);

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
         Demo.Camera.render (the_Visuals => (1 => the_Sprite));

         while not Demo.Camera.cull_Completed
         loop
            delay Duration'Small;
         end loop;

         Demo.Renderer.render;
      end loop;
   end;

   Demo.destroy;
   new_Line;
end launch_camera_Demo;
