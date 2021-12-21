with
     openGL.Visual,
     openGL.Terrain,
     openGL.Demo,
     openGL.Light;


procedure launch_large_Terrain_Demo
--
--  Exercise the culler with a large terrain grid.
--
is
   use openGL,
       openGL.Math,
       openGL.linear_Algebra_3d;

begin
   Demo.print_Usage;
   Demo.define ("openGL 'Large Terrain' Demo");

   --  Setup the camera.
   --
   Demo.Camera.Position_is ((0.0, 100.0, 500.0),
                            y_Rotation_from (to_Radians (0.0)));

   -- Set the lights initial position to far behind and far to the left.
   --
   declare
      use openGL.Light;
      the_Light : openGL.Light.item := Demo.Renderer.new_Light;
   begin
      the_Light.Site_is ((0.0, 1000.0, 0.0));
      Demo.Renderer.set (the_Light);
   end;


   declare
      Heights : constant asset_Name := to_Asset ("assets/kidwelly-terrain-510x510.png");
      Texture : constant asset_Name := to_Asset ("assets/kidwelly-terrain-texture-255x255.png");

      Terrain : constant openGL.Visual.Grid := openGL.Terrain.new_Terrain (heights_File => Heights,
                                                                           texture_File => Texture,
                                                                           Scale        => (1.0, 25.0, 1.0));
      Count   : constant Positive :=   Terrain'Length (1)
                                     * Terrain'Length (2);
      Last    : Natural := 0;
      Sprites : openGL.Visual.views (1 .. Count);

   begin
      for Row in Terrain'Range (1)
      loop
         for Col in Terrain'Range (2)
         loop
            Last           := Last + 1;
            Sprites (Last) := Terrain (Row, Col);
         end loop;
      end loop;


      --  Main loop.
      --
      while not Demo.Done
      loop
         Demo.Dolly.evolve;
         Demo.Done := Demo.Dolly.quit_Requested;

         Demo.Camera.render (Sprites (1 .. Last));

         while not Demo.Camera.cull_Completed
         loop
            delay Duration'Small;
         end loop;

         Demo.Renderer.render;
         Demo.FPS_Counter.increment;    -- Frames per second display.
      end loop;
   end;

   Demo.destroy;
end launch_large_Terrain_Demo;
