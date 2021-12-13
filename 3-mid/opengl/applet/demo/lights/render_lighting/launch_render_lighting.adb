with
     openGL.Light,
     openGL.Visual,
     openGL.Model.Sphere.lit_colored_textured,
     openGL.Model.Sphere.lit_colored,
     openGL.Palette,
     openGL.Demo;


procedure launch_render_Lighting
--
--  Exercise the rendering of lit models.
--
is
   use openGL,
       openGL.Model,
       openGL.Math,
       openGL.linear_Algebra_3d;

   the_Texture : constant asset_Name := to_Asset ("assets/opengl/texture/Face1.bmp");
begin
   Demo.print_Usage ("To see the light move, disable 'Sync to VBlank'.");
   Demo.define ("openGL 'render Lighting' Demo");
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));
   declare
      use openGL.Palette;

      --  The Models.
      --
      the_Ball_1_Model : constant Model.Sphere.lit_colored_textured.view
        := openGL.Model.Sphere.lit_colored_textured.new_Sphere (Radius => 1.0,
                                                                Image  => the_Texture);
      the_Ball_2_Model : constant Model.Sphere.lit_colored.view
        := openGL.Model.Sphere.lit_colored.new_Sphere (Radius => 1.0,
                                                       Color  => (light_Apricot, Opaque));

      --  The Visuals.
      --
      use openGL.Visual.Forge;

      the_Visuals : constant openGL.Visual.views := (1 => new_Visual (the_Ball_1_Model.all'Access),
                                                     2 => new_Visual (the_Ball_2_Model.all'Access));

      the_Light : openGL.Light.item := Demo.Renderer.new_Light;

      -- Light movement.
      --
      initial_Site : constant openGL.Vector_3 := (-10_000.0, 0.0, 10_000.0);
      site_Delta   :          openGL.Vector_3 := (      1.0, 0.0,      0.0);

   begin
      the_Visuals (1).Site_is ((0.0,  1.0, 0.0));
      the_Visuals (2).Site_is ((0.0, -1.0, 0.0));

      -- Set the lights initial position to far behind and far to the left.
      --
      the_Light.Site_is (initial_Site);
      Demo.Renderer.set (the_Light);


      --  Main loop.
      --
      while not Demo.Done
      loop
         -- Handle user commands.
         --
         Demo.Dolly.evolve;
         Demo.Done := Demo.Dolly.quit_Requested;

         -- Move the light.
         --
         if    the_Light.Site (1) >  10_000.0
         then
            site_Delta (1) := -1.0;
            the_Light.Color_is (Palette.dark_Green);

         elsif the_Light.Site (1) < -10_000.0
         then
            site_Delta (1) := 1.0;

            the_Light.Color_is (openGL.Palette.dark_Red);
         end if;

         the_Light.Site_is (the_Light.Site + site_Delta);

         Demo.Renderer.set (the_Light);

         --  Render the sprites.
         --
         Demo.Camera.render (the_Visuals);

         while not Demo.Camera.cull_Completed
         loop
            delay Duration'Small;
         end loop;

         Demo.Renderer.render;
         Demo.FPS_Counter.increment;    -- Frames per second display.
      end loop;
   end;

   Demo.destroy;
end launch_render_Lighting;
