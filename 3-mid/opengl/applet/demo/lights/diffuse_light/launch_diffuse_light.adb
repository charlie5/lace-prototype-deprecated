with
     openGL.Light,
     openGL.Visual,
     openGL.Model.Box.lit_textured,
     openGL.Palette,
     openGL.Demo;


procedure launch_diffuse_Light
--
--  Exercise the rendering of models with a diffuse light.
--
is
   use openGL,
       openGL.Model,
       openGL.Math,
       openGL.linear_Algebra_3d;

   the_Texture : constant asset_Name := to_Asset ("assets/opengl/texture/Face1.bmp");

begin
   Demo.print_Usage;
   Demo.define ("openGL 'diffuse Light' Demo");
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));
   declare
      use openGL.Model.box,
          openGL.Visual.Forge,
          openGL.Light,
          openGL.Palette;

      --  The Model.
      --
      the_Box : constant Model.Box.lit_textured.view
        := openGL.Model.Box.lit_textured.new_Box (Size  => (4.0, 4.0, 4.0),
                                                  Faces => (Front => (texture_Name => the_Texture),
                                                            Rear  => (texture_Name => the_Texture),
                                                            Upper => (texture_Name => the_Texture),
                                                            Lower => (texture_Name => the_Texture),
                                                            Left  => (texture_Name => the_Texture),
                                                            Right => (texture_Name => the_Texture)));
      --  The Visual.
      --
      the_Visuals : constant openGL.Visual.views := (1 => new_Visual (the_Box.all'Access));


      -- The Light.
      --
      the_Light      :          openGL.Light.item := Demo.Renderer.new_Light;
      initial_Site   : constant openGL.Vector_3   := (0.0, 0.0, 15.0);
      site_Delta     :          openGL.Vector_3   := (1.0, 0.0,  0.0);
      cone_Direction : constant openGL.Vector_3   := (0.0, 0.0, -1.0);

   begin
      -- Setup the visual.
      --
      the_Visuals (1).Site_is (Origin_3D);
      the_Visuals (1).Spin_is (y_Rotation_from (to_Radians (20.0)));

      -- Setup the light.
      --
      the_Light. Kind_is (Diffuse);
      the_Light. Site_is (initial_Site);
      the_Light.Color_is (White);

      the_Light.     cone_Angle_is     (5.0);
      the_Light.     cone_Direction_is (cone_Direction);
      the_Light.ambient_Coefficient_is (0.015);

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
         if    the_Light.Site (1) >  2.0 then   site_Delta (1) := -0.01;
         elsif the_Light.Site (1) < -2.0 then   site_Delta (1) :=  0.01;
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

         delay 1.0 / 60.0;
      end loop;
   end;

   Demo.destroy;
end launch_diffuse_Light;
