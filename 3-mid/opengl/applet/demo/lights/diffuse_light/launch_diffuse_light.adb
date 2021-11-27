with
     openGL.Light.directional,
     openGL.Light.diffuse,
     openGL.Visual,
     openGL.Model.Box.lit,
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
   Demo.print_Usage ("To see the light move, disable 'Sync to VBlank'.");
   Demo.define ("openGL 'diffuse Light' Demo");
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));
   declare
      use openGL.Model.box,
          openGL.Palette;

      --  The Models.
      --
      the_Box : constant Model.Box.lit.view
        := openGL.Model.Box.lit.new_Box (Size  => (1.0, 1.0, 1.0),
                                         Faces => (Front => (Colors => (others => (Blue,     Opaque)),  texture_Name => the_Texture),
                                                   Rear  => (Colors => (others => (Blue,     Opaque)),  texture_Name => the_Texture),
                                                   Upper => (Colors => (others => (Green,    Opaque)),  texture_Name => the_Texture),
                                                   Lower => (Colors => (others => (Green,    Opaque)),  texture_Name => the_Texture),
                                                   Left  => (Colors => (others => (Dark_Red, Opaque)),  texture_Name => the_Texture),
                                                   Right => (Colors => (others => (Red,      Opaque)),  texture_Name => the_Texture)));


      --  The Visuals.
      --
      use openGL.Visual.Forge;

      the_Visuals : constant openGL.Visual.views := (1 => new_Visual (the_Box.all'Access));

      -- Light movement.
      --
      initial_Site : constant openGL.Vector_3 := (-10_000.0, 0.0, 10_000.0);
      site_Delta   :          openGL.Vector_3 := (      1.0, 0.0,      0.0);

   begin
      the_Visuals (1).Site_is ((0.0,  1.0, 0.0));

      -- Set the lights initial position to far behind and far to the left.
      --
      declare
         Light : openGL.Light.directional.item := Demo.Renderer.Light (Id => 1);
         --  Light : openGL.Light.diffuse.item := Demo.Renderer.Light (Id => 1);
      begin
         Light.Site_is (initial_Site);
         Demo.Renderer.Light_is (Id => 1, Now => Light);
      end;


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
         declare
            Light : openGL.Light.directional.item := Demo.Renderer.Light (Id => 1);
         begin
            if    Light.Site (1) >  10_000.0
            then
               site_Delta (1) := -1.0;

               Light.Color_is (Ambient  => (openGL.Palette.dark_Green, Opaque),
                               Diffuse  => (openGL.Palette.Grey,       Opaque),
                               Specular => (openGL.Palette.White,      Opaque));

            elsif Light.Site (1) < -10_000.0
            then
               site_Delta (1) :=  1.0;

               Light.Color_is (Ambient  => (openGL.Palette.dark_Red, Opaque),
                               Diffuse  => (openGL.Palette.Grey,     Opaque),
                               Specular => (openGL.Palette.White,    Opaque));
            end if;

            Light.Site_is (Light.Site + site_Delta);

            Demo.Renderer.Light_is (Id => 1, Now => Light);
         end;

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
end launch_diffuse_Light;
