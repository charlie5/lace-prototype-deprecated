with
     openGL.Camera,
     openGL.Palette,
     openGL.Model.Box   .lit_colored_textured,
     openGL.Model.Sphere.lit_colored_textured,
     openGL.Visual,
     openGL.Demo;

procedure launch_two_Cameras_Demo
--
--  Exercise the culler with two cameras.
--
is
   use openGL,
       openGL.Model,
       openGL.Model.Box,
       openGL.Palette,
       openGL.Math,
       openGL.linear_Algebra_3d;

   Camera_2 : openGL.Camera.item;
begin
   Demo.print_Usage;
   openGL.Demo.define ("openGL 'Two Cameras' Demo");

   --  Setup the extra camera.
   --
   Camera_2.define;
   Camera_2.Renderer_is (Demo.Renderer'unchecked_Access);

   Camera_2.Position_is ((0.0, 20.0, 0.0),
                         y_Rotation_from (to_Radians (0.0)));

   Camera_2.Viewport_is (width  => 1000,
                         height => 1000);


   -- Create the sprites.
   --
   declare
      use openGL.Math.Functions;

      --  The Models.
      --
      the_Face      : constant asset_Name := to_Asset ("assets/opengl/texture/Face1.bmp");
      the_box_Model : constant Model.box.lit_colored_textured.view
        := Model.box.lit_colored_textured.new_Box
             (size  => (0.5, 0.5, 0.5),
              faces => (front => (colors => (others => (White,    Opaque)),  texture_name => the_Face),
                        rear  => (colors => (others => (Blue,     Opaque)),  texture_name => the_Face),
                        upper => (colors => (others => (Green,    Opaque)),  texture_name => the_Face),
                        lower => (colors => (others => (Green,    Opaque)),  texture_name => the_Face),
                        left  => (colors => (others => (Dark_Red, Opaque)),  texture_name => the_Face),
                        right => (colors => (others => (Red,      Opaque)),  texture_name => the_Face)));

      the_ball_Model : constant Model.Sphere.lit_colored_textured.view
        := Model.Sphere.lit_colored_textured.new_Sphere (radius => 0.5);

      --  The Sprites.
      --
      the_Sprites   : constant Visual.views (1 .. 4_000) := (others => Visual.Forge.new_Visual (Model.view (the_box_Model)));
      the_Sprites_2 : constant Visual.views (1 .. 4_000) := (others => Visual.Forge.new_Visual (Model.view (the_ball_Model)));

      grid_Size     : constant openGL.Real := SqRt (openGL.Real (the_Sprites'Length));
      x             :          openGL.Real := -grid_Size / 2.0;
      z             :          openGL.Real := 0.0;

   begin
      Demo.Dolly.Speed_is (0.1);

      for i in the_Sprites'Range
      loop
         x := x + 1.0;

         if i mod Integer (SqRt (openGL.Real (the_Sprites'Length))) = 0
         then
            z := z - 1.0;
            x := -grid_Size / 2.0;
         end if;

         the_Sprites (i).Site_is ((x, 0.0, z));
      end loop;


      for i in the_Sprites_2'Range
      loop
         x := x + 1.2;

         if i mod Integer (SqRt (openGL.Real (the_Sprites_2'Length))) = 0
         then
            z := z - 1.0;
            x := -grid_Size / 2.0;
         end if;

         the_Sprites_2 (i).Site_is ((x, 0.0, z));
      end loop;


      --  Main loop.
      --
      while not Demo.Done
      loop
         Demo.Dolly.evolve;
         Demo.Done := Demo.Dolly.quit_Requested;

         Demo.Camera.render (the_Sprites);
         Camera_2   .render (the_Sprites_2);

         while not (    Demo.Camera.cull_Completed
                    and Camera_2   .cull_Completed)
         loop
            delay Duration'Small;
         end loop;

         Demo.Renderer.render;
         Demo.FPS_Counter.increment;    -- Frames per second display.
      end loop;
   end;

   Demo.destroy;
   Camera_2.destroy;
end launch_two_Cameras_Demo;
