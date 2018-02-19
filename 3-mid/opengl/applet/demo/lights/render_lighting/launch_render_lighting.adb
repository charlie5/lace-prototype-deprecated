with
     openGL.Light.directional,
     openGL.Visual,
     openGL.Model.box   .lit_colored_textured,
     openGL.Model.sphere.lit_colored_textured,
     openGL.Model.sphere.lit_colored,
     openGL.Palette,
     openGL.Demo,

     Ada.Text_IO,
     Ada.Exceptions;


procedure launch_render_Lighting
--
--  Exercise the rendering of lit models.
--
is
   use openGL,
       openGL.Model,
       openGL.Math,
       openGL.linear_Algebra_3d,
       ada.Text_IO;

   the_Texture : constant openGL.asset_Name := to_Asset ("assets/opengl/texture/Face1.bmp");
begin
   Demo.define ("openGL 'render Boxes' Demo");
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));
   declare
      use openGL.Model.box,
          openGL.Palette;

      --  The Models.
      --
      the_box_Model : constant openGL.Model.box.lit_colored_textured.view
        := openGL.Model.box.lit_colored_textured.new_Box
             (Size => (1.0, 2.0, 1.0),
              faces => (front => (colors => (others => (Blue,     Opaque)),  texture_name => the_Texture),
                        rear  => (colors => (others => (Blue,     Opaque)),  texture_name => the_Texture),
                        upper => (colors => (others => (Green,    Opaque)),  texture_name => the_Texture),
                        lower => (colors => (others => (Green,    Opaque)),  texture_name => the_Texture),
                        left  => (colors => (others => (Dark_Red, Opaque)),  texture_name => the_Texture),
                        right => (colors => (others => (Red,      Opaque)),  texture_name => the_Texture)));

      the_ball_1_Model : constant openGL.Model.sphere.lit_colored_textured.view
        := openGL.Model.sphere.lit_colored_textured.new_Sphere (Radius => 1.0,
                                                                Image  => the_Texture);
      the_ball_2_Model : constant openGL.Model.sphere.lit_colored.view
        := openGL.Model.sphere.lit_colored.new_Sphere (Radius => 1.0,
                                                       Color  => (openGL.Palette.light_Apricot, openGL.Opaque));

      --  The Visuals.
      --
      use openGL.Visual.Forge;

--        the_Visuals : constant openGL.Visual.views := (1 => new_Visual (the_box_Model.all'Access));

      the_Visuals : constant openGL.Visual.views := (1 => new_Visual (the_ball_1_Model.all'Access),
                                                     2 => new_Visual (the_ball_2_Model.all'Access));

      -- Light movement.
      --
      initial_Site : constant openGL.Vector_3    := (-100_000_000.0, 0.0, 100_000_000.0);
      site_Delta   :          openGL.Vector_3    := (      10_000.0, 0.0,           0.0);

   begin
      the_Visuals (1).Site_is ((0.0,  0.0, 0.0));
      the_Visuals (1).Site_is ((0.0, -2.0, 0.0));

      -- Set the lights initial position to far behind and far to the left.
      --
      declare
         Light : openGL.Light.directional.item := Demo.Renderer.Light (Id => 1);
      begin
         Light.Site_is (initial_Site);
         Demo.Renderer.Light_is (Id  => 1,
                                 Now => Light);
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
            if    Light.Site (1) > 100_000_000.0 then   site_Delta (1) := -10_000.0;
            elsif Light.Site (1) > 100_000_000.0 then   site_Delta (1) :=  10_000.0;
            end if;

            Light.Site_is (Light.Site + site_Delta);
            Demo.Renderer.Light_is (Id  => 1,
                                    Now => Light);
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
   new_Line;

exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main task !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_render_Lighting;
