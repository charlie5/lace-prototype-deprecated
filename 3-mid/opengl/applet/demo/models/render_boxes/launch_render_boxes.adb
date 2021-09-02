with
     openGL.Visual,

     openGL.Model.Box. colored,
     openGL.Model.Box.textured,
     openGL.Model.Box.lit_colored_textured,

     openGL.Palette,
     openGL.Demo;

procedure launch_render_Boxes
--
--  Exercise the rendering of box models.
--
is
   use openGL,
       openGL.Model,
       openGL.Math,
       openGL.linear_Algebra_3d;

   the_Texture : constant openGL.asset_Name := to_Asset ("assets/opengl/texture/Face1.bmp");
begin
   Demo.print_Usage;
   Demo.define ("openGL 'Render Boxes' Demo");
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));
   declare
      use openGL.Model.box,
          openGL.Palette;

      --  The Models.
      --
      the_Box_1_Model : constant Model.Box.colored.view
        := Model.Box.colored.new_Box
             (Size => (1.0, 2.0, 1.0),
              Faces => (Front => (Colors => (others => (Blue,     Opaque))),
                        Rear  => (Colors => (others => (Blue,     Opaque))),
                        Upper => (Colors => (others => (Green,    Opaque))),
                        Lower => (Colors => (others => (Green,    Opaque))),
                        Left  => (Colors => (others => (Dark_Red, Opaque))),
                        Right => (Colors => (others => (Red,      Opaque)))));

      the_Box_2_Model : constant Model.Box.lit_colored_textured.view
        := Model.Box.lit_colored_textured.new_Box
             (Size => (1.0, 2.0, 1.0),
              Faces => (Front => (Colors => (others => (Blue,     Opaque)),  texture_Name => the_Texture),
                        Rear  => (Colors => (others => (Blue,     Opaque)),  texture_Name => the_Texture),
                        Upper => (Colors => (others => (Green,    Opaque)),  texture_Name => the_Texture),
                        Lower => (Colors => (others => (Green,    Opaque)),  texture_Name => the_Texture),
                        Left  => (Colors => (others => (Dark_Red, Opaque)),  texture_Name => the_Texture),
                        Right => (Colors => (others => (Red,      Opaque)),  texture_Name => the_Texture)));

      the_Box_3_Model : constant Model.Box.textured.view
        := Model.Box.textured.new_Box
             (Size => (1.0, 2.0, 1.0),
              Faces => (Front => (texture_Name => the_Texture),
                        Rear  => (texture_Name => the_Texture),
                        Upper => (texture_Name => the_Texture),
                        Lower => (texture_Name => the_Texture),
                        Left  => (texture_Name => the_Texture),
                        Right => (texture_Name => the_Texture)));

      --  The Visuals.
      --
      use openGL.Visual.Forge;

      the_Visuals : constant openGL.Visual.views := (1 => new_Visual (the_Box_1_Model.all'Access),
                                                     2 => new_Visual (the_Box_2_Model.all'Access),
                                                     3 => new_Visual (the_Box_3_Model.all'Access));
   begin
      the_Visuals (1).Site_is ((-3.0, 0.0, 0.0));
      the_Visuals (2).Site_is (( 0.0, 0.0, 0.0));
      the_Visuals (3).Site_is (( 3.0, 0.0, 0.0));

      --  Main loop.
      --
      while not Demo.Done
      loop
         -- Handle user commands.
         --
         Demo.Dolly.evolve;
         Demo.Done := Demo.Dolly.quit_Requested;

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
end launch_render_Boxes;
