with
     openGL.Visual,
     openGL.Model.Billboard.        textured,
     openGL.Model.Billboard.colored_textured,
     openGL.Palette,
     openGL.Demo;

procedure launch_render_Billboards
--
--  Exercise the render of billboard models.
--
is
   use openGL,
       openGL.Model,
       openGL.Math,
       openGL.linear_Algebra_3d;

   the_Texture : constant openGL.asset_Name :=  to_Asset ("assets/opengl/texture/Face1.bmp");
begin
   Demo.print_Usage;
   Demo.define ("openGL 'Render Billboards' Demo");
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));
   declare
      --  The Models.
      --
      the_Billboard_Model : constant Model.Billboard.textured.view
        := Model.Billboard.textured.forge.new_Billboard (--Scale   => (1.0, 1.0, 1.0),
                                                         Plane   => Billboard.xy,
                                                         Texture => the_Texture);

      the_colored_Billboard_Model : constant Model.Billboard.colored_textured.view
        := Model.Billboard.colored_textured.new_Billboard (--Scale   => (1.0, 1.0, 1.0),
                                                           Plane   => Billboard.xy,
                                                           Color   => (Palette.Green, Opaque),
                                                           Texture => the_Texture);
      --  The Sprites.
      --
      use openGL.Visual.Forge;

      the_Sprites : constant openGL.Visual.views := (new_Visual (        the_Billboard_Model.all'Access),
                                                     new_Visual (the_colored_Billboard_Model.all'Access));
   begin
      the_Sprites (2).Site_is ((3.0, 0.0, 0.0));

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
         Demo.Camera.render (the_Sprites);

         while not Demo.Camera.cull_Completed
         loop
            delay Duration'Small;
         end loop;

         Demo.Renderer.render;
         Demo.FPS_Counter.increment;    -- Frames per second display.
      end loop;
   end;

   Demo.destroy;
end launch_render_Billboards;
