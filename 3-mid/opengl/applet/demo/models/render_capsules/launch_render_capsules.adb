with
     openGL.Visual,
     openGL.Model.Capsule.lit_colored_textured,
     openGL.Palette,
     openGL.Light,
     openGL.Demo;


procedure launch_render_Capsules
--
--  Exercise the render of capsule models.
--
is
   use openGL,
       openGL.Model,
       openGL.Math,
       openGL.linear_Algebra_3d;
begin
   Demo.print_Usage;
   Demo.define ("openGL 'Render Capsules' Demo");
   Demo.Camera.Position_is ((0.0, 3.0, 10.0),
                            y_Rotation_from (to_Radians (-0.0)));
   Demo.Dolly.Speed_is (0.1);

   declare
      use openGL.Palette;

      the_Light   : openGL.Light.item   := Demo.Renderer.new_Light;
      the_Texture : constant asset_Name := to_Asset ("assets/opengl/texture/Face1.bmp");

      --  The Models.
      --
      the_Capsule_Model : constant Model.Capsule.lit_colored_textured.view
        := Model.Capsule.lit_colored_textured.new_Capsule (Radius => 0.5,
                                                           Height => 2.0,
                                                           Color  => (White, Opaque),
                                                           Image  => the_Texture);
      --  The Visuals.
      --
      use openGL.Visual.Forge;

      the_Visuals : constant openGL.Visual.views := (1 => new_Visual (the_Capsule_Model.all'Access));
   begin
      the_Light.Site_is ((0.0, 5.0, 10.0));
      Demo.Renderer.set (the_Light);

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
end launch_render_Capsules;
