with
     openGL.Visual,
     openGL.Model.capsule.lit_colored_textured,
     openGL.Palette,
     openGL.Demo,

     Ada.Text_IO,
     Ada.Exceptions;


procedure launch_render_Capsules
--
--  Exercise the render of capsule models.
--
is
   use openGL,
       openGL.Model,
       openGL.Math,
       openGL.linear_Algebra_3d,
       ada.Text_IO;

   the_Texture : constant openGL.asset_Name := to_Asset ("assets/opengl/texture/Face1.bmp");
begin
   Demo.define ("openGL 'render Capsules' Demo");

   Demo.Camera.Position_is ((0.0, 3.0, 10.0),
                            y_Rotation_from (to_Radians (-0.0)));

   Demo.Dolly.Speed_is (0.1);

   declare
      use openGL.Palette;

      --  The Models.
      --
      the_capsule_Model : constant openGL.Model.capsule.lit_colored_textured.view
        := openGL.Model.capsule.lit_colored_textured.new_Capsule (radius => 0.5,
                                                                  height => 2.0,
                                                                  color  => (White, Opaque),
                                                                  image  => the_Texture);
      --  The Visuals.
      --
      use openGL.Visual.Forge;

      the_Visuals : constant openGL.Visual.views := (1 => new_Visual (the_capsule_Model.all'Access));
   begin
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
   new_Line;

exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main thread !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_render_Capsules;
