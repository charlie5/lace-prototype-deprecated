with
     openGL.Visual,
     openGL.Model.billboard.        textured,
     openGL.Model.billboard.colored_textured,
     openGL.Palette,
     openGL.Demo,

     Ada.Text_IO,
     Ada.Exceptions;


procedure launch_render_Billboards
--
--  Exercise the render of billboard models.
--
is
   use openGL,
       openGL.Model,
       openGL.Math,
       openGL.linear_Algebra_3d,
       ada.Text_IO;

   the_Texture : constant openGL.asset_Name   :=  to_Asset ("assets/opengl/texture/Face1.bmp");
begin
   Demo.define ("openGL 'render Billboards' Demo");
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));
   declare
      --  The Models.
      --
      the_billboard_Model : constant openGL.Model.billboard.textured.view
        := openGL.Model.billboard.textured.forge.new_Billboard (scale   => (1.0, 1.0, 1.0),
                                                                plane   => Billboard.xy,
                                                                texture => the_Texture);

      the_colored_billboard_Model : constant openGL.Model.billboard.colored_textured.view
        := openGL.Model.billboard.colored_textured.forge.new_Billboard (scale   => (1.0, 1.0, 1.0),
                                                                        plane   => Billboard.xy,
                                                                        color   => (Palette.Green, Opaque),
                                                                        texture => the_Texture);
      --  The Sprites.
      --
      use openGL.Visual.Forge;

      the_Sprites : constant openGL.Visual.views := (new_Visual (        the_billboard_Model.all'Access),
                                                     new_Visual (the_colored_billboard_Model.all'Access));
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
   new_Line;

exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main thread !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_render_Billboards;
