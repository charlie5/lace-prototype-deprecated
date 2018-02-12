with
     openGL.Visual,
     openGL.Model.box.lit_colored_textured,
     openGL.Palette,
     openGL.Demo,

     Ada.Text_IO,
     Ada.Exceptions;


procedure launch_render_lit_Box
--
--  Exercise the render of box models.
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


      --  The Visuals.
      --
      use openGL.Visual.Forge;

      the_Visuals : constant openGL.Visual.views := (1 => new_Visual (the_box_Model.all'Access));
   begin
      the_Visuals (1).Site_is ((0.0, 0.0, 0.0));

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
end launch_render_lit_Box;
