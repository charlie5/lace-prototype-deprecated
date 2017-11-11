with
     openGL.Visual,

     openGL.Model.box. colored,
     openGL.Model.box.textured,
     openGL.Model.box.lit_colored_textured,

     openGL.Palette,
     openGL.Demo,

     Ada.Text_IO,
     Ada.Exceptions;


procedure launch_render_Boxes
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
      the_box_1_Model : constant openGL.Model.box.colored.view
        := openGL.Model.box.colored.forge.new_Box
             (Size => (1.0, 2.0, 1.0),
              faces => (front => (colors => (others => (Blue,     Opaque))),
                        rear  => (colors => (others => (Blue,     Opaque))),
                        upper => (colors => (others => (Green,    Opaque))),
                        lower => (colors => (others => (Green,    Opaque))),
                        left  => (colors => (others => (Dark_Red, Opaque))),
                        right => (colors => (others => (Red,      Opaque)))));

      the_box_2_Model : constant openGL.Model.box.lit_colored_textured.view
        := openGL.Model.box.lit_colored_textured.forge.new_Box
             (Size => (1.0, 2.0, 1.0),
              faces => (front => (colors => (others => (Blue,     Opaque)),  texture_name => the_Texture),
                        rear  => (colors => (others => (Blue,     Opaque)),  texture_name => the_Texture),
                        upper => (colors => (others => (Green,    Opaque)),  texture_name => the_Texture),
                        lower => (colors => (others => (Green,    Opaque)),  texture_name => the_Texture),
                        left  => (colors => (others => (Dark_Red, Opaque)),  texture_name => the_Texture),
                        right => (colors => (others => (Red,      Opaque)),  texture_name => the_Texture)));

      the_box_3_Model : constant openGL.Model.box.textured.view
        := openGL.Model.box.textured.forge.new_Box
             (Size => (1.0, 2.0, 1.0),
              faces => (front => (texture_name   => the_Texture),
                        rear  => (texture_name   => the_Texture),
                        upper => (texture_name   => the_Texture),
                        lower => (texture_name   => the_Texture),
                        left  => (texture_name   => the_Texture),
                        right => (texture_name   => the_Texture)));

      --  The Visuals.
      --
      use openGL.Visual.Forge;

      the_Visuals : constant openGL.Visual.views := (1 => new_Visual (the_box_1_Model.all'Access),
                                                     2 => new_Visual (the_box_2_Model.all'Access),
                                                     3 => new_Visual (the_box_3_Model.all'Access));
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
   new_Line;

exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main thread !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_render_Boxes;
