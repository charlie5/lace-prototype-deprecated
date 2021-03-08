with
     openGL.Visual,
     openGL.Palette,
--       openGL.Texture,
--       openGL.IO,
     openGL.Font, --.texture,
     openGL.Model.Text.lit_colored_textured,
     openGL.Demo,

     ada.Text_IO,
--       ada.Unchecked_Deallocation,
     ada.Exceptions;


procedure launch_render_Text
--
--  Render updated text.
--
is
   use openGL,
--         openGL.Model,
       openGL.Palette,
       openGL.Math,
       openGL.linear_Algebra_3d,
       ada.Text_IO;

--     the_Font : openGL.Font.texture.view;
--     Success  : Boolean;

   the_font_Id : constant openGL.Font.font_Id := (to_Asset ("assets/opengl/font/LiberationMono-Regular.ttf"), 24);


begin
   openGL.Demo.define ("openGL 'render Models' Demo");

   --  Setup the camera.
   --
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));

   Demo.Renderer.add_Font  (the_font_Id);

--     the_Font := openGL.Font.texture.new_Font_texture ("assets/opengl/font/LiberationMono-Regular.ttf");
--     Success  := the_Font.FaceSize (15,  78, 95);


   declare
--        the_Texture : openGL.asset_Name := to_Asset ("assets/opengl/texture/Face1.bmp");

      --  The Models.
      --

      the_text_Model : constant openGL.Model.Text.lit_colored_textured.view
        := openGL.Model.Text.lit_colored_textured.new_Text (scale => (1.0, 1.0, 1.0),
                                                            text     => "Howdy",
                                                            Font     => the_font_Id,
                                                            Color    => (Red, Opaque),
                                                            Centered => False);

      --  The Sprites.
      --
      use openGL.Visual.Forge;

      the_Sprites : constant openGL.Visual.views := (1 => new_Visual (the_text_Model.all'Access));
      Current     : constant Integer             := the_Sprites'First;

   begin
      --  Main loop.
      --
      while not Demo.Done
      loop
         Demo.Dolly.evolve;
         Demo.Done := Demo.Dolly.quit_Requested;

         declare
            Command : Character;
            Avail   : Boolean;
         begin
            Demo.Dolly.get_last_Character (Command, Avail);

            if Avail
            then
               case Command
               is
               when ' ' =>
                  if the_text_Model.Text.all = "Howdy"
                  then
                     the_text_Model.Text_is ("Doody");
                  else
                     the_text_Model.Text_is ("Howdy");
                  end if;

               when others =>
                  null;
               end case;
            end if;
         end;

         --  Render all sprites.
         --
         Demo.Camera.render ((1 => the_Sprites (Current)));

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
end launch_render_Text;
