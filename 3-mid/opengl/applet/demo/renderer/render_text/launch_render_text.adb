with
     openGL.Visual,
     openGL.Palette,
     openGL.Font,
     openGL.Model.Text.lit_colored,
     openGL.Demo;

procedure launch_render_Text
--
--  Render updated text.
--
is
   use openGL,
       openGL.Palette,
       openGL.Math,
       openGL.linear_Algebra_3d;

   the_font_Id : constant openGL.Font.font_Id := (to_Asset ("assets/opengl/font/LiberationMono-Regular.ttf"), 24);

begin
   Demo.print_Usage ("Use space ' ' to cycle the text.");
   openGL.Demo.define ("openGL 'Render Text' Demo");

   --  Setup the camera.
   --
   Demo.Camera.Position_is ((3.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));

   Demo.Renderer.add_Font (the_font_Id);

   declare
      --  The model.
      --
      the_Text_Model : constant Model.Text.lit_colored.view
        := Model.Text.lit_colored.new_Text (Text     => "Howdy",
                                            Font     => the_font_Id,
                                            Color    => (Red, Opaque),
                                            Centered => False);

      --  The sprites.
      --
      use openGL.Visual.Forge;

      the_Sprites : constant openGL.Visual.views := (1 => new_Visual (the_Text_Model.all'Access));
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
                  if the_Text_Model.Text = "Howdy"
                  then
                     the_Text_Model.Text_is ("Doody");
                  else
                     the_Text_Model.Text_is ("Howdy");
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
end launch_render_Text;
