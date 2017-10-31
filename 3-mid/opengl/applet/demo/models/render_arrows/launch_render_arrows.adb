with
     openGL.Visual,
     openGL.Model.arrow.colored,
     openGL.Demo,

     Ada.Text_IO,
     Ada.Exceptions;


procedure launch_render_Arrows
--
--  Exercise the render of arrow models.
--
is
   use openGL,
       openGL.Model,
       openGL.Math,
       openGL.linear_Algebra_3d,
       ada.Text_IO;
begin
   Demo.define ("openGL 'render Arrows' Demo");
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));
   declare
      --  The Models.
      --
      the_arrow_Model : constant openGL.Model.arrow.colored.view
        := openGL.Model.arrow.colored.Forge.new_Arrow (End_2 => (0.0, 5.0, 0.0));

      --  The Sprites.
      --
      use openGL.Visual.Forge;

      the_Sprites : constant openGL.Visual.views := (1 => new_Visual (the_arrow_Model.all'Access));
      Current     :          Integer             := the_Sprites'First;

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
                  if     Current  = the_Sprites'Last
                  then   Current := the_Sprites'First;
                  else   Current := Current + 1;
                  end if;

               when others =>
                  null;
               end case;
            end if;
         end;

         --  Render the sprite.
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
end launch_render_Arrows;
