with
     openGL.Model,
     openGL.Visual,
     openGL.Demo,

     Ada.Text_IO,
     Ada.Exceptions;

procedure launch_render_Models
--
--  Exercise the renderer with an example of all the models.
--
is
   use openGL,
       openGL.Math,
       openGL.linear_Algebra_3d,
       ada.Text_IO;

begin
   Demo.define ("openGL 'render Models' Demo");
   Demo.Camera.Position_is ((0.0, 0.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));

   declare
      --  The models.
      --
      the_Models : constant openGL.Model.views := openGL.Demo.Models;

      --  The visuals.
      --
      use openGL.Visual.Forge;

      the_Visuals : openGL.Visual.views (the_Models'Range);
      Current     : Integer := the_Visuals'First;

   begin
      for i in the_Visuals'Range
      loop
         the_Visuals (i) := new_Visual (the_Models (i));
      end loop;

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
                  if     Current  = the_Visuals'Last
                  then   Current := the_Visuals'First;
                  else   Current := Current + 1;
                  end if;

               when others =>
                  null;
               end case;
            end if;
         end;

         --  Render all sprites.
         --
         Demo.Camera.render ((1 => the_Visuals (Current)));

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
end launch_render_Models;
