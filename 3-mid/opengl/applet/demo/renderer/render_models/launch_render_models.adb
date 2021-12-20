with
     openGL.Model,
     openGL.Visual,
     openGL.Light,
     openGL.Palette,
     openGL.Demo;


procedure launch_render_Models
--
--  Exercise the renderer with an example of all the models.
--
is
   use openGL,
       openGL.Math,
       openGL.linear_Algebra_3D,
       openGL.Palette;

begin
   Demo.print_Usage ("Use space ' ' to cycle through models.");
   Demo.define ("openGL 'Render Models' Demo");
   Demo.Camera.Position_is ((0.0, 2.0, 10.0),
                            y_Rotation_from (to_Radians (0.0)));

   declare
      use openGL.Light;
      the_Light : openGL.Light.item := Demo.Renderer.new_Light;
   begin
--        the_Light.Kind_is (Diffuse);
--        the_Light.Site_is ((0.0, 0.0, 5.0));
      the_Light.Site_is ((5_000.0, 2_000.0, 5_000.0));
--        the_Light.Site_is ((000.0, 5_000.0, 000.0));

          the_Light.Color_is (White);
--        the_Light.ambient_Coefficient_is (0.91);

      Demo.Renderer.set (the_Light);
   end;


   -- Set the lights initial position to far behind and far to the left.
   --
--     declare
--        use openGL.Palette;
--
--        initial_Site   : constant openGL.Vector_3 := (0.0, 0.0, 15.0);
--        cone_Direction : constant openGL.Vector_3 := (0.0, 0.0, -1.0);
--
--        Light : openGL.Light.diffuse.item := Demo.Renderer.Light (Id => 1);
--     begin
--        Light.Color_is (Ambient  => (Grey,  Opaque),
--                        Diffuse  => (White, Opaque));
--        --  Specular => (White, Opaque));
--
--        Light.Position_is       (initial_Site);
--        Light.cone_Direction_is (cone_Direction);
--
--        Demo.Renderer.Light_is (Id => 1, Now => Light);
--     end;



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

      the_Visuals (3).Site_is ((0.0, 0.0, -50.0));

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

         --  Render all visuals.
         --
         Demo.Camera.render ((1 => the_Visuals (Current)));

         while not Demo.Camera.cull_Completed
         loop
            delay Duration'Small;
         end loop;

         Demo.Renderer.render;
         Demo.FPS_Counter.increment;    -- Frames per second display.

         delay 1.0 / 60.0;
      end loop;
   end;

   Demo.destroy;
end launch_render_Models;
