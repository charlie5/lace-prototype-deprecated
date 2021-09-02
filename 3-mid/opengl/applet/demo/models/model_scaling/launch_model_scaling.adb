with
     openGL.Visual,
     openGL.Demo,
     openGL.Model;

procedure launch_Model_scaling
--
--  Exercise the scaling of models.
--
is
   use openGL,
       openGL.Math,
       openGL.linear_Algebra_3d;
begin
   Demo.print_Usage;
   Demo.define ("openGL 'Model Scaling' Demo");
   Demo.Camera.Position_is ((0.0, 0.0, 20.0),
                            y_Rotation_from (to_Radians (0.0)));
   declare
      --  The models.
      --
      the_Models : constant openGL.Model.views := openGL.Demo.Models;

      --  The visuals.
      --
      the_Visuals : openGL.Visual.views (the_Models'Range);

      --  Scaling
      --
      scaling_Up  : Boolean       := True;
      Scale       : math.Vector_3 := (1.0, 1.0, 1.0);

   begin
      for i in the_Visuals'Range
      loop
         the_Visuals (i) := Visual.Forge.new_Visual (the_Models (i));
      end loop;

      Demo.layout (the_Visuals);


      --  Main loop.
      --
      while not Demo.Done
      loop
         if scaling_Up then Scale := Scale + (0.001, 0.001, 0.001);
                       else Scale := Scale - (0.001, 0.001, 0.001);
         end if;

         if    Scale (1) > 2.0   then scaling_Up := False;
         elsif Scale (1) < 0.002 then scaling_Up := True;
         end if;

         for Each of the_Visuals
         loop
            Each.Model.Scale := Scale;
         end loop;

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
end launch_Model_scaling;
