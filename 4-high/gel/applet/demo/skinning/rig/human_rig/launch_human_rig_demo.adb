with
     gel.Window.setup,
     gel.Applet.gui_world,
     gel.Camera,
     gel.Rig,
     gel.Forge,

     openGL.Model.any,
     openGL.Light,

     ada.command_Line,
     ada.Calendar,
     ada.Exceptions,
     ada.Text_IO;

pragma unreferenced (gel.Window.setup);


procedure launch_human_rig_Demo
--
-- Rigged human model with two animations (jumping and golf swing).
--
is
   use ada.Exceptions,
       ada.Text_IO;

   no_Model : exception;

begin
   new_Line;
   put_Line ("Usage: $ ./launch_human_rig_demo <model_name>");
   new_Line;
   put_Line ("model_name := (golfer | jumper)");
   new_Line;

   declare
      use gel.Rig,
          gel.Math,
          gel.linear_Algebra_3D,
          openGL,
          ada.command_Line,
          ada.Calendar;

      Arg : constant String := (if argument_Count = 0 then raise no_Model with "No model specified."
                                                      else Argument (1));

      model_Name : constant String := (if    Arg = "golfer" then "assets/human-animation-golf.dae"
                                       elsif Arg = "jumper" then "assets/human-animation-jump.dae"
                                       else                      raise no_Model with "No model exists for " & Arg);
      -------------
      --- Variables
      --
      the_Applet    : constant gel.Applet.gui_World.view     := gel.Forge.new_gui_Applet ("Rig Demo", 1536, 864);
      the_Rig       : aliased  gel.Rig.item;
      the_rig_Model : aliased constant openGL.Model.any.view := openGL.Model.any.new_Model (Model            => openGL.to_Asset (model_Name),
                                                                                            Texture          => openGL.null_Asset,
                                                                                            Texture_is_lucid => False);
      next_render_Time : ada.calendar.Time := ada.calendar.Clock;

   begin
      the_Applet.gui_Camera.Site_is ((0.0, -10.0, 1.0));                                  -- Position the camera.
      the_Applet.gui_Camera.Spin_is (x_Rotation_from (to_Radians (-90.0)));               -- Rotate   the camera.

      the_Applet.enable_simple_Dolly (1);                                                 -- Enable user camera control via keyboard.
      the_Applet.Dolly.Speed_is (0.05);                                                   -- Slow down dolly movement.

      -- Set the lights position and ambient color.
      --
      declare
         Light : openGL.Light.item := the_Applet.Renderer.new_Light;
      begin
         Light.Color_is ((255.0 / 255.0,
                          153.0 / 255.0,
                          102.0 / 255.0));

         Light.Site_is ((1000.0, 0.0, 1000.0));

         the_Applet.Renderer.set (Light);
      end;

      the_Rig.define (the_Applet.gui_World,
                      the_rig_Model.all'Access,
                      Mass         => 0.0,
                      is_Kinematic => False);

      the_Rig   .Spin_is       (x_Rotation_from (to_Radians (-90.0)));            -- Orientate the rig.
      the_Applet.gui_World.add (the_Rig.base_Sprite, and_Children => True);       -- Add the rigs armature sprite.

      the_Rig.enable_Graphics;
      the_Rig.motion_Mode_is (Animation);
      the_Rig.assume_Pose;

      while the_Applet.is_open
      loop
         the_Applet.gui_World.evolve;                                             -- Evolve the world.
         the_Rig             .evolve (world_Age => the_Applet.gui_World.Age);     -- Evolve the rig.
         the_Rig.assume_Pose;
         the_Applet.freshen;                                                      -- Handle any new events and update the screen.

         next_render_Time := next_render_Time + 1.0/60.0;
         delay until next_render_Time;
      end loop;

      the_Applet.destroy;
   end;

exception
   when E : no_Model => put_Line (exception_Message (E));
                        new_Line;
end launch_human_rig_Demo;
