with
     gel.Window.setup,
     gel.Applet.gui_world,
     gel.Camera,
     gel.Rig,
     gel.Forge,

     openGL.Model.any,
     openGL.Light.directional,

     ada.Calendar;

pragma unreferenced (gel.Window.setup);


procedure launch_box_rig_1_bone_Demo
--
-- Simple rigged box model with one animated bone.
--
is
   use gel.Rig,
       gel.Math,
       gel.linear_Algebra_3D,
       openGL,
       ada.Calendar;

   -------------
   --- Variables
   --
   the_Applet    : constant gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("Rig Demo", 1536, 864);
   the_rig_Model : constant openGL.Model.any.view     := openGL.Model.any.new_Model (-- Model            => openGL.to_Asset ("./box_1_bone.dae"),
                                                                                     --  Model            => openGL.to_Asset ("./box_1_bone-animated.dae"),
                                                                                     --  Model            => openGL.to_Asset ("./box_2_bone.dae"),
                                                                                     --  Model            => openGL.to_Asset ("./box_3_bone.dae"),
                                                                                     Model            => openGL.to_Asset ("./human-default-animated-01_01.dae"),
                                                                                     Texture          => openGL.null_Asset,
                                                                                     Texture_is_lucid => False);
   the_Rig          : gel.Rig.item;
   next_render_Time : ada.calendar.Time;

begin
   the_Applet.gui_Camera.Site_is ((0.0, -10.0, 1.0));                        -- Position the camera.
   the_Applet.gui_Camera.Spin_is (x_Rotation_from (to_Radians (-90.0)));     -- Rotate   the camera.

   the_Applet.enable_simple_Dolly (1);                                       -- Enable user camera control via keyboards
   the_Applet.Dolly.Speed_is (0.05);

   the_Applet.gui_World.Gravity_is ((0.0, -9.0, 0.0));

   -- Set the lights position and ambient color.
   --
   declare
      Light : openGL.Light.directional.item := the_Applet.Renderer.Light (Id => 1);
   begin
      Light.ambient_Color_is ((255.0 / 255.0,
                               153.0 / 255.0,
                               102.0 / 255.0,
                                 1.0));
      Light.Site_is          ((1000.0, 000.0, 1000.0));

      the_Applet.Renderer.Light_is (Id => 1, Now => Light);
   end;


   the_Rig.define (the_Applet.gui_World,
                   the_rig_Model.all'Access,
                   Mass         => 0.0,
                   is_Kinematic => False);

   the_Rig.Spin_is (x_Rotation_from (to_Radians (-90.0)));

   the_Applet.gui_World.add (the_Rig.base_Sprite, and_Children => True);     -- Add the rigs armature sprite.

   the_Rig.enable_Graphics;
   the_Rig.motion_Mode_is (Animation);
   the_Rig.assume_Pose;

   next_render_Time := ada.Calendar.clock;

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
end launch_box_rig_1_bone_Demo;
