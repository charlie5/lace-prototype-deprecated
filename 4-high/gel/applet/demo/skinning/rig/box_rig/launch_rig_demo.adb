with
     gel.Window.setup,
     gel.Applet.gui_world,
     gel.World,
     gel.Camera,
     gel.Sprite,
     gel.Rig,
     gel.Forge,

     openGL.Model.any,
     openGL.Light,

     ada.Calendar;

pragma unreferenced (gel.Window.setup);


procedure launch_rig_Demo
--
-- Simple rigged box model with two animated bones.
--
--
is
   use gel.Rig,
       openGL,
       ada.Calendar;

   -----------
   --- Utility
   --

   --  function "+" (From : in String) return ada.strings.unbounded.unbounded_String
   --    renames ada.strings.unbounded.To_unbounded_String;

   -------------
   --- Variables
   --
   the_Applet    : constant gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("Rig Demo", 1536, 864);

   the_Ground    : constant gel.Sprite.view           := gel.Forge.new_box_Sprite (the_Applet.gui_World,
                                                                                   Mass => 0.0,
                                                                                   Size => (50.0, 1.0, 50.0));

   the_rig_Model : aliased constant openGL.Model.any.view := openGL.Model.any.new_Model (--Scale            => (1.0, 1.0, 1.0),
                                                                                         --  Model            => openGL.to_Asset ("./tarantula-rigged.dae"),
                                                                                         Model            => openGL.to_Asset ("./box_1_bone.dae"),
                                                                                         --  Model            => openGL.to_Asset ("./box_2_bone.dae"),
                                                                                         Texture          => openGL.null_Asset,
                                                                                         Texture_is_lucid => False);
   the_Rig       : aliased  gel.Rig.item;

   next_render_Time : ada.calendar.Time;

begin
   the_Applet.gui_Camera.Site_is ((0.0, 0.0, 10.0));     -- Position the camera

   the_Applet.enable_simple_Dolly (1);                   -- Enable user camera control via keyboards
   the_Applet.Dolly.Speed_is (0.05);

   the_Applet.enable_Mouse (detect_Motion => False);     -- Enable mouse events.

   the_Applet.gui_World.Gravity_is ((0.0, -0.0, 0.0));

   -- Set the lights position and ambient color.
   --
   declare
      Light : openGL.Light.item := the_Applet.Renderer.new_Light;
   begin
      Light.Color_is ((0.5, 0.9, 0.5));
      Light. Site_is ((1000.0, 1000.0, 1000.0));

      the_Applet.Renderer.set (Light);
   end;


   --  declare
   --     leaf_bone_Lengths : bone_id_Map_of_details;
   --  begin
   --     leaf_bone_Lengths.insert (+"head", to_Details (length => 0.13));
   --     leaf_bone_Lengths.insert (+"jaw",  to_Details (length => 0.11));
   --
   --     leaf_bone_Lengths.insert (+"eye_L", to_Details (length => 0.015));
   --     leaf_bone_Lengths.insert (+"eye_R", to_Details (length => 0.015));
   --
   --     leaf_bone_Lengths.insert (+"toe_L", to_Details (length => 0.06));
   --     leaf_bone_Lengths.insert (+"toe_R", to_Details (length => 0.06));
   --
   --     leaf_bone_Lengths.insert (+"thumb_02_L", to_Details (length => 0.02));
   --     leaf_bone_Lengths.insert (+"thumb_02_R", to_Details (length => 0.02));
   --
   --     leaf_bone_Lengths.insert (+"foot_L", to_Details (yaw_Limits => (to_Radians (-0.0),
   --                                                                     to_Radians ( 0.0))));
   --     leaf_bone_Lengths.insert (+"foot_R", to_Details (yaw_Limits => (to_Radians (-0.0),
   --                                                                     to_Radians ( 0.0))));
   --
   --
   --     leaf_bone_Lengths.insert (+"forearm_L", to_Details (yaw_Limits   => (to_Radians (-40.0),
   --                                                                          to_Radians ( 40.0)),
   --                                                         pitch_Limits => (to_Radians (-40.0),
   --                                                                          to_Radians ( 40.0))));
   --
   --     leaf_bone_Lengths.insert (+"upper_arm_L", to_Details (yaw_Limits   => (to_Radians (-40.0),
   --                                                                            to_Radians ( 40.0)),
   --                                                           pitch_Limits => (to_Radians (-40.0),
   --                                                                            to_Radians ( 40.0))));
      the_Rig.define (the_Applet.gui_World,
                      the_rig_Model.all'Access,
                      Mass         => 1.0,
                      --  bone_Details => leaf_bone_Lengths,
                      is_Kinematic => False);
   --  end;

   the_Ground.Site_is ((0.0,  -4.0,  0.0));
   --  the_Rig   .Spin_is (x_Rotation_from (to_Radians (-90.0)));

   the_Applet.gui_World.add (the_Rig.base_Sprite, and_Children => True);     -- Add the rigs armature sprite.
   the_Applet.gui_World.add (the_Ground,          and_Children => False);    -- Add the ground        sprite.

   the_Rig.enable_Graphics;

   next_render_Time := ada.Calendar.clock;

   while the_Applet.is_open
   loop
      the_Applet.gui_World.evolve;                                             -- Evolve the world.
      the_Rig             .evolve (world_Age => the_Applet.gui_World.Age);     -- Evolve the rig.
      the_Applet.freshen;                                                      -- Handle any new events and update the screen.

      next_render_Time := next_render_Time + gel.World.evolve_Period;
      delay until next_render_Time;
   end loop;

   the_Applet.destroy;
end launch_rig_Demo;
