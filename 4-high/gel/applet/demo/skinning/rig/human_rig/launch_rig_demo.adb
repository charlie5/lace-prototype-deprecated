with collada.Document,
     collada.Library.geometries,
     collada.Library.animations,
     collada.Library.controllers;

with gel.Rig;

with gel.Window.setup,
     gel.Applet.gui_world,
     gel.Camera;

with gel.Sprite,
     gel.Forge,
     openGL.Model,
     openGL.Model.box,
     openGL.Model.any;

with opengl.Palette,
     opengl.IO,
     ada.Calendar,
     ada.Strings.fixed;

with float_math.algebra.linear.d3;   use  float_math.algebra.linear.d3;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with physics.Model;
--  with physics.Motor.spring.angular;



procedure launch_rig_Demo
--
-- Simple rigged box model with two animated bones.
--
--
is
   package Math renames float_Math;

   use gel.Applet,    openGL.Model.box,  gel.Rig,
       openGL,        opengl.Palette,
       Math,
       ada.Calendar,  ada.Strings,    ada.Strings.fixed;

   use type math.Real,  opengl.Real;


   --- Utility
   --

   function "+" (From : in ada.strings.unbounded.unbounded_String) return String
     renames ada.strings.unbounded.to_String;

   function "+" (From : in String) return ada.strings.unbounded.unbounded_String
     renames ada.strings.unbounded.To_unbounded_String;



   the_Applet       : constant gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("animated box Model", 1920, 1200);

   the_Ground       : constant gel.Sprite.view           := gel.Forge.new_box_Sprite (the_Applet.gui_World,
                                                                                      mass => 0.0,
                                                                                      size => (50.0, 1.0, 50.0));

   the_rig_Model    : aliased  openGL.Model.any.view := openGL.Model.any.new_Model (--Scale            => (1.0, 1.0, 1.0),
--                                                                                                   Model            => openGL.to_Asset ("./tarantula-rigged.dae"),
                                                                                    Model            => openGL.to_Asset ("./assets/chimp.dae"),
--                                                                                                   Model            => openGL.to_Asset ("./assets/chimp.obj"),
--                                                                                                   Model            => openGL.to_Asset ("./assets/chimpanzee.obj"),
                                                                                    --  math_Model       => null,
                                                                                    Texture          => openGL.null_Asset,
                                                                                    Texture_is_lucid => False);
   the_Rig          : aliased  gel.Rig.item;

   Counter          :         Integer          := 0;
   next_render_Time :         ada.calendar.Time;
   start_Time       :         ada.Calendar.Time;

   --  the_Motor        : Physics.Motor.spring.angular.item;

   the_ref_Bone : gel.Sprite.view := gel.forge.new_box_Sprite (in_World => the_Applet.gui_World,
                                                               Mass     => 1.0, -- Mass,
                                                               Size     => (3.0, 2.0, 1.0),
                                                               Colors   => (1      => White,
                                                                            3      => Green,
                                                                            4      => Blue,
                                                                            others => Red));
begin
--     the_Applet.gui_Camera.Site_is ((0.0, 0.0, 2.0));             -- Position the camera
   the_Applet.gui_Camera.Site_is ((0.0, 0.0, 10.0));             -- Position the camera
--     the_Applet.gui_Camera.Site_is ((0.0, -3.0, 3.5));             -- Position the camera

   the_Applet.enable_simple_Dolly (1);                          -- Enable user camera control via keyboards
   the_Applet.Dolly.Speed_is (0.05);

   the_Applet.enable_Mouse (detect_Motion => False);            -- Enable mouse events.

   the_Applet.gui_World.Gravity_is ((0.0, -0.1, 0.0));


   -- Add reference bone/box.
   --
   declare
   begin
      the_Applet.gui_World.add (the_ref_Bone);
      the_ref_Bone.Site_is ((3.0,  5.0,  0.0));
   end;



   declare
      leaf_bone_Lengths : bone_id_Map_of_details;
   begin
      --        leaf_bone_Lengths.insert (+"Bone", 1.0);

      leaf_bone_Lengths.insert (+"head", to_Details (length => 0.13));
      leaf_bone_Lengths.insert (+"jaw",  to_Details (length => 0.11));

      leaf_bone_Lengths.insert (+"eye_L", to_Details (length => 0.015));
      leaf_bone_Lengths.insert (+"eye_R", to_Details (length => 0.015));

      leaf_bone_Lengths.insert (+"toe_L", to_Details (length => 0.06));
      leaf_bone_Lengths.insert (+"toe_R", to_Details (length => 0.06));

      leaf_bone_Lengths.insert (+"thumb_02_L", to_Details (length => 0.02));
      leaf_bone_Lengths.insert (+"thumb_02_R", to_Details (length => 0.02));

      leaf_bone_Lengths.insert (+"foot_L", to_Details (yaw_Limits => (to_Radians (-0.0),
                                                                      to_Radians ( 0.0))));
      leaf_bone_Lengths.insert (+"foot_R", to_Details (yaw_Limits => (to_Radians (-0.0),
                                                                      to_Radians ( 0.0))));


      leaf_bone_Lengths.insert (+"forearm_L", to_Details (yaw_Limits   => (to_Radians (-40.0),
                                                                           to_Radians ( 40.0)),
                                                          pitch_Limits => (to_Radians (-40.0),
                                                                           to_Radians ( 40.0))));

      leaf_bone_Lengths.insert (+"upper_arm_L", to_Details (yaw_Limits   => (to_Radians (-40.0),
                                                                           to_Radians ( 40.0)),
                                                          pitch_Limits => (to_Radians (-40.0),
                                                                           to_Radians ( 40.0))));


      the_Rig.define (the_Applet.gui_World,
                      the_rig_Model.all'Access,
                      mass         => 1.0,
                      bone_Details => leaf_bone_Lengths,
                      is_Kinematic => False);
--                        is_Kinematic => True);

--        the_Rig.motion_Mode_is (Animation);
   end;

--     the_Rig.Site_is ((0.0, 5.0, 0.0));
   the_Rig.Spin_is (x_Rotation_from (to_Radians (-90.0)));

--     the_Rig.base_Sprite.move (to_Site => (2.0, 0.0, 0.0));
--     the_Rig.base_Sprite.rotate (x_Rotation_from (to_Radians (-90.0)));

   the_Applet.gui_World.add (the_Rig.base_Sprite, and_Children => True);     -- Add the rigs armature sprite.

--     declare
--        use gel.physics_Model;
--
--        the_Rig : gel.Sprite.view := gel.Sprite.Forge.new_Sprite (Name           => "kkk",
--                                                                  World          => the_Applet.gui_World,
--                                                                  graphics_Model => the_rig_Model'unchecked_Access,
--                                                                  physics_Model  => gel.physics_Model.Forge.new_physics_Model (Id          => 11,
--                                                                                                                               shape_Info  => (a_Sphere, sphere_radius => 1.0),
--                                                                                                                               Scale       => (1.0, 1.0, 1.0),
--                                                                                                                               Mass        => 0.0,
--                                                                                                                               Friction    => 0.5,
--                                                                                                                               Restitution => 0.5,
--                                                                                                                               is_Tangible => True),
--                                                                  owns_Graphics  => True,
--                                                                  owns_Physics   => True,
--                                                                  is_Kinematic   => False);
--     begin
--        the_Applet.gui_World.add (the_Rig, and_Children => True);     -- Add the rigs armature sprite.
--     end;


--     the_Applet.gui_World.add (the_Box.skin_Sprite, and_Children => True);     -- Add the rigs skin     sprite.


   the_Applet.gui_World.add (the_Ground);                                    -- Add the 'ground' box
   the_Ground.Site_is ((0.0,  -4.0,  0.0));


   the_Rig.enable_Graphics;

--     the_Motor.Rigid := the_Rig.Sprite (for_Bone => +"hips").Solid;
   --  the_Motor.Rigid := the_ref_Bone.Solid;

   declare
      desired_Rotation : math.Matrix_3x3 := Identity_3x3; -- x_Rotation_from (math.to_Radians (0.0));
   begin
--        the_Motor.desiredForward := (0.0, 0.0,  -1.0);   -- the Motor's desired forward direction, part of the desired orientation.
--        the_Motor.desiredUp      := (0.0, 1.0,  0.0);   -- the Motor's desired up      direction.
--        the_Motor.desiredRight   := (1.0, 0.0,  0.0);   -- the Motor's desired right   direction.

      null;
      --  the_Motor.desiredForward :=  Row (desired_Rotation, 3);
      --  the_Motor.desiredUp      :=  Row (desired_Rotation, 2);
      --  the_Motor.desiredRight   :=  Row (desired_Rotation, 1);
   end;

   --  the_Motor.angularKd      := 0.000_1;    -- the damping constant for angular mode.
   --  the_Motor.angularKs      := 1.0;        -- the spring  constant for angular mode.
   --  the_Motor.is_Enabled     := True;

   start_Time       := ada.calendar.Clock;
   next_render_Time := ada.Calendar.clock;

   while the_Applet.is_open
   loop
      Counter := Counter + 1;

--        if Counter = 200
--        then
--           the_Rig.assume_Pose;
--        end if;

      the_Applet.gui_World.evolve (by => 1.0/60.0);    -- Evolve the world.

      the_Rig             .evolve (world_Age => the_Applet.gui_World.Age);
      --        the_Rig             .evolve (world_Age => 5.0);

--        if Counter mod 1 = 0
--        then
         --  the_Motor.update;
--        end if;

--        declare
--           the_Spin : Matrix_3x3 := the_Rig.base_Sprite

--  --        the_Rig.base_Sprite .rotate (Math.Identity_3x3);
--        the_Rig.base_Sprite.child_Joints (1).Sprite_B.rotate (z_Rotation_from (to_Radians (0.0)));
--        the_Rig.base_Sprite.child_Joints (1).Sprite_B.Spin_is (z_Rotation_from (to_Radians (0.0)));
--        the_Rig.base_Sprite.child_Joints (1).Sprite_B.child_Joints (1).Sprite_B.Spin_is (z_Rotation_from (to_Radians (0.0)));
--        the_Rig.base_Sprite.child_Joints (1).Sprite_B.child_Joints (1).Sprite_B.child_Joints (1).Sprite_B.Spin_is (z_Rotation_from (to_Radians (0.0)));

      if Counter mod 200 = 0
      then
         declare
            procedure apply_random_force (the_Sprite : in out gel.Sprite.item'Class)
            is
            begin
               the_Sprite.apply_Force ((-the_Sprite.Site (1) * 0.1, 2.0, 0.0));
            end apply_random_force;

         begin
            null; -- the_Rig.base_Sprite.apply (apply_random_force'unrestricted_Access);
         end;
      end if;

      declare
         procedure make_2d (the_Sprite : in out gel.Sprite.item'Class)
         is
         begin
            the_Sprite.Site_is ((the_Sprite.Site (1),
                                 the_Sprite.Site (2),
                                 0.0));
         end make_2d;
      begin
         null; -- the_Rig.base_Sprite.apply (make_2d'unrestricted_Access);
      end;


      the_Applet.freshen;                              -- Handle any new events and update the screen.


      next_render_Time := next_render_Time + 1.0/60.0;
      delay until next_render_Time;
--  raise Program_Error;
   end loop;


   the_Applet.destroy;


exception
   when E : others =>
      put_Line (Exception_Information (E));
end launch_rig_Demo;
