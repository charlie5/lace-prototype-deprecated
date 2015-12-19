with collada.Document,
     collada.Library.geometries,
     collada.Library.animations,
     collada.Library.controllers;

with mmi.Rig;

with mmi.Window.setup,
     mmi.Applet.gui_world,
     mmi.Camera;

with mmi.Sprite,
     mmi.Forge,
     openGL.Model,
     openGL.Model.box,
     openGL.Model.open_gl;

with opengl.Palette,
     opengl.IO,
     ada.Calendar,
     ada.Strings.fixed;

with float_math.algebra.linear.d3;   use  float_math.algebra.linear.d3;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;



procedure launch_rig_Demo
--
-- Simple rigged box model with two animated bones.
--
--
is
   package Math renames float_Math;

   use mmi.Applet,    openGL.Model.box,  mmi.Rig,
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



   the_Applet       : constant mmi.Applet.gui_World.view := mmi.Forge.new_gui_Applet ("animated box Model", 1920, 1200);

   the_Ground       : constant mmi.Sprite.view           := mmi.Forge.new_box_Sprite (the_Applet.gui_World,
                                                                                      mass => 0.0,
                                                                                      size => (50.0, 1.0, 50.0));

   the_rig_Model    : aliased  openGL.Model.open_gl.item := openGL.Model.open_gl.Forge.to_Model (Scale            => (1.0, 1.0, 1.0),
                                                                                                 Model            => openGL.to_Asset ("./tarantula-rigged.dae"),
                                                                                                 math_Model       => null,
                                                                                                 Texture          => openGL.null_Asset,
                                                                                                 Texture_is_lucid => False);
   the_Rig          : aliased  mmi.Rig.item;

   Counter          :         Integer          := 0;
   next_render_Time :         ada.calendar.Time;
   start_Time       :         ada.Calendar.Time;

begin
   the_Applet.gui_Camera.Site_is ((0.0, 0.0, 10.0));             -- Position the camera

   the_Applet.enable_simple_Dolly (1);                           -- Enable user camera control via keyboards
   the_Applet.Dolly.Speed_is (0.05);

   the_Applet.enable_Mouse (detect_Motion => False);             -- Enable mouse events.

   the_Applet.gui_World.Gravity_is ((0.0, -0.5, 0.0));


   declare
      leaf_bone_Lengths : bone_id_Map_of_details;
   begin
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
                      the_rig_Model'unchecked_Access,
                      mass         => 1.0,
                      bone_Details => leaf_bone_Lengths,
                      is_Kinematic => False);
   end;

   the_Ground.Site_is ((0.0,  -4.0,  0.0));
   the_Rig   .Spin_is (x_Rotation_from (to_Radians (-90.0)));

   the_Applet.gui_World.add (the_Rig.base_Sprite, and_Children => True);     -- Add the rigs armature sprite.
   the_Applet.gui_World.add (the_Ground,          and_Children => False);    -- Add the ground        sprite.

   the_Rig.enable_Graphics;


   start_Time       := ada.calendar.Clock;
   next_render_Time := ada.Calendar.clock;

   while the_Applet.is_open
   loop
      Counter := Counter + 1;

      the_Applet.gui_World.evolve (by => 1.0/60.0);                            -- Evolve the world.
      the_Rig             .evolve (world_Age => the_Applet.gui_World.Age);     -- Evolve the rig.
      the_Applet.freshen;                                                      -- Handle any new events and update the screen.

      next_render_Time := next_render_Time + 1.0/60.0;
      delay until next_render_Time;
   end loop;

   the_Applet.destroy;

exception
   when E : others =>
      put_Line (Exception_Information (E));
end launch_rig_Demo;
