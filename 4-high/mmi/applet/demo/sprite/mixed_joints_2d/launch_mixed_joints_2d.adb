with
     mmi.Window.lumen,
     mmi.Applet.gui_world,

     mmi.Forge,
     mmi.Sprite,

     mmi.Joint,
     mmi.hinge_Joint,
     mmi.ball_Joint,
     mmi.cone_twist_Joint,
     mmi.slider_Joint,
     mmi.any_Joint,

     opengl.Palette,
     Physics,

     float_math.Algebra.linear.d3;

pragma Unreferenced (mmi.Window.lumen);
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Physics.Forge;


procedure launch_mixed_Joints_2d
--
--  Drops a variety of shapes a plane terrain.
--
--
is
   package Math renames float_Math;

   use mmi.Applet,
       openGL,
       opengl.Palette;

   use type openGL.Real;

   the_Applet : constant mmi.Applet.gui_World.view := mmi.Forge.new_gui_Applet ("hinged Box",
                                                                                1920, 1200,
                                                                                space_Kind => Physics.Forge.Box2D);
   Counter    :          Natural                   := 0;

begin
   the_Applet.gui_World .Gravity_is    ((0.0, -10.0, 0.0));
   the_Applet.gui_Camera.Site_is       ((0.0, 4.0, 30.0));      -- Position the camera.
   the_Applet.Renderer  .Background_is (Grey);
   the_Applet.enable_simple_Dolly      (1);                     -- Enable user camera control via keyboard.

   --  Add joints.
   --
   declare
      use mmi.Forge,
          float_Math,
          float_math.Algebra.linear.d3;
   begin
      --  Hinge
      --
      declare
         use mmi.any_Joint,
             math.Vectors;

         the_hinge_Box_1 : constant mmi.Sprite.view := new_circle_Sprite (the_Applet.gui_World, mass => 0.0);
         the_hinge_Box_2 : constant mmi.Sprite.view := new_circle_Sprite (the_Applet.gui_World, mass => 1.0);
--           the_hinge_Joint :          mmi.hinge_Joint .view; -- := new mmi.hinge_Joint.item;
         new_Joint       :          mmi.      Joint .view;

         Frame_A : constant math.Matrix_4x4 := math.Identity_4x4;
         Frame_B : constant math.Matrix_4x4 := math.Identity_4x4;
      begin
         the_hinge_Box_1.Site_is (( 0.0,  0.0,  0.0));
         the_hinge_Box_2.Site_is ((-10.0, 0.0,  0.0));


         the_hinge_Box_1.attach_via_Hinge (the_Child         => the_hinge_Box_2,
                                           Frame_in_parent   => Frame_A,
                                           Frame_in_child    => Frame_B,
                                           Limits            => (0.0, to_Radians (355.0)),
                                           collide_Connected => False,
                                           new_joint         => new_Joint);
--           the_hinge_Joint := mmi.hinge_Joint .view (new_Joint);

         the_Applet.gui_World.add (the_hinge_Box_1, and_children => True);
      end;

--        --  DoF6
--        --
--        declare
--           use mmi.any_Joint,
--               math.Vectors;
--
--           the_dof6_Box_1  : constant mmi.Sprite.local.view := new_box_Sprite (the_Applet.gui_World, mass => 0.0);
--           the_dof6_Box_2  : constant mmi.Sprite.local.view := new_box_Sprite (the_Applet.gui_World);
--           the_DoF6_Joint  :          mmi.any_Joint   .view := new mmi.any_Joint.item;
--
--           Frame_A : math.Matrix_4x4 := math.Identity_4x4;
--           Frame_B : math.Matrix_4x4 := math.Identity_4x4;
--        begin
--           set_Translation (Frame_B, (  2.0, 2.0, 0.0));
--
--           the_dof6_Joint.define (the_Applet.gui_World.Physics,
--                                  the_dof6_Box_1,  the_dof6_Box_2,
--                                  Frame_A,         Frame_B);
--
--           the_dof6_Box_1.Site_is ((-20.0,        0.0,  0.0));
--           the_dof6_Box_2.Site_is ((-20.0 - 2.0,  0.0,  2.0));
--
--           the_Applet.gui_World.add (the_dof6_Box_1);
--           the_Applet.gui_World.add (the_dof6_Box_2);
--           the_Applet.gui_World.add (the_dof6_Joint.all'Access);
--        end;
--
--        --  Ball
--        --
--        declare
--           use mmi.any_Joint,
--               math.Vectors;
--
--           the_ball_Box_1  : constant mmi.Sprite.local.view := new_box_Sprite (the_Applet.gui_World, mass => 0.0);
--           the_ball_Box_2  : constant mmi.Sprite.local.view := new_box_Sprite (the_Applet.gui_World);
--           the_ball_Joint  :          mmi.ball_Joint  .view := new mmi.ball_Joint.item;
--
--           Pivot_in_A : math.Vector_3 := (0.0, -1.0, 0.0);
--           Pivot_in_B : math.Vector_3 := (0.0,  1.0, 0.0);
--        begin
--           the_ball_Joint.define (the_Applet.gui_World.Physics,
--                                  the_ball_Box_1,  the_ball_Box_2,
--                                  Pivot_in_A,      Pivot_in_B);
--
--           the_ball_Box_1.Site_is ((-10.0,        0.0,  0.0));
--           the_ball_Box_2.Site_is ((-10.0 - 2.0,  0.0,  2.0));
--
--           the_Applet.gui_World.add (the_ball_Box_1);
--           the_Applet.gui_World.add (the_ball_Box_2);
--           the_Applet.gui_World.add (the_ball_Joint.all'Access);
--        end;
--
--        --  Slider
--        --
--        declare
--           use mmi.any_Joint,
--               math.Vectors;
--
--           the_slider_Box_1  : constant mmi.Sprite.local.view := new_box_Sprite (the_Applet.gui_World, mass => 0.0);
--           the_slider_Box_2  : constant mmi.Sprite.local.view := new_box_Sprite (the_Applet.gui_World);
--           the_slider_Joint  :          mmi.slider_Joint.view := new mmi.slider_Joint.item;
--
--           Frame_A : math.Matrix_4x4 := math.Identity_4x4;
--           Frame_B : math.Matrix_4x4 := math.Identity_4x4;
--
--           y_Rot   : math.Matrix_3x3 := y_Rotation_from (to_Radians (45.0));
--           x_Rot   : math.Matrix_3x3 := x_Rotation_from (to_Radians (45.0));
--        begin
--           set_Translation (Frame_A, (  -4.0, 4.0, -4.0));
--           set_Translation (Frame_B, (  4.0, 0.0, 0.0));
--  --           set_Rotation    (Frame_A, x_Rot);
--  --           set_Rotation    (Frame_B, x_Rot);
--
--           the_slider_Joint.define (the_Applet.gui_World.Physics,
--                                    the_slider_Box_1,  the_slider_Box_2,
--                                    Frame_A,           Frame_B);
--
--           the_slider_Box_1.Site_is ((-10.0,        10.0,  0.0));
--           the_slider_Box_2.Site_is ((-10.0 - 2.0,  15.0,  0.0));
--
--           the_Applet.gui_World.add (the_slider_Box_1);
--           the_Applet.gui_World.add (the_slider_Box_2);
--           the_Applet.gui_World.add (the_slider_Joint.all'Access);
--        end;
--
--        --  cone Twist
--        --
--        declare
--           use mmi.any_Joint,
--               math.Vectors;
--
--           the_cone_twist_Box_1  : constant mmi.Sprite.local.view     := new_box_Sprite (the_Applet.gui_World, mass => 0.0);
--           the_cone_twist_Box_2  : constant mmi.Sprite.local.view     := new_box_Sprite (the_Applet.gui_World);
--           the_cone_twist_Joint  :          mmi.cone_twist_Joint.view := new mmi.cone_twist_Joint.item;
--
--           Frame_A : math.Matrix_4x4 := math.Identity_4x4;
--           Frame_B : math.Matrix_4x4 := math.Identity_4x4;
--
--           y_Rot   : math.Matrix_3x3 := y_Rotation_from (to_Radians (45.0));
--           x_Rot   : math.Matrix_3x3 := x_Rotation_from (to_Radians (45.0));
--        begin
--  --           set_Translation (Frame_A, (  -4.0, 4.0, -4.0));
--           set_Translation (Frame_B, (  4.0, 0.0, 0.0));
--  --           set_Rotation    (Frame_A, x_Rot);
--  --           set_Rotation    (Frame_B, x_Rot);
--
--           the_cone_twist_Joint.define (the_Applet.gui_World.Physics,
--                                        the_cone_twist_Box_1,  the_cone_twist_Box_2,
--                                        Frame_A,           Frame_B);
--
--           the_cone_twist_Box_1.Site_is ((10.0,        10.0,  0.0));
--           the_cone_twist_Box_2.Site_is ((10.0 + 2.0,  10.0,  0.0));
--
--           the_Applet.gui_World.add (the_cone_twist_Box_1);
--           the_Applet.gui_World.add (the_cone_twist_Box_2);
--           the_Applet.gui_World.add (the_cone_twist_Joint.all'Access);
--        end;

      declare
         Added : Boolean := True;
      begin
         while the_Applet.is_open
         loop
            Counter := Counter + 1;

--              if false -- Counter mod (1*60) = 0
--              then
--                 if Added then
--                    the_Applet.gui_World.rid (the_hinge_Joint.all'Access);
--  --                    the_Applet.gui_World.rid (the_hinge_Box_1);
--  --                    the_Applet.gui_World.rid (the_hinge_Box_2);
--                    Added := False;
--                 else
--  --                    the_Applet.gui_World.add (the_hinge_Box_1);
--  --                    the_Applet.gui_World.add (the_hinge_Box_2);
--                    the_Applet.gui_World.add (the_hinge_Joint.all'Access);
--                    Added := True;
--                 end if;
--              end if;

            the_Applet.freshen;     -- Handle any new events and update the screen.
         end loop;

         the_Applet.destroy;
      end;

   end;


exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main thread !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_mixed_Joints_2d;
