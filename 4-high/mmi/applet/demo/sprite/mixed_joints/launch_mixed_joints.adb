with
     mmi.Window.lumen,
     mmi.Applet.gui_world,

     mmi.Forge,
     mmi.Sprite,

     mmi.hinge_Joint,
     mmi.ball_Joint,
     mmi.cone_twist_Joint,
     mmi.slider_Joint,
     mmi.any_Joint,

     opengl.Palette,
     Physics,

     float_math.Algebra.linear.d3;

pragma Unreferenced (mmi.Window.lumen);


procedure launch_mixed_Joints
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

   the_Applet : constant mmi.Applet.gui_World.view := mmi.Forge.new_gui_Applet ("hinged Box", 1920, 1200);
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

         the_hinge_Box_1 : constant mmi.Sprite.view := new_box_Sprite (the_Applet.gui_World, mass => 0.0);
         the_hinge_Box_2 : constant mmi.Sprite.view := new_box_Sprite (the_Applet.gui_World);
         the_hinge_Joint :          mmi.hinge_Joint .view := new mmi.hinge_Joint.item;

         Frame_A : math.Matrix_4x4 := math.Identity_4x4;
         Frame_B : math.Matrix_4x4 := math.Identity_4x4;
      begin
         set_Translation (Frame_B, (  2.0, 2.0, 0.0));

         the_hinge_Joint.define (the_Applet.gui_World.Physics,
                                 the_hinge_Box_1,   the_hinge_Box_2,
                                 Frame_A,           Frame_B,
                                 collide_Conected => False);

         the_hinge_Box_1.Site_is (( 0.0,  0.0,  0.0));
         the_hinge_Box_2.Site_is ((-1.0,  2.0,  0.0));

         the_Applet.gui_World.add (the_hinge_Box_1,  and_children => False);
         the_Applet.gui_World.add (the_hinge_Box_2,  and_children => False);
         the_Applet.gui_World.add (the_hinge_Joint.all'Access);
      end;

      --  DoF6
      --
      declare
         use mmi.any_Joint,
             math.Vectors;

         the_dof6_Box_1  : constant mmi.Sprite.view := new_box_Sprite (the_Applet.gui_World, mass => 0.0);
         the_dof6_Box_2  : constant mmi.Sprite.view := new_box_Sprite (the_Applet.gui_World);
         the_DoF6_Joint  :          mmi.any_Joint   .view := new mmi.any_Joint.item;

         Frame_A : math.Matrix_4x4 := math.Identity_4x4;
         Frame_B : math.Matrix_4x4 := math.Identity_4x4;
      begin
         set_Translation (Frame_B, (  2.0, 2.0, 0.0));

         the_dof6_Joint.define (the_Applet.gui_World.Physics,
                                the_dof6_Box_1,  the_dof6_Box_2,
                                Frame_A,         Frame_B);

         the_dof6_Box_1.Site_is ((-20.0,        0.0,  0.0));
         the_dof6_Box_2.Site_is ((-20.0 - 2.0,  0.0,  2.0));

         the_Applet.gui_World.add (the_dof6_Box_1);
         the_Applet.gui_World.add (the_dof6_Box_2);
         the_Applet.gui_World.add (the_dof6_Joint.all'Access);
      end;

      --  Ball
      --
      declare
         use mmi.any_Joint,
             math.Vectors;

         the_ball_Box_1  : constant mmi.Sprite.view := new_box_Sprite (the_Applet.gui_World, mass => 0.0);
         the_ball_Box_2  : constant mmi.Sprite.view := new_box_Sprite (the_Applet.gui_World);
         the_ball_Joint  :          mmi.ball_Joint  .view := new mmi.ball_Joint.item;

         Pivot_in_A : math.Vector_3 := (0.0, -1.0, 0.0);
         Pivot_in_B : math.Vector_3 := (0.0,  1.0, 0.0);
      begin
         the_ball_Joint.define (the_Applet.gui_World.Physics,
                                the_ball_Box_1,  the_ball_Box_2,
                                Pivot_in_A,      Pivot_in_B);

         the_ball_Box_1.Site_is ((-10.0,        0.0,  0.0));
         the_ball_Box_2.Site_is ((-10.0 - 2.0,  0.0,  2.0));

         the_Applet.gui_World.add (the_ball_Box_1);
         the_Applet.gui_World.add (the_ball_Box_2);
         the_Applet.gui_World.add (the_ball_Joint.all'Access);
      end;

      --  Slider
      --
      declare
         use mmi.any_Joint,
             math.Vectors;

         the_slider_Box_1  : constant mmi.Sprite.view := new_box_Sprite (the_Applet.gui_World, mass => 0.0);
         the_slider_Box_2  : constant mmi.Sprite.view := new_box_Sprite (the_Applet.gui_World);
         the_slider_Joint  :          mmi.slider_Joint.view := new mmi.slider_Joint.item;

         Frame_A : math.Matrix_4x4 := math.Identity_4x4;
         Frame_B : math.Matrix_4x4 := math.Identity_4x4;

         y_Rot   : math.Matrix_3x3 := y_Rotation_from (to_Radians (45.0));
         x_Rot   : math.Matrix_3x3 := x_Rotation_from (to_Radians (45.0));
      begin
         set_Translation (Frame_A, (  -4.0, 4.0, -4.0));
         set_Translation (Frame_B, (  4.0, 0.0, 0.0));
--           set_Rotation    (Frame_A, x_Rot);
--           set_Rotation    (Frame_B, x_Rot);

         the_slider_Joint.define (the_Applet.gui_World.Physics,
                                  the_slider_Box_1,  the_slider_Box_2,
                                  Frame_A,           Frame_B);

         the_slider_Box_1.Site_is ((-10.0,        10.0,  0.0));
         the_slider_Box_2.Site_is ((-10.0 - 2.0,  15.0,  0.0));

         the_Applet.gui_World.add (the_slider_Box_1);
         the_Applet.gui_World.add (the_slider_Box_2);
         the_Applet.gui_World.add (the_slider_Joint.all'Access);
      end;

      --  cone Twist
      --
      declare
         use mmi.any_Joint,
             math.Vectors;

         the_cone_twist_Box_1  : constant mmi.Sprite.view     := new_box_Sprite (the_Applet.gui_World, mass => 0.0);
         the_cone_twist_Box_2  : constant mmi.Sprite.view     := new_box_Sprite (the_Applet.gui_World);
         the_cone_twist_Joint  :          mmi.cone_twist_Joint.view := new mmi.cone_twist_Joint.item;

         Frame_A : math.Matrix_4x4 := math.Identity_4x4;
         Frame_B : math.Matrix_4x4 := math.Identity_4x4;

         y_Rot   : math.Matrix_3x3 := y_Rotation_from (to_Radians (45.0));
         x_Rot   : math.Matrix_3x3 := x_Rotation_from (to_Radians (45.0));
      begin
--           set_Translation (Frame_A, (  -4.0, 4.0, -4.0));
         set_Translation (Frame_B, (  4.0, 0.0, 0.0));
--           set_Rotation    (Frame_A, x_Rot);
--           set_Rotation    (Frame_B, x_Rot);

         the_cone_twist_Joint.define (the_Applet.gui_World.Physics,
                                      the_cone_twist_Box_1,  the_cone_twist_Box_2,
                                      Frame_A,           Frame_B);

         the_cone_twist_Box_1.Site_is ((10.0,        10.0,  0.0));
         the_cone_twist_Box_2.Site_is ((10.0 + 2.0,  10.0,  0.0));

         the_Applet.gui_World.add (the_cone_twist_Box_1);
         the_Applet.gui_World.add (the_cone_twist_Box_2);
         the_Applet.gui_World.add (the_cone_twist_Joint.all'Access);
      end;
   end;


   while the_Applet.is_open
   loop
      the_Applet.freshen;     -- Handle any new events and update the screen.
   end loop;

   the_Applet.destroy;
end launch_mixed_Joints;
