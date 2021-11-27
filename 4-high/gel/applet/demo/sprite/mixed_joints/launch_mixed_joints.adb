with
     gel.Window.sdl,
     gel.Applet.gui_world,
     gel.Forge,
     gel.Sprite,
     gel.hinge_Joint,
     gel.ball_Joint,
     gel.cone_twist_Joint,
     gel.slider_Joint,
     gel.any_Joint,

     openGL.Palette;

pragma unreferenced (gel.Window.sdl);


procedure launch_mixed_Joints
--
--  Shows a variety of joints.
--
is
   package Math renames gel.Math;

   use openGL,
       opengl.Palette;

   the_Applet : constant gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("mixed Joints", 1536, 864);
begin
   the_Applet.gui_World .Gravity_is    ((0.0, -10.0, 0.0));
   the_Applet.gui_Camera.Site_is       ((0.0, 4.0, 30.0));                      -- Position the camera.
   the_Applet.Renderer  .Background_is (Grey);
   the_Applet.enable_simple_Dolly      (gel.Applet.gui_world.gui_world_Id);     -- Enable user camera control via keyboard.

   --  Add joints.
   --
   declare
      use gel.Forge,
          gel.linear_Algebra_3D,
          Math;
   begin
      --  Hinge
      --
      declare
         the_hinge_Box_1 : constant gel.Sprite.view := new_box_Sprite (the_Applet.gui_World, Mass => 0.0);
         the_hinge_Box_2 : constant gel.Sprite.view := new_box_Sprite (the_Applet.gui_World);
         the_hinge_Joint : constant gel.hinge_Joint .view := new gel.hinge_Joint.item;

         Frame_A : constant math.Matrix_4x4 := math.Identity_4x4;
         Frame_B :          math.Matrix_4x4 := math.Identity_4x4;
      begin
         set_Translation (Frame_B, (  2.0, 2.0, 0.0));

         the_hinge_Joint.define (the_Applet.gui_World.Space,
                                 the_hinge_Box_1,   the_hinge_Box_2,
                                 Frame_A,           Frame_B,
                                 collide_Conected => False);

         the_hinge_Box_1.Site_is (( 0.0,  0.0,  0.0));
         the_hinge_Box_2.Site_is ((-1.0,  2.0,  0.0));

         the_Applet.gui_World.add (the_hinge_Box_1,  and_Children => False);
         the_Applet.gui_World.add (the_hinge_Box_2,  and_Children => False);
         the_Applet.gui_World.add (the_hinge_Joint.all'Access);
      end;

      --  DoF6
      --
      declare
         the_DoF6_Box_1  : constant gel.Sprite   .view := new_box_Sprite (the_Applet.gui_World, Mass => 0.0);
         the_DoF6_Box_2  : constant gel.Sprite   .view := new_box_Sprite (the_Applet.gui_World);
         the_DoF6_Joint  : constant gel.any_Joint.view := new gel.any_Joint.item;

         Frame_A : constant math.Matrix_4x4 := math.Identity_4x4;
         Frame_B :          math.Matrix_4x4 := math.Identity_4x4;
      begin
         set_Translation (Frame_B, (2.0, 2.0, 0.0));

         the_DoF6_Joint.define (the_Applet.gui_World.Space,
                                the_DoF6_Box_1,  the_DoF6_Box_2,
                                Frame_A,         Frame_B);

         the_DoF6_Box_1.Site_is ((-20.0,        0.0,  0.0));
         the_DoF6_Box_2.Site_is ((-20.0 - 2.0,  0.0,  2.0));

         the_Applet.gui_World.add (the_DoF6_Box_1);
         the_Applet.gui_World.add (the_DoF6_Box_2);
         the_Applet.gui_World.add (the_DoF6_Joint.all'Access);
      end;

      --  Ball
      --
      declare
         the_ball_Box_1  : constant gel.Sprite    .view := new_box_Sprite (the_Applet.gui_World, Mass => 0.0);
         the_ball_Box_2  : constant gel.Sprite    .view := new_box_Sprite (the_Applet.gui_World);
         the_ball_Joint  : constant gel.ball_Joint.view := new gel.ball_Joint.item;

         Pivot_in_A : constant math.Vector_3 := (0.0, -1.0, 0.0);
         Pivot_in_B : constant math.Vector_3 := (0.0,  1.0, 0.0);
      begin
         the_ball_Joint.define (the_Applet.gui_World.Space,
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
         the_slider_Box_1  : constant gel.Sprite      .view := new_box_Sprite (the_Applet.gui_World, Mass => 0.0);
         the_slider_Box_2  : constant gel.Sprite      .view := new_box_Sprite (the_Applet.gui_World);
         the_slider_Joint  : constant gel.slider_Joint.view := new gel.slider_Joint.item;

         Frame_A : math.Matrix_4x4 := math.Identity_4x4;
         Frame_B : math.Matrix_4x4 := math.Identity_4x4;

         y_Rot   : math.Matrix_3x3 := y_Rotation_from (to_Radians (45.0));
         x_Rot   : math.Matrix_3x3 := x_Rotation_from (to_Radians (45.0));
      begin
         set_Translation (Frame_A, (-4.0, 4.0, -4.0));
         set_Translation (Frame_B, ( 4.0, 0.0,  0.0));
--           set_Rotation    (Frame_A, x_Rot);
--           set_Rotation    (Frame_B, x_Rot);

         the_slider_Joint.define (the_Applet.gui_World.Space,
                                  the_slider_Box_1,  the_slider_Box_2,
                                  Frame_A,           Frame_B);

         the_slider_Box_1.Site_is ((-10.0,       10.0, 0.0));
         the_slider_Box_2.Site_is ((-10.0 - 2.0, 15.0, 0.0));

         the_Applet.gui_World.add (the_slider_Box_1);
         the_Applet.gui_World.add (the_slider_Box_2);
         the_Applet.gui_World.add (the_slider_Joint.all'Access);
      end;

      --  cone Twist
      --
      declare
         the_cone_twist_Box_1  : constant gel.Sprite          .view := new_box_Sprite (the_Applet.gui_World, Mass => 0.0);
         the_cone_twist_Box_2  : constant gel.Sprite          .view := new_box_Sprite (the_Applet.gui_World);
         the_cone_twist_Joint  : constant gel.cone_twist_Joint.view := new gel.cone_twist_Joint.item;

         Frame_A : constant math.Matrix_4x4 := math.Identity_4x4;
         Frame_B :          math.Matrix_4x4 := math.Identity_4x4;

         y_Rot   : math.Matrix_3x3 := y_Rotation_from (to_Radians (45.0));
         x_Rot   : math.Matrix_3x3 := x_Rotation_from (to_Radians (45.0));
      begin
--           set_Translation (Frame_A, (  -4.0, 4.0, -4.0));
         set_Translation (Frame_B, (4.0, 0.0, 0.0));
--           set_Rotation    (Frame_A, x_Rot);
--           set_Rotation    (Frame_B, x_Rot);

         the_cone_twist_Joint.define (the_Applet.gui_World.Space,
                                      the_cone_twist_Box_1,  the_cone_twist_Box_2,
                                      Frame_A,               Frame_B);

         the_cone_twist_Box_1.Site_is ((10.0,       10.0, 0.0));
         the_cone_twist_Box_2.Site_is ((10.0 + 2.0, 10.0, 0.0));

         the_Applet.gui_World.add (the_cone_twist_Box_1);
         the_Applet.gui_World.add (the_cone_twist_Box_2);
         the_Applet.gui_World.add (the_cone_twist_Joint.all'Access);
      end;
   end;


   while the_Applet.is_open
   loop
      the_Applet.freshen;     -- Handle any new events, evolve the world and update the screen.
   end loop;

   the_Applet.destroy;
end launch_mixed_Joints;
