with
     mmi.Window.lumen,
     mmi.Applet.gui_world,
     mmi.Forge,
     mmi.Sprite,
     mmi.hinge_Joint,
     mmi.physics_Model,

     openGL.Model.box.colored,
     opengl.Palette,

     float_math.Algebra.linear.d3,

     Ada.Text_IO,
     Ada.Exceptions;

pragma Unreferenced (mmi.Window.lumen);


procedure launch_hinged_Box
--
--  Shows variously hinged boxes.
--
is
   package Math renames float_Math;

   use mmi.Applet,  openGL.Model.box,
       openGL,      opengl.Palette,
       Ada.Text_IO;

   use type openGL.Real;

   the_Applet : constant mmi.Applet.gui_World.view := mmi.Forge.new_gui_Applet ("hinged Box", 1920, 1200);
   X          :          float_math.Real           := 0.0;

begin
   the_Applet.gui_Camera.Site_is ((0.0, 4.0, 30.0));      -- Position the camera.
   the_Applet.enable_simple_Dolly (1);                    -- Enable user camera control via keyboard.
   the_Applet.Renderer.Background_is (Blue);

   --  Add sprites and joints.
   --
   declare
      use float_Math;

      bs : constant := 1.0;

      --  Box
      --
      the_box_Model : constant openGL.Model.box.colored.view
        := openGL.Model.box.colored.forge.new_Box (size  => (bs, bs, bs),
                                                   faces => (front => (colors => (others => (Red,     Opaque))),
                                                             rear  => (colors => (others => (Blue,    Opaque))),
                                                             upper => (colors => (others => (Violet,  Opaque))),
                                                             lower => (colors => (others => (Yellow,  Opaque))),
                                                             left  => (colors => (others => (Cyan,    Opaque))),
                                                             right => (colors => (others => (Magenta, Opaque)))));
      the_static_box_physics_Model : constant mmi.physics_Model.view
        := mmi.physics_Model.Forge.new_physics_Model (shape_Info => (kind         => mmi.physics_Model.Cube,
                                                                     half_extents => the_box_Model.Scale));
      the_dynamic_box_physics_Model : constant mmi.physics_Model.view
        := mmi.physics_Model.Forge.new_physics_Model (shape_Info => (kind         => mmi.physics_Model.Cube,
                                                                     half_extents => the_box_Model.Scale),
                                                      mass       => 1.0);
      the_Box : constant mmi.Sprite.view
        := mmi.Sprite.forge.new_Sprite ("demo.Box",
                                        the_Applet.gui_World,
                                        the_box_Model.all'Access,
                                        the_dynamic_box_physics_Model,
                                        owns_Graphics => True,
                                        owns_Physics  => True);

      the_Box_1 : constant mmi.Sprite.view
        := mmi.Sprite.forge.new_Sprite ("demo.Box1",
                                        the_Applet.gui_World,
                                        the_box_Model.all'Access,
                                        the_static_box_physics_Model,
                                        owns_Graphics => False,
                                        owns_Physics  => True);

      the_Box_2 : constant mmi.Sprite.view
        := mmi.Sprite.forge.new_Sprite ("demo.Box2",
                                        the_Applet.gui_World,
                                        the_box_Model.all'Access,
                                        the_dynamic_box_physics_Model,
                                        owns_Graphics => False,
                                        owns_Physics  => False);

      the_Joint   : constant mmi.hinge_Joint.view := new mmi.hinge_Joint.item;
      the_Joint_2 : constant mmi.hinge_Joint.view := new mmi.hinge_Joint.item;

   begin
      the_Applet.gui_World.Gravity_is ((0.0, -10.0, 0.0));

      the_Applet.gui_World.add (the_Box);                    -- Add box.
      the_Applet.gui_World.add (the_Box_1);                    -- Add box.
      the_Applet.gui_World.add (the_Box_2);                    -- Add box.

      the_Box  .Site_is (( 10.0, 10.0, 0.0));
      the_Box_1.Site_is (( 0.0, 0.0, 0.0));
      the_Box_2.Site_is ((-1.0, 2.0, 0.0));

      the_Applet.freshen;

      declare
         use math.Algebra.linear.d3,
             math.Vectors;

         Frame   : math.Matrix_4x4 := math.Identity_4x4;
         Frame_A : math.Matrix_4x4 := math.Identity_4x4;
         Frame_B : math.Matrix_4x4 := math.Identity_4x4;

         y_Rot   : math.Matrix_3x3 := y_Rotation_from (to_Radians (180.0));
      begin
         set_Translation (Frame,   (  8.0, 8.0, 0.0));
         set_Translation (Frame_B, (  2.0, 2.0, 0.0));
--           set_Translation (Frame_B, y_Rot * math.Vector_3'( -2.0, 0.0, 0.0));
--
--           set_Rotation    (Frame_A, x_Rotation_from (to_Radians (0.0)));
--           set_Rotation    (Frame_B, y_Rot);

         the_Joint.define (the_Applet.gui_World.Physics,
                           the_Box,
                           Frame);

         the_Joint_2.define (the_Applet.gui_World.Physics,
                             the_Box_1,   the_Box_2,
                             Frame_A,     Frame_B,
                             low_Limit        => to_Radians (-360.0),
                             high_Limit       => to_Radians ( 360.0),
                             collide_Conected => False);

--           the_Joint.low_Bound_is (Pitch, 0.0);
--           the_Joint.low_Bound_is (Yaw,   0.0);
--           the_Joint.low_Bound_is (Roll,  0.0);
--
--           the_Joint.high_Bound_is (Pitch, 0.0);
--           the_Joint.high_Bound_is (Yaw,   0.0);
--           the_Joint.high_Bound_is (Roll,  0.0);
      end;

      the_Applet.gui_World.add (the_Joint  .all'Access);        -- Add joint to the world.
      the_Applet.gui_World.add (the_Joint_2.all'Access);        -- Add joint to the world.
   end;


   while the_Applet.is_open
   loop
      the_Applet.freshen;                                       -- Handle any new events and update the screen.
   end loop;

   the_Applet.destroy;


exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main thread !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_hinged_Box;
