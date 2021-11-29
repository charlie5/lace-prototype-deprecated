with
     gel.Window.sdl,
     gel.Applet.gui_world,
     gel.Forge,
     gel.Sprite,
     gel.hinge_Joint,
     physics.Model,

     openGL.Model.box.colored,
     openGL.Palette,

     float_math.Algebra.linear.d3,

     ada.Text_IO,
     ada.Exceptions;

pragma unreferenced (gel.Window.sdl);


procedure launch_hinged_Box
--
--  Shows variously hinged boxes.
--
is
   package Math renames float_Math;

   use openGL,
       openGL.Model.box,
       opengl.Palette,
       ada.Text_IO;

   the_Applet : constant gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("hinged Box", 1536, 864);
   X          :          float_math.Real           := 0.0;

begin
   the_Applet.gui_Camera.Site_is ((0.0, 4.0, 30.0));      -- Position the camera.
   the_Applet.enable_simple_Dolly (1);                    -- Enable user camera control via keyboard.
   the_Applet.Renderer.Background_is (Blue);

   --  Add sprites and joints.
   --
   declare
      use float_Math;

      box_Size : constant gel.Math.Vector_3 := (1.0, 1.0, 1.0);

      --  Box
      --
      the_box_Model : constant openGL.Model.box.colored.view
        := openGL.Model.box.colored.new_Box (Size  => box_Size,
                                             Faces => (Front => (colors => (others => (Red,     Opaque))),
                                                       Rear  => (colors => (others => (Blue,    Opaque))),
                                                       Upper => (colors => (others => (Violet,  Opaque))),
                                                       Lower => (colors => (others => (Yellow,  Opaque))),
                                                       Left  => (colors => (others => (Cyan,    Opaque))),
                                                       Right => (colors => (others => (Magenta, Opaque)))));
      the_static_box_physics_Model : constant physics.Model.view
        := physics.Model.forge.new_physics_Model (shape_Info => (Kind         => physics.Model.cube,
                                                                 half_Extents => box_Size));
      the_dynamic_box_physics_Model : constant physics.Model.view
        := physics.Model.forge.new_physics_Model (shape_Info => (Kind         => physics.Model.cube,
                                                                 half_Extents => box_Size / 2.0),
                                                  Mass       => 1.0);
      the_Box_1 : constant gel.Sprite.view
        := gel.Sprite.forge.new_Sprite ("demo.Box.static.1",
                                        the_Applet.gui_World.all'Access,
                                        math.Origin_3d,
                                        the_box_Model.all'Access,
                                        the_static_box_physics_Model,
                                        owns_Graphics => False,
                                        owns_Physics  => True);

      the_Box_2 : constant gel.Sprite.view
        := gel.Sprite.forge.new_Sprite ("demo.Box.dynamic.2",
                                        the_Applet.gui_World.all'Access,
                                        math.Origin_3d,
                                        the_box_Model.all'Access,
                                        the_dynamic_box_physics_Model,
                                        owns_Graphics => False,
                                        owns_Physics  => False);

      the_Box_3 : constant gel.Sprite.view
        := gel.Sprite.forge.new_Sprite ("demo.Box.dynamic.3",
                                        the_Applet.gui_World.all'Access,
                                        math.Origin_3d,
                                        the_box_Model.all'Access,
                                        the_dynamic_box_physics_Model,
                                        owns_Graphics => True,
                                        owns_Physics  => True);

      the_Joint_1 : constant gel.hinge_Joint.view := new gel.hinge_Joint.item;
      the_Joint_2 : constant gel.hinge_Joint.view := new gel.hinge_Joint.item;

   begin
      the_Applet.gui_World.Gravity_is ((0.0, -10.0, 0.0));

      the_Applet.gui_World.add (the_Box_1);
      the_Applet.gui_World.add (the_Box_2);
      the_Applet.gui_World.add (the_Box_3);

      the_Box_1.Site_is ((  0.0,  0.0, 0.0));
      the_Box_2.Site_is (( -1.0,  2.0, 0.0));
      the_Box_3.Site_is (( 10.0, 10.0, 0.0));
      --  the_Box_3.Site_is (( 10.0, 10.0, 0.0));

      declare
         use math.Algebra.linear.d3;

         Frame_1 : constant math.Matrix_4x4 := math.Identity_4x4;
         Frame_2 : math.Matrix_4x4 := math.Identity_4x4;
         Frame_3 : math.Matrix_4x4 := math.Identity_4x4;

         y_Rot   : math.Matrix_3x3 := y_Rotation_from (to_Radians (180.0));
      begin
         set_Translation (Frame_2, (2.0, 2.0, 0.0));
         set_Translation (Frame_3, (8.0, 8.0, 0.0));
         --  set_Translation (Frame_3, (8.0, 8.0, 0.0));
--           set_Translation (Frame_B, y_Rot * math.Vector_3'( -2.0, 0.0, 0.0));
--
--           set_Rotation    (Frame_A, x_Rotation_from (to_Radians (0.0)));
--           set_Rotation    (Frame_B, y_Rot);

         the_Joint_1.define (the_Applet.gui_World.Space,
                             the_Box_3,
                             Frame_3);

         the_Joint_2.define (the_Applet.gui_World.Space,
                             the_Box_1, the_Box_2,
                             Frame_1,   Frame_2,
                             low_Limit        => to_Radians (-360.0),
                             high_Limit       => to_Radians ( 360.0),
                             collide_Conected => False);

         -- TODO:
         --  the_Joint_2.define (the_Applet.gui_World.Space,
         --                      the_Box_1, the_Box_2,
         --                      pivot_Axis => (0.0, 0.0, 0.0));

--           the_Joint.low_Bound_is (Pitch, 0.0);
--           the_Joint.low_Bound_is (Yaw,   0.0);
--           the_Joint.low_Bound_is (Roll,  0.0);
--
--           the_Joint.high_Bound_is (Pitch, 0.0);
--           the_Joint.high_Bound_is (Yaw,   0.0);
--           the_Joint.high_Bound_is (Roll,  0.0);
      end;

      the_Applet.gui_World.add (the_Joint_1.all'Access);        -- Add joint to the world.
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
      put_Line (ada.Exceptions.exception_Information (E));
      new_Line;
end launch_hinged_Box;
