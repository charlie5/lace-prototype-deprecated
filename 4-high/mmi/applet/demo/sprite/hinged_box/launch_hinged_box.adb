with
     mmi.Window.lumen,
     mmi.Applet.gui_world,

     mmi.Forge,
     mmi.Sprite,

     mmi.hinge_Joint,
     mmi.any_Joint,

     openGL.Model.box.colored,
     openGL.Model.sphere.lit_colored_textured,
     openGL.Model.capsule.lit_colored_textured,
     openGL.Model.open_gl,
     openGL.Model.terrain,
     mmi.Terrain,

     mmi.physics_Model,

     opengl.Palette,
     opengl.IO,
     Physics;

pragma Unreferenced (mmi.Window.lumen);

with float_math.Algebra.linear.d3;
with openGL.IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;


procedure launch_hinged_Box
--
--  Drops a variety of shapes a plane terrain.
--
--
is
   package Math   renames float_Math;

   use mmi.Applet,  openGL.Model.box,
       openGL,      opengl.Palette;

   use type openGL.Real;

   the_Applet : constant mmi.Applet.gui_World.view := mmi.Forge.new_gui_Applet ("hinged Box", 1920, 1200);
   X : float_math.Real := 0.0;


begin
   the_Applet.gui_Camera.Site_is ((0.0, 4.0, 30.0));      -- Position the camera.
   the_Applet.enable_simple_Dolly (1);                    -- Enable user camera control via keyboard.
   the_Applet.Renderer.Background_is (Blue);



   --  Add shapes
   --
   declare
      use float_Math,
          float_math.Algebra.linear.d3;

      bs : constant := 1.0;
      --  Box
      --
      the_box_Model : constant openGL.Model.box.colored.view
        := openGL.Model.box.colored.forge.new_Box (scale => (bs, bs, bs),
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

      the_Joint   : mmi.hinge_Joint.view := new mmi.hinge_Joint.item;
      the_Joint_2 : mmi.hinge_Joint.view := new mmi.hinge_Joint.item;

   begin
      the_Applet.gui_World.Gravity_is ((0.0, -10.0, 0.0));

      the_Applet.gui_World.add (the_Box);                    -- Add box.
      the_Applet.gui_World.add (the_Box_1);                    -- Add box.
      the_Applet.gui_World.add (the_Box_2);                    -- Add box.

      the_Box  .Site_is (( 10.0, 10.0, 0.0));
      the_Box_1.Site_is (( 0.0, 0.0, 0.0));
      the_Box_2.Site_is ((-1.0, 2.0, 0.0));

      the_Applet.freshen;

      --     the_Cone    .Spin_is (y_Rotation_from (to_Radians (90.0)));
      --     the_Cylinder.Spin_is (x_Rotation_from (to_Radians (45.0)));
      --     the_Cylinder.Gyre_is ((0.0, 1.0, 0.0));

      declare
         use mmi.any_Joint, math.Algebra.linear.d3, math.Vectors;

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

      the_Applet.gui_World.add (the_Joint  .all'Access);        -- add joint to the world
      the_Applet.gui_World.add (the_Joint_2.all'Access);        -- add joint to the world
   end;


   while the_Applet.is_open
   loop
      the_Applet.freshen;                                 -- Handle any new events and update the screen.
   end loop;

   the_Applet.destroy;


exception
   when E : others =>
      new_Line;
      put_Line ("Unhandled exception in main thread !");
      put_Line (Ada.Exceptions.Exception_Information (E));
      new_Line;
end launch_hinged_Box;





--
--
--
--  with mmi.Window.sdl,
--       mmi.Applet,
--       mmi.Camera;
--
--  with mmi.Sprite.forge,
--       mmi.Model,
--       mmi.box_Model,
--       mmi.hinge_Joint,
--       mmi.any_Joint;
--
--  with opengl.Palette,
--       math.Algebra.linear.d3,
--       ada.Calendar;
--
--
--
--  procedure launch_hinged_Box
--  --
--  -- drops two boxes joined by a hinge onto a larger 'terrain' box.
--  --
--  is
--     use mmi.Applet,    mmi.box_Model,
--         openGL,        opengl.Palette,
--         Math, math.Algebra.linear.d3,
--         ada.Calendar;
--
--     use type math.Real;
--
--     the_Window : mmi.window.view := mmi.Window.sdl.forge.new_Window ("drop Ball on Box", 500, 500);
--     the_Applet : mmi.Applet.view := mmi.applet.forge.new_Applet (use_window => the_Window);
--
--
--     the_terrain_Model : aliased mmi.box_Model.item
--       := (mmi.Model.item with
--           scale => (10.0, 0.5, 10.0),
--           faces => (front => (colors => (others => (Red,     Opaque))),
--                     rear  => (colors => (others => (Blue,    Opaque))),
--                     upper => (colors => (others => (Green,   Opaque))),
--                     lower => (colors => (others => (Yellow,  Opaque))),
--                     left  => (colors => (others => (Cyan,    Opaque))),
--                     right => (colors => (others => (Magenta, Opaque)))));
--
--     the_Terrain : mmi.Sprite.view
--       := mmi.Sprite.forge.new_Sprite (the_terrain_Model'access,  mass => 0.0);
--
--
--     the_box_Model : aliased mmi.box_Model.item
--       := (mmi.Model.item with
--           scale => (1.0, 1.5, 0.2),
--           faces => (front => (colors => (others => (Green,     Opaque))),
--                     rear  => (colors => (others => (Blue,    Opaque))),
--                     upper => (colors => (others => (Mauve,   Opaque))),
--                     lower => (colors => (others => (Yellow,  Opaque))),
--                     left  => (colors => (others => (Cyan,    Opaque))),
--                     right => (colors => (others => (Magenta, Opaque)))));
--
--     Box_A : mmi.Sprite.view
--       := mmi.Sprite.forge.new_Sprite (the_box_Model'access,  mass => 0.0);
--
--     Box_B : mmi.Sprite.view
--       := mmi.Sprite.forge.new_Sprite (the_box_Model'access,  mass => 1.0);
--
--     the_Joint : mmi.hinge_Joint.view := new mmi.hinge_Joint.item;
--     ball_Joint : mmi.any_Joint.view := new mmi.any_Joint.item;
--
--
--     next_render_Time : ada.calendar.Time;
--
--  begin
--     the_Applet.add (the_Terrain);                       -- add terrain
--     the_Terrain.Site_is ((4.0,  -10.0,  0.0));            --
--
--
--
--     the_Applet.add (Box_A);                             -- add boxes
--     Box_A.Site_is ((-2.0,  0.0,  0.0));                --
--  --     Box_A.Gyre_is ((0.0, 50.0, 0.0));                    --
--
--
--     the_Applet.add (Box_B);                             --
--     Box_B.Site_is (( 2.0,  0.0,  0.0));                 --
--     Box_B.Spin_is (y_Rotation_from (to_Radians (180.0)));                 --
--     Box_B.Gyre_is (( 0.0, -50.0, 0.0));                    --
--
--
--  --     the_Joint.define (Box_A, Box_B, pivot_axis => (0.0, 1.0, 0.0));                    -- create the joint
--  --     the_Joint.Limits_are (-90.0 * math.Pi/180.0,
--  --                            40.0 * math.Pi/180.0);
--
--     declare
--        use mmi.any_Joint, math.Algebra.linear.d3, math.real_Arrays;
--
--        Frame_A : math.Matrix_4x4 := math.Identity_4x4;
--        Frame_B : math.Matrix_4x4 := math.Identity_4x4;
--
--        y_Rot   : math.Matrix_3x3 := y_Rotation_from (to_Radians (180.0));
--     begin
--        set_Translation (Frame_A, ( 2.0, 0.0, 0.0));
--        set_Translation (Frame_B, y_Rot * math.Vector_3'( -2.0, 0.0, 0.0));
--
--        set_Rotation    (Frame_A, x_Rotation_from (to_Radians (0.0)));
--        set_Rotation    (Frame_B, y_Rot);
--
--        ball_Joint.define (Box_A, Box_B, Frame_A, Frame_B);
--
--        ball_Joint.low_Bound_is (Pitch, 0.0);
--        ball_Joint.low_Bound_is (Yaw,   0.0);
--        ball_Joint.low_Bound_is (Roll,  0.0);
--
--        ball_Joint.high_Bound_is (Pitch, 0.0);
--        ball_Joint.high_Bound_is (Yaw,   0.0);
--        ball_Joint.high_Bound_is (Roll,  0.0);
--
--     end;
--
--
--
--  --     the_Applet.add (Box_A);                             -- add boxes
--  --     the_Applet.add (Box_B);                             --
--
--  --     the_Applet.World.add (the_Joint.all'access);        -- add joint to the world
--     the_Applet.World.add (ball_Joint.all'access);        -- add joint to the world
--
--
--                                                         --
--
--
--     the_Applet.Camera.Site_is ((0.0, -2.0, 15.0));       -- position the camera
--     the_Applet.enable_Dolly;                            -- enable user camera control via keyboards
--     the_Applet.enable_Mouse;                            -- enable mouse events
--
--
--     next_render_Time := ada.Calendar.clock;
--
--     while the_Applet.is_open loop
--        the_Applet.World.evolve (by => 1.0/60.0);        -- evolve the world
--        the_Applet.freshen;                              -- handle any new events and update the screen
--
--        next_render_Time := next_render_Time + 1.0/60.0;
--        delay until next_render_Time;
--     end loop;
--
--
--     the_Applet.destroy;
--  end launch_hinged_Boxes;
