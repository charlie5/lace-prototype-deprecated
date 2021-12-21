with
     gel.Window.setup,
     gel.Applet.gui_world,
     gel.Camera,
     gel.Sprite,
     gel.Human_v1,
     gel.Forge,
     gel.human_Types,
     gel.human_Types_v1,
     physics.Model,

     openGL.Model.box.lit_colored_textured,
     openGL.Model.any,
     openGL.Model.sphere.lit_colored_textured,
     opengl.Palette,
     opengl.IO,

     float_Math.algebra.linear.d3,

     ada.Calendar,
     ada.Strings.fixed,
     ada.Text_IO,
     ada.Exceptions;


procedure launch_human_Model_v1
--
-- Drops an gel human model onto a simple box terrain.
--
--
is
   use gel.Applet,    openGL.Model.box,  gel.Human_v1, gel.human_Types,
       openGL,        opengl.Palette,
       float_Math,    float_Math.algebra.linear.d3,
       ada.Calendar,  ada.Strings,    ada.Strings.fixed, ada.Text_IO, ada.Exceptions;

   use type math.Real,  opengl.Real;

   the_Applet : constant gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("human Model", 1920, 1200);
   the_Ground : constant gel.Sprite.view           := gel.Forge.new_box_Sprite (the_Applet.gui_World,
                                                                                mass => 0.0,
                                                                                size => (50.0, 1.0, 50.0));

--     the_human_graphics_Model : aliased gel.graphics_Model.open_gl.view
--       := gel.graphics_Model.open_gl.forge.new_Model (scale   => (1.0, 1.0, 1.0),
--  --                                                     model   => gel.to_Asset ("assets/gel/model/gel-human.dae"),
--                                                     model   => gel.to_Asset ("assets/gel/collada/mh-human-dae.dae"),
--  --                                                     model   => gel.to_Asset ("assets/gel/collada/alfieri.dae"),
--                                                     texture => gel.null_Asset, -- gel.to_Asset ("assets/collada/gel-human-texture.tga"),
--                                                     Texture_is_lucid => False);
--     the_human_physics_Model : constant gel.physics_Model.view
--       := gel.physics_Model.Forge.new_physics_Model (shape_Info => (kind         => gel.physics_Model.Cube,
--                                                                    half_extents => 0.5 * (4.0, 1.0, 2.0)),
--                                                     mass       => 1.0);
--  --     the_human_physics_Model : constant gel.physics_Model.view
--  --       := gel.physics_Model.Forge.new_physics_Model (shape_Info => (kind          => gel.physics_Model.a_Sphere,
--  --                                                                    sphere_radius => 0.2),
--  --                                                     mass       => 0.5);
   my_Human         : aliased gel.Human_v1.item;
   use gel.Human_v1;

   next_render_Time :         ada.calendar.Time;

begin
   gel.Human_v1.Mode_is (Skin);
   gel.Human_v1.Mode_is (Bones);
   --  gel.Human_v1.Mode_is (Skin_and_Bones);

   the_Applet.gui_World.Gravity_is ((0.0, -0.0, 0.0));
   --  the_Applet.gui_World.Gravity_is ((0.0, -9.8, 0.0));
   --  the_Applet.gui_World.Gravity_is ((0.0, -0.5, 0.0));

   --  --  the_Applet.gui_Camera.Site_is ((0.0, 1.0, 5.0));    -- Position the camera
   --  the_Applet.gui_Camera.Site_is ((0.0, -9.0, 5.0));    -- Position the camera
   the_Applet.gui_Camera.Site_is ((0.0, -0.0, 5.0));    -- Position the camera
   the_Applet.enable_simple_Dolly (1);                 -- Enable user camera control via keyboards
   the_Applet.Dolly.Speed_is (0.1);
   the_Applet.enable_Mouse (detect_Motion => False);                            -- Enable mouse events.

--     gel.Human_v1.use_Model ("assets/mh-blender-no_bones.dae");
   --  gel.Human_v1.use_Model ("assets/mh-blender-2.dae");
   --  gel.Human_v1.use_Model ("assets/mh-blender-2-y_up.dae");
--     gel.Human_v1.use_Model ("assets/human-default.dae");
   gel.Human_v1.use_Model ("assets/human-default-animated-01_01.dae");

--     gel.Human_v1.use_Model ("assets/human-new.dae");
--     gel.Human_v1.use_Model ("assets/human-default-1.dae");
   --     gel.Human.use_Model ("assets/gel/collada/alfieri.dae");

   my_Human.define (the_Applet.gui_World,
                    null, -- the_human_graphics_Model,
                    null, -- the_human_physics_Model,
                    mass => 1.0,
                    is_Kinematic => False);

   --  my_Human.base_Sprite.rotate (to_spin => x_Rotation_from (to_Radians (0.0)));
--     my_Human.base_Sprite.move   ((0.0, 2.0, 0.0));
   the_Applet.gui_World.add (my_Human.base_Sprite, and_Children => True);      -- Add the human
--     my_Human.base_Sprite.move ((0.0,  0.0,  0.0));                             --


--     my_Human.skin_Sprite.rotate (to_spin =>  (x_Rotation_from (to_Radians (0.0))));
--     my_Human.skin_Sprite.move   ((0.0, 2.0, 0.0));
   the_Applet.gui_World.add (my_Human.skin_Sprite);                                 -- Add human skin.


   the_Applet.gui_World.add (the_Ground);                         -- Add the ground
   the_Ground.Site_is ((0.0,  -10.0,  0.0));                      --

      my_Human.motion_Mode_is (gel.Human_v1.Animation);

--     my_Human.enable_Graphics;
--     my_Human.attach_program_Parameters_to_model_Faces;


   next_render_Time := ada.Calendar.clock;

   while the_Applet.is_open
   loop
      --  my_Human.base_Sprite.apply_Force ((0.0, 100.0, 0.0));
      --  my_Human.Sprite (for_Bone => gel.human_Types_v1.upper_Arm_R).apply_Force ((0.0, 10000.0, 0.0));
      --  gel.Human_v1.Sprite (my_Human, for_Bone => gel.human_Types_v1.upper_Arm_R).apply_Force ((0.0, 100.0, 0.0));

      the_Applet.gui_World.evolve; -- (by => 1.0/60.0);      -- Evolve the world.
      my_Human  .evolve (the_Applet.gui_World.Age);
      the_Applet.freshen;                              -- Handle any new events and update the screen.

      next_render_Time := next_render_Time + 1.0/60.0;
      delay until next_render_Time;
   end loop;

--     opengl.IO.stop_Capture;

   the_Applet.destroy;

--  exception
--     when E : others =>
--        put_Line (Exception_Information (E));
end launch_human_Model_v1;
