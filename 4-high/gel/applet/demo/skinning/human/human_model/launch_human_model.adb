with gel.Window.setup,
     gel.Applet.gui_world,
     gel.Camera;

with gel.Sprite,
     openGL.Model.box.lit_colored_textured,
     openGL.Model.any,
     openGL.Model.sphere.lit_colored_textured,
     gel.Human,
     gel.Forge;

with opengl.Palette,
     opengl.IO,
     float_Math,
     ada.Calendar,
     ada.Strings.fixed;

with float_Math.algebra.linear.d3;   use  float_Math.algebra.linear.d3;
--  with float_Math.algebra.linear.d4;   use  float_Math.algebra.linear.d4;

with ada.Text_IO;
with GEL.human_Types;
with physics.Model;
with ada.Exceptions;



procedure launch_human_Model
--
-- Drops an gel human model onto a simple box terrain.
--
--
is
   use gel.Applet,    openGL.Model.box,  gel.Human,  gel.human_Types,
       openGL,        opengl.Palette,
       float_Math,
       ada.Calendar,  ada.Strings,  ada.Strings.fixed,  ada.Text_IO,  ada.Exceptions;

   use type math.Real,  opengl.Real;

   the_Applet : constant gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("human Model", 1920, 1200);

   the_Ground : constant gel.Sprite.view     := gel.Forge.new_box_Sprite  (the_Applet.gui_World,
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
   my_Human         : aliased gel.Human.item;

   Counter          :         Integer          := 0;
   next_render_Time :         ada.calendar.Time;

begin
   the_Applet.gui_World.Gravity_is ((0.0, -1.0, 0.0));

   the_Applet.gui_Camera.Site_is ((0.0, 0.0, 40.0));   -- Position the camera
   the_Applet.enable_simple_Dolly (1);                 -- Enable user camera control via keyboards
   the_Applet.enable_Mouse (detect_Motion => False);                            -- Enable mouse events.

   gel.Human.use_Model ("assets/gel/collada/mh-human-dae.dae");
--     gel.Human.use_Model ("assets/gel/collada/alfieri.dae");
   my_Human.define (the_Applet.gui_World,
                    null, -- the_human_graphics_Model,
                    null, -- the_human_physics_Model,
                    mass => 1.0);

   the_Applet.gui_World.add (my_Human.base_Sprite, and_Children => True);               -- Add the human
   my_Human.base_Sprite.move ((0.0,  10.0,  0.0));             --


   the_Applet.gui_World.add (the_Ground);                         -- Add the ground
   the_Ground.Site_is ((0.0,  -10.0,  0.0));                      --

   my_Human.motion_Mode_is (gel.Human.Animation);
   --  the_Human.motion_Mode_is (gel.Human_v1.Physics);

--     my_Human.enable_Graphics;
--     my_Human.attach_program_Parameters_to_model_Faces;


   next_render_Time := ada.Calendar.clock;

   while the_Applet.is_open
   loop

--        if Counter = 150 then
--           my_Human.Sprite (Head).Speed_is ((5.0, 5.0, 0.0));
--           my_Human.Sprite (Head).Gyre_is  ((0.0, 5.0, 0.0));
--
--           my_Human.Sprite (Hand_L).Speed_is ((5.0, 10.0, 0.0));
--           my_Human.Sprite (Hand_L).Gyre_is  ((0.0, 5.0, 0.0));
--
--           my_Human.Sprite (upLeg_L).Speed_is ((-5.0, 10.0, 0.0));
--           my_Human.Sprite (upLeg_L).Gyre_is  ((0.0, 5.0, 0.0));
--
--           my_Human.Sprite (Foot_L).Speed_is ((-5.0, 10.0, 0.0));
--           my_Human.Sprite (Foot_L).Gyre_is  ((0.0, 5.0, 0.0));
--
--           Counter := 0;
--        else
--           Counter := Counter + 1;
--        end if;

      the_Applet.gui_World.evolve; -- (by => 1.0/60.0);    -- evolve the world

      --  my_Human  .evolve;
      my_Human.animate (world_Age => the_Applet.World.Age);
      the_Applet.freshen;                          -- handle any new events and update the screen
      delay 0.5;

      next_render_Time := next_render_Time + 1.0/60.0;
      delay until next_render_Time;
   end loop;

--     opengl.IO.stop_Capture;

   the_Applet.destroy;

exception
   when E : others =>
      put_Line (Exception_Information (E));
end launch_human_Model;
