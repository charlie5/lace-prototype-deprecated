with gel.Sprite,
     gel.Human_v1,
     physics.Model,
     openGL.Model.any,
     gel.Window.setup,
     gel.Applet.gui_world,
     openGL.Model.box,
     gel.any_Joint,
     gel.Forge,

--       physics.Motor.spring.angular,
     openGL.Palette,
     float_math.algebra.linear.d3,
     float_math.Random,

     ada.Calendar,
     ada.Exceptions;

with Ada.Text_IO; use Ada.Text_IO;



procedure launch_simple_Animation
--
-- Runs a single animation of a human model.
--
is
   package Math renames float_Math;

   use openGL.Model.box,       gel.Human_v1,
       openGL,                 openGL.Palette,
       Math,                   math.Algebra.linear.d3,
       ada.Calendar,           ada.Exceptions;

   use type math.Real,  math.Index;

   -- Options
   --
--     add_Balls : Boolean := True;
   add_Balls : Boolean := False;


   -- the Applet
   --
   use gel.Applet.gui_world;

   the_Applet : gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("Simple Animation", 1800, 1100);


   -- the Ground
   --
   the_Ground : gel.Sprite.view := gel.Forge.new_box_Sprite (in_world => the_Applet.gui_World,
                                                             mass     => 0.0,
                                                             size     => (50.0, 1.0, 50.0));

     --  human_model_Name : constant String := "assets/human-default-animated-01_01-y_up.dae";
     human_model_Name : constant String := "assets/human-default-animated-01_01.dae";
     --  human_model_Name : constant String := "assets/mh-blender-2.dae";


   --  human_model_Name : constant String := "assets/human_animation.dae";
   --  human_model_Name : constant String := "assets/alfieri.dae";

--  --     human_model_Name : constant String := "./assets/human-golf_swing-v1.dae";
--       human_model_Name : constant String := "assets/human-default-animated-01_01.dae";
--  --     human_model_Name : constant String := "assets/human-default-animated-01_02.dae";
--  --     human_model_Name : constant String := "assets/human-default-animated-01_03.dae";
--  --     human_model_Name : constant String := "assets/human-default-animated-01_04.dae";
--  --     human_model_Name : constant String := "assets/human-default-animated-01_05.dae";

   next_render_Time : ada.calendar.Time;
   Now              : ada.calendar.Time := Ada.Calendar.Clock;
   Counter 	    : Integer           := 0;

begin
   --  gel.Human_v1.Mode_is (Skin);
   gel.Human_v1.Mode_is (Skin_and_Bones);
   --  gel.Human_v1.Mode_is (Bones);

   --- Setup the applet.
   --
   the_Applet.gui_World.Gravity_is ((0.0, -10.0, 0.0));

   the_Applet.gui_Camera.Site_is ((0.0, 0.0, 8.0));     -- Position the camera.
   the_Applet.enable_simple_Dolly (in_World => 1);      -- Enable user camera control via keyboard.
   the_Applet.enable_Mouse (detect_Motion => False);    -- Enable mouse events.


   gel.Human_v1.use_Model (human_model_Name);

   declare
      the_Human : gel.Human_v1.view
        := gel.Human_v1.Forge.new_Human (the_Applet.gui_World,
                                         null,
                                         null,
                                         Mass => 0.0,
                                         is_Kinematic => True);
   begin
      --- Setup the human model.
      --
      --  --  the_Human.base_Sprite.rotate (to_spin => x_Rotation_from (to_Radians (0.0)));
      --  the_Human.base_Sprite.rotate (to_spin => y_Rotation_from (to_Radians (90.0)));
--        the_Human.base_Sprite.move   ((0.0, -10.0, 0.0));

--        the_Human.skin_Sprite.rotate (to_spin =>  (y_Rotation_from (to_Radians (45.0))));
--        the_Human.skin_Sprite.move   ((0.0, 5.0, 0.0));

      the_Applet.gui_World.add (the_Human.base_Sprite, and_Children => True);           -- Add human.
      the_Applet.gui_World.add (the_Human.skin_Sprite);                                 -- Add human skin.

      the_Human.motion_Mode_is (gel.Human_v1.Animation);
      --  the_Human.motion_Mode_is (gel.Human_v1.Physics);


      --- Add balls, if desired.
      --
      if add_Balls
      then
         declare
            the_Balls : array (1 .. 150) of gel.Sprite.view := (others => gel.Forge.new_ball_Sprite (in_World => the_Applet.gui_World,
                                                                                                    Mass     => 1.0,
                                                                                                    Radius   => 0.5,
                                                                                                    Color    => (openGL.Palette.random_Color, Opaque)));
            function random_Site return math.Vector_3
            is
               use math.Random;

               half_Extent : constant math.Real := 25.0 / 2.0;
            begin
               return (random_Real (-half_Extent, half_Extent),
                       0.0,
                       random_Real (-half_Extent, half_Extent));
            end;

         begin
            for i in the_Balls'Range
            loop
               the_Balls (i).Site_is (random_Site);
               the_Applet.gui_World.add (the_Balls (i));
            end loop;
         end;
      end if;


      --- Setup the simulation world.
      --
--        the_Applet.gui_World.add (the_Ground);               -- Add the ground.
--        the_Ground.Site_is ((0.0,  -0.5,  0.0));

      --  the_Human.evolve (0.0); --the_Applet.gui_World.Age);

      --- Run the simulation.
      --
      next_render_Time := ada.calendar.Clock;

      while the_Applet.is_open
      loop
         Counter := Counter + 1;


         the_Applet.gui_World.evolve; -- (by => 1.0/60.0);      -- Evolve the world.

         the_Human .evolve (the_Applet.gui_World.Age);
         --  the_Human .evolve (0.0); --the_Applet.gui_World.Age);
         the_Applet.freshen;                                -- Handle any new events and update the screen.
         delay 0.1;

         --  if Counter mod 600 = 0
         --  then
         --     null;
--  --              the_Human.base_Sprite.move (the_Human.base_Sprite.Site + (0.0, 0.0, -1.0));
--              the_Human.evolve (the_Applet.gui_World.Age);
--           end if;

         next_render_Time := next_render_Time + 1.0/60.0;
         --  delay until next_render_Time;
      end loop;

      the_Applet.destroy;
   end;

--  exception
--     when E : others =>
--        put_Line (Exception_Information (E));
end launch_simple_Animation;
