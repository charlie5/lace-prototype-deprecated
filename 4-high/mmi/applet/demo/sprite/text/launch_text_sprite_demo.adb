with
     mmi.Window.lumen,
     mmi.Applet.gui_world,
     mmi.Forge,
     mmi.Sprite,

     openGL.Palette,
     openGL.Model.text.lit_colored_textured,

     physics.Forge,
     float_Math;

with Ada.Text_IO; use Ada.Text_IO;


procedure launch_text_sprite_Demo
--
--  Drops a ball onto a box 'terrain'.
--
is
   use mmi.Applet, openGL.Palette;

   the_Applet : constant mmi.Applet.gui_World.view := mmi.Forge.new_gui_Applet  ("text sprite Demo",
                                                                                 space_Kind => physics.Bullet);

   the_Text   : constant mmi.Sprite.view           := mmi.Forge.new_text_Sprite (the_Applet.gui_World,
                                                                                 "Howdy",
                                                                                 the_Applet.Font,
                                                                                 Green);

   the_Text_2   : constant mmi.Sprite.view         := mmi.Forge.new_text_Sprite (the_Applet.gui_World,
                                                                                 "Doody",
                                                                                 the_Applet.Font,
                                                                                 Green);

   Counter : Integer := 0;


begin
   the_Applet.gui_Camera.Site_is ((0.0, 0.0, 50.0));      -- Position the camera.
   the_Applet.enable_simple_Dolly (1);                    -- Enable user camera control via keyboards.

   the_Applet.gui_World.add (the_Text);                   -- Add text.
   the_Applet.gui_World.add (the_Text_2);                   -- Add text.

   the_Text_2.Site_is ((0.0, 10.0, 0.0));

   while the_Applet.is_open
   loop
      Counter := Counter + 1;

      if Counter mod 20 = 0
      then
         if openGL.Model.text.lit_colored_textured.view (the_Text.graphics_Model).Text.all = "Yay"
         then
            openGL.Model.text.lit_colored_textured.view (the_Text.graphics_Model).Text_is ("Howdy");
         else
            openGL.Model.text.lit_colored_textured.view (the_Text.graphics_Model).Text_is ("Yay");
         end if;

      end if;

--        if Counter mod 1105 = 0
--        then
--           exit;
--        end if;


      the_Applet.gui_World.evolve (by => 1.0 / 60.0);
      the_Applet.freshen;                                 -- Handle any new events and update the screen.
   end loop;

   the_Applet.destroy;
end launch_text_sprite_Demo;
