with
     gel.Window.lumen,
     gel.Applet.gui_world,
     gel.Forge,
     gel.Sprite,

     openGL.Palette,
     openGL.Model.text.lit_colored_textured,

     Physics;

pragma Unreferenced (gel.Window.lumen);


procedure launch_text_sprite_Demo
--
--  Shows a few text sprite.
--
is
   use gel.Applet,
       gel.Math,
       openGL.Palette;

   the_Applet : constant gel.Applet.gui_World.view := gel.Forge.new_gui_Applet ("text sprite Demo",
                                                                                space_Kind => physics.Bullet);

   the_Text_1 : constant gel.Sprite.view := gel.Forge.new_text_Sprite (the_Applet.gui_World,
                                                                       Origin_3D,
                                                                       "Howdy",
                                                                       the_Applet.Font,
                                                                       Green);

   the_Text_2 : constant gel.Sprite.view := gel.Forge.new_text_Sprite (the_Applet.gui_World,
                                                                       Origin_3D,
                                                                       "Doody",
                                                                       the_Applet.Font,
                                                                       Green);
   Counter    : Integer := 0;

begin
   the_Applet.gui_Camera.Site_is ((0.0, 0.0, 50.0));      -- Position the camera.
   the_Applet.enable_simple_Dolly (1);                    -- Enable user camera control via keyboards.

   the_Applet.gui_World.add (the_Text_1);                 -- Add text.
   the_Applet.gui_World.add (the_Text_2);                 -- Add text.

   the_Text_2.Site_is ((0.0, 10.0, 0.0));

   while the_Applet.is_open
   loop
      Counter := Counter + 1;

      if Counter mod 200 = 0
      then
         if openGL.Model.text.lit_colored_textured.view (the_Text_1.graphics_Model).Text.all = "Yay"
         then
            openGL.Model.text.lit_colored_textured.view (the_Text_1.graphics_Model).Text_is ("Howdy");
         else
            openGL.Model.text.lit_colored_textured.view (the_Text_1.graphics_Model).Text_is ("Yay");
         end if;

      end if;

      the_Applet.gui_World.evolve (by => 1.0 / 60.0);
      the_Applet.freshen;                                 -- Handle any new events and update the screen.
   end loop;

   the_Applet.destroy;
end launch_text_sprite_Demo;
