with
     gel.Window.sdl,
     gel.Applet.gui_world,
     gel.Forge,
     gel.Sprite,

     openGL.Palette,
     openGL.Model.text.lit_colored,

     Physics;

pragma unreferenced (gel.Window.sdl);


procedure launch_text_sprite_Demo
--
--  Shows a few text sprites.
--
is
   use gel.Math,
       openGL.Palette;

   the_Applet : constant gel.Applet.gui_World.view := gel.forge.new_gui_Applet ("text sprite Demo",
                                                                                space_Kind => physics.Bullet);

   the_Text_1 : constant gel.Sprite.view := gel.forge.new_text_Sprite (the_Applet.gui_World,
                                                                       Origin_3D,
                                                                       "Howdy",
                                                                       the_Applet.Font,
                                                                       Green);

   the_Text_2 : constant gel.Sprite.view := gel.forge.new_text_Sprite (the_Applet.gui_World,
                                                                       Origin_3D,
                                                                       "Doody",
                                                                       the_Applet.Font,
                                                                       Green);
   text_1_Model : constant openGL.Model.text.lit_colored.view
                                         := openGL.Model.text.lit_colored.view (the_Text_1.graphics_Model);
begin
   the_Applet.gui_Camera.Site_is ((0.0, 0.0, 50.0));      -- Position the camera.
   the_Applet.enable_simple_Dolly (1);                    -- Enable user camera control via keyboards.

   the_Applet.gui_World.add (the_Text_1);
   the_Applet.gui_World.add (the_Text_2);

   the_Text_2.Site_is ((0.0, 10.0, 0.0));

   while the_Applet.is_open
   loop
      if text_1_Model.Text = "Yay"
      then
         text_1_Model.Text_is ("Howdy");
      else
         text_1_Model.Text_is ("Yay");
      end if;

      the_Applet.gui_World.evolve;
      the_Applet.freshen;                                 -- Handle any new events and update the screen.

      delay 0.5;
   end loop;

   the_Applet.destroy;
end launch_text_sprite_Demo;
