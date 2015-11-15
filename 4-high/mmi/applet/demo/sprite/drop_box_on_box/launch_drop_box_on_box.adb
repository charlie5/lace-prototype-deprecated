with mmi.Forge,
     mmi.Window.lumen,
     mmi.Applet.gui_world,
     mmi.Camera,
     mmi.Mouse,

     mmi.Sprite,
     openGL.Model.box.colored,
     openGL.Model.sphere.lit_colored_textured,
     mmi.physics_Model,

     opengl.Palette,
     lace.event.Utility,

     ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;


procedure launch_drop_Box_on_Box
--
--  Drops a box onto a box 'terrain'.
--
--
is
   use mmi.Applet,    openGL.Model.box,
       openGL,        opengl.Palette,
       ada.Calendar;

   the_Applet : constant mmi.Applet.gui_World.view
     := mmi.Forge.new_gui_Applet ("drop Ball on Box");


   the_ground_graphics_Model : openGL.Model.box.colored.view
     := openGL.Model.box.colored.forge.new_Box (scale => (10.0, 0.5, 10.0),
                                                      faces => (front => (colors => (others => (Red,     Opaque))),
                                                                rear  => (colors => (others => (Blue,    Opaque))),
                                                                upper => (colors => (others => (Green,   Opaque))),
                                                                lower => (colors => (others => (Yellow,  Opaque))),
                                                                left  => (colors => (others => (Cyan,    Opaque))),
                                                                right => (colors => (others => (Magenta, Opaque)))));
   the_ground_physics_Model : constant mmi.physics_Model.view
     := mmi.physics_Model.Forge.new_physics_Model (shape_Info => (kind         => mmi.physics_Model.Cube,
                                                                  half_extents => (5.0, 0.25, 5.0)));
--       := mmi.physics_Model.Forge.new_physics_Model (shape_Info => (kind => mmi.physics_Model.a_Sphere, sphere_radius => 1.0));

   the_Ground : mmi.Sprite.view
     := mmi.Sprite.forge.new_Sprite ("demo.Ground",
                                     the_Applet.gui_World,
                                     the_ground_graphics_Model.all'access,
                                     the_ground_physics_Model);


   the_box_physics_Model : constant mmi.physics_Model.view
     := mmi.physics_Model.Forge.new_physics_Model (shape_Info => (kind        => mmi.physics_Model.Cube,
                                                                  half_extents => (1.0, 0.5, 1.0)),
                                                   Mass => 1.0);

   the_box_graphics_Model :  openGL.Model.box.colored.view
     := openGL.Model.box.colored.forge.new_Box (scale => (1.0, 1.0, 1.0),
                                                      faces => (front => (colors => (others => (Red,     Opaque))),
                                                                rear  => (colors => (others => (Blue,    Opaque))),
                                                                upper => (colors => (others => (Green,   Opaque))),
                                                                lower => (colors => (others => (Yellow,  Opaque))),
                                                                left  => (colors => (others => (Cyan,    Opaque))),
                                                                right => (colors => (others => (Magenta, Opaque)))));
   the_Box : mmi.Sprite.view
     := mmi.Sprite.forge.new_Sprite ("demo.Box",
                                     the_Applet.gui_World,
                                     the_box_graphics_Model,
                                     the_box_physics_Model);
begin
   the_Applet.gui_Camera.Site_is ((0.0, 5.0, 15.0));      -- Position the camera.
   the_Applet.enable_simple_Dolly (1);                    -- Enable user camera control via keyboards.
   the_Applet.enable_Mouse (detect_Motion => False);      -- Enable the mouse.

   the_Applet.gui_World.add (the_Ground,  and_children => False);               -- Add box.
   the_Ground.Site_is  ((0.0,  0.0,  0.0));

   the_Applet.gui_World.add (the_Box, and_children => False);                   -- Add ball.
   the_Box.Site_is ((5.0, 10.0,  0.0));

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
end launch_drop_Box_on_Box;
