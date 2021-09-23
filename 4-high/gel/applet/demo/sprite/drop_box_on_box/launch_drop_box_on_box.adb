with
     gel.Forge,
     gel.Window.lumen,
     gel.Applet.gui_world,
     gel.Camera,
     gel.Sprite,
     physics.Model,

     openGL.Model.box.colored,
     openGL.Palette,

     Ada.Text_IO,
     Ada.Exceptions;

pragma Unreferenced (gel.window.lumen);

procedure launch_drop_Box_on_Box
--
--  Drops a box onto a box 'terrain'.
--
is
   use gel.Applet,    openGL.Model.box,
       openGL,        opengl.Palette,
       Ada.Text_IO;

   the_Applet : constant gel.Applet.gui_World.view
     := gel.Forge.new_gui_Applet ("drop Ball on Box");


   the_ground_graphics_Model : constant openGL.Model.box.colored.view
     := openGL.Model.box.colored.new_Box (size => (10.0, 0.5, 10.0),
                                                      faces => (front => (colors => (others => (Red,     Opaque))),
                                                                rear  => (colors => (others => (Blue,    Opaque))),
                                                                upper => (colors => (others => (Green,   Opaque))),
                                                                lower => (colors => (others => (Yellow,  Opaque))),
                                                                left  => (colors => (others => (Cyan,    Opaque))),
                                                                right => (colors => (others => (Magenta, Opaque)))));
   the_ground_physics_Model : constant physics.Model.view
     := physics.Model.Forge.new_physics_Model (shape_Info => (kind         => physics.Model.Cube,
                                                              half_extents => (5.0, 0.25, 5.0)));

   the_Ground : constant gel.Sprite.view
     := gel.Sprite.forge.new_Sprite ("demo.Ground",
                                     gel.Sprite.World_view (the_Applet.gui_World),
                                     math.Origin_3d,
                                     the_ground_graphics_Model.all'access,
                                     the_ground_physics_Model);


   the_box_physics_Model : constant physics.Model.view
     := physics.Model.Forge.new_physics_Model (shape_Info => (kind        => physics.Model.cube,
                                                              half_extents => (1.0, 0.5, 1.0)),
                                                   Mass => 1.0);

   the_box_graphics_Model : constant openGL.Model.box.colored.view
     := openGL.Model.box.colored.new_Box (size => (1.0, 1.0, 1.0),
                                          faces => (front => (colors => (others => (Red,        Opaque))),
                                                    rear  => (colors => (others => (Blue,       Opaque))),
                                                    upper => (colors => (others => (dark_Green, Opaque))),
                                                    lower => (colors => (others => (Yellow,     Opaque))),
                                                    left  => (colors => (others => (Cyan,       Opaque))),
                                                    right => (colors => (others => (Magenta,    Opaque)))));
   the_Box : constant gel.Sprite.view
     := gel.Sprite.forge.new_Sprite ("demo.Box",
                                     gel.Sprite.World_view (the_Applet.gui_World),
                                     math.Origin_3d,
                                     the_box_graphics_Model,
                                     the_box_physics_Model);
begin
   the_Applet.gui_Camera.Site_is ((0.0, 5.0, 15.0));      -- Position the camera.
   the_Applet.enable_simple_Dolly (1);                    -- Enable user camera control via keyboards.
   the_Applet.enable_Mouse (detect_Motion => False);      -- Enable the mouse.

   the_Applet.gui_World.add (the_Ground,  and_children => False);               -- Add ground.
   the_Ground.Site_is  ((0.0,  0.0,  0.0));

   the_Applet.gui_World.add (the_Box, and_children => False);                   -- Add ball.
   the_Box.Site_is ((0.0, 10.0,  0.0));

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
