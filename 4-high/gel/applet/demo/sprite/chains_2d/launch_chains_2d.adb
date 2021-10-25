with
     gel.Window.sdl,
     gel.Applet.gui_world,
     gel.Forge,
     gel.Sprite,
     gel.Joint,

     Physics,
     openGL.Palette;

pragma unreferenced (gel.Window.sdl);


procedure launch_Chains_2d
--
--  Creates a chain of balls in a 2D space.
--
is
   use gel.Forge,
       gel.Applet,
       gel.Math,
       opengl.Palette;

   the_Applet : gel.Applet.gui_World.view := new_gui_Applet ("Chains 2D",
                                                             1536, 864,
                                                             space_Kind => physics.Box2D);

   the_Ground : constant gel.Sprite.view  := new_rectangle_Sprite (the_Applet.gui_World,
                                                                   Mass   =>   0.0,
                                                                   Width  => 100.0,
                                                                   Height =>   1.0,
                                                                   Color  => apple_Green);
begin
   the_Applet.gui_World .Gravity_is    ((0.0, -10.0,   0.0));
   the_Applet.gui_Camera.Site_is       ((0.0, -30.0, 100.0));
   the_Applet.Renderer  .Background_is (Grey);
   the_Applet.enable_simple_Dolly      (in_World => gui_World.gui_world_Id);

   the_Ground.Site_is ((0.0, -40.0, 0.0));
   the_Applet.gui_World.add (the_Ground, and_Children => False);

   --  Add joints.
   --
   declare
      ball_Count    : constant := 39;
      the_root_Ball : constant gel.Sprite.view  :=                     new_circle_Sprite (the_Applet.gui_World, Mass =>  0.0);
      the_Balls     : constant gel.Sprite.views := (1 .. ball_Count => new_circle_Sprite (the_Applet.gui_World, Mass =>  1.0));

      Parent    : gel.Sprite.view := the_root_Ball;
      new_Joint : gel.Joint .view;
   begin
      for i in the_Balls'Range
      loop
         the_Balls (i).Site_is ((Real (-i),  0.0,  0.0));

         Parent.attach_via_Hinge (the_Child  => the_Balls (i),
                                  pivot_Axis => (0.0, 0.0, 1.0),
                                  low_Limit  => to_Radians (-180.0),
                                  high_Limit => to_Radians ( 180.0),
                                  new_joint  => new_Joint);
         Parent := the_Balls (i);
      end loop;

      the_Applet.gui_World.add (the_root_Ball, and_Children => True);
   end;

   while the_Applet.is_open
   loop
      the_Applet.freshen;     -- Handle any new events, evolve physics and update the screen.
   end loop;

   gel.Applet.gui_world.free (the_Applet);
end launch_Chains_2d;
