with
     physics.Space,
     physics.Shape,
     physics.Object,
     physics.Forge,

     ada.Text_IO;

procedure launch_hello_physics_interface_2D_Demo
--
-- Drops a circle onto a rectangle.
--
is
   use physics.Math,
       physics.Forge,
       ada.Text_IO;

   the_Space  : constant physics.Space .view := new_Space (Physics.Box2d);

   the_Sphere : constant physics.Shape .view := the_Space.new_circle_Shape;
   the_Box    : constant physics.Shape .view := the_Space.new_polygon_Shape (Vertices => (1 => (-1.0, -1.0),
                                                                                          2 => ( 1.0, -1.0),
                                                                                          3 => ( 1.0,  1.0),
                                                                                          4 => (-1.0,  1.0)));

   the_Ball   : constant physics.Object.view := the_Space.new_Object (of_Shape     => the_Sphere,
                                                                      of_Mass      => 1.0,
                                                                      Friction     => 0.5,
                                                                      Restitution  => 0.5,
                                                                      at_Site      => (0.0,  10.0, 0.0),
                                                                      is_Kinematic => False);

   the_Ground : constant physics.Object.view := the_Space.new_Object (of_Shape     => the_Box,
                                                                      of_Mass      => 0.0,
                                                                      Friction     => 0.5,
                                                                      Restitution  => 0.5,
                                                                      at_Site      => (0.0, -1.5, 0.0),
                                                                      is_Kinematic => False);
begin
   the_Space.add (the_Ball);
   the_Space.add (the_Ground);

   for i in 1 .. 200
   loop
      the_Space.evolve (by => 1.0/60.0);

      put_Line (  "Sites ~ Ball => "   & Image (the_Ball  .Site)
                & "        Ground => " & Image (the_Ground.Site));
   end loop;
end launch_hello_physics_interface_2D_Demo;
