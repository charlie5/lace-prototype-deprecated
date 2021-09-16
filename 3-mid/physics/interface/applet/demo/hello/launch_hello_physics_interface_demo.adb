with
     physics.Space,
     physics.Shape,
     physics.Object,
     physics.Forge,

     ada.Text_IO;

procedure launch_hello_physics_interface_Demo
--
-- Simply exercises the physics interface.
--
is
   use physics.Math,
       physics.Forge,
       ada.Text_IO;

   the_Space  : constant physics.Space .view := new_Space (Physics.Box2d);

   the_Sphere : constant physics.Shape .view := the_Space.new_circle_Shape;
   the_Box    : constant physics.Shape .view := the_Space.new_circle_Shape;

   the_Ball   : constant physics.Object.view := the_Space.new_Object (of_shape     => the_Sphere,
                                                             of_mass      => 1.0,
                                                             friction     => 0.5,
                                                             restitution  => 0.5,
                                                             at_site      => (0.0,  10.0, 0.0),
                                                             is_kinematic => False);

   the_Ground : constant physics.Object.view := the_Space.new_Object (of_shape     => the_Box,
                                                             of_mass      => 0.0,
                                                             friction     => 0.5,
                                                             restitution  => 0.5,
                                                             at_site      => (0.0, 0.0, 0.0),
                                                             is_kinematic => False);
begin
   the_Space.add (the_Ball);
   the_Space.add (the_Ground);

   for i in 1 .. 100
   loop
      the_Space.evolve (by => 1.0/60.0);

      put_Line (  "Sites ~ Ball => "   & Image (the_Ball  .Site)
                & "        Ground => " & Image (the_Ground.Site));
   end loop;
end launch_hello_physics_interface_Demo;
