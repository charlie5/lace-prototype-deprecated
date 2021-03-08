with
     physics.Space,
     physics.Shape,
     physics.Object,
     physics.Forge,

     ada.text_IO;


procedure launch_hello_physics_interface_Demo
--
-- Simply exercises the physics interface.
--
is
   use physics.Math,
       physics.Forge,
       ada.text_IO;

   the_Space  : physics.Space.view  := physics.Space .view (new_Space (Physics.Box2d));

   the_Sphere : physics.Shape .view := the_Space.new_circle_Shape;
   the_Box    : physics.Shape .view := the_Space.new_circle_Shape;

   the_Ball   : physics.Object.view := the_Space.new_Object (of_shape     => the_Sphere,
                                                             of_mass      => 1.0,
                                                             friction     => 0.5,
                                                             restitution  => 0.5,
                                                             at_site      => (0.0,  10.0, 0.0),
                                                             is_kinematic => False);

   the_Ground : physics.Object.view := the_Space.new_Object (of_shape     => the_Box,
                                                             of_mass      => 0.0,
                                                             friction     => 0.5,
                                                             restitution  => 0.5,
                                                             at_site      => (0.0, 0.0, 0.0),
                                                             is_kinematic => False);
begin
   the_Space.add (the_Ball);
   the_Space.add (the_Ground);

   for Count in 1 .. 100
   loop
      the_Space.evolve (by => 1.0/60.0);

      put_Line (  "Sites ~ Ball => "   & Image (the_Ball  .Site)
                & "        Ground => " & Image (the_Ground.Site));
   end loop;
end launch_hello_physics_interface_Demo;
