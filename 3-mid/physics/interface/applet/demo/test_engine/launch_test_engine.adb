with
     physics.Space,
     physics.Shape,
     physics.Object,
     physics.Forge,
     physics.Engine,
     ada.text_IO;


procedure launch_test_Engine
--
-- Simply exercises the physics engine.
--
is
   use physics.Math,
       physics.Forge,
       ada.text_IO;

   the_Space  : constant physics.Space.view  := new_Space (Physics.Box2d);

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
   the_Engine : aliased physics.Engine.item;

begin
--     the_Engine.start (space_Kind => Physics.Box2d);
   the_Engine.start (the_Space);

--     the_Engine.add (the_Ground);
   the_Engine.add (the_Ball);

--     for Count in 1 .. 100
   loop
--        the_Space.evolve (by => 1.0/60.0);
      delay 1.0/500.0;
      put_Line (  "Sites ~ Ball => "   & Image (the_Ball  .Site)
                & "        Ground => " & Image (the_Ground.Site));
   end loop;

   the_Engine.stop;


--     for Count in 1 .. 100
--     loop
--        the_Space.evolve (by => 1.0/60.0);
--
--        put_Line (  "Sites ~ Ball => "   & Image (the_Ball  .Site)
--                  & "        Ground => " & Image (the_Ground.Site));
--     end loop;

end launch_test_Engine;
