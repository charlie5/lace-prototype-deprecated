with
     physics.Space;


package physics.Forge
--
-- Provides constructors for physical classes.
--
is
   type Real_view is access all math.Real;


   ----------
   --- Space
   --

   type space_Kind is (Bullet,    Box2d);

   function new_Space (Kind : in space_Kind)  return Space.view;


end physics.Forge;
