with
     physics.Space;

package physics.Forge
--
-- Provides constructors for physics classes.
--
is
   type Real_view is access all math.Real;


   ----------
   --- Space
   --

   function new_Space (Kind : in space_Kind)  return Space.view;


end physics.Forge;
