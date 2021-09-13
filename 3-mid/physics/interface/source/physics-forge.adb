with
     bullet_physics.Space,
     box2d_physics .Space;

package body physics.Forge
is
   ----------
   --- Space
   --

   function new_Space (Kind : in space_Kind) return Space.view
   is
      Self : Space.view;
   begin
      case Kind
      is
         when Bullet =>
            Self := Space.view' (new bullet_physics.Space.item' (bullet_physics.Space.to_Space));

         when Box2d =>
            Self := Space.view' (new  box2d_physics.Space.item' (box2d_physics.Space.to_Space));
      end case;

      return Self;
   end new_Space;


end physics.Forge;
