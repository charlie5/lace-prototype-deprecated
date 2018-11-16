package body Impact.d2
--
--  A port of the Box2D physics engine.
--
is


   function b2MixFriction (Friction1, Friction2 : float32) return float32
   is
      use float_math.Functions;
   begin
      return SqRt (friction1 * friction2);
   end b2MixFriction;




   function b2MixRestitution (restitution1, restitution2 : float32) return float32
   is
   begin
      if restitution1 > restitution2 then return restitution1;
      else                                return restitution2;
      end if;
   end b2MixRestitution;





   procedure swap_any (a, b : in out T)
   is
      Pad : constant T := a;
   begin
      a := b;
      b := Pad;
   end swap_any;





end Impact.d2;
