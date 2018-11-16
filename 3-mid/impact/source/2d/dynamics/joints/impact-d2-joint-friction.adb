with impact.d2.Solid;

package body impact.d2.Joint.friction
--
--  Point-to-point constraint
--
--  Cdot = v2 - v1
--      = v2 + cross(w2, r2) - v1 - cross(w1, r1)
--  J = [-I -r1_skew I r2_skew ]
--  Identity used:
--  w k % (rx i + ry j) = w * (-ry i + rx j)
--
--  Angle constraint
--  Cdot = w2 - w1
--  J = [0 0 -1 0 0 1]
--  K = invI1 + invI2
--
is


   function to_b2FrictionJointDef return b2FrictionJointDef
   is
      Self : b2FrictionJointDef;
   begin
      Self.kind         := e_frictionJoint;
      Self.localAnchorA := (0.0, 0.0);
      Self.localAnchorB := (0.0, 0.0);
      Self.maxForce     := 0.0;
      Self.maxTorque    := 0.0;

      return Self;
   end to_b2FrictionJointDef;



   procedure initialize (Self : in out b2FrictionJointDef;   bodyA,   bodyB   : in  Solid_view;
                                                             anchor           : in  b2Vec2)
   is
      use impact.d2.Solid;
   begin
      Self.bodyA := bodyA;
      Self.bodyB := bodyB;

      Self.localAnchorA := Self.bodyA.GetLocalPoint (anchor);
      Self.localAnchorB := Self.bodyB.GetLocalPoint (anchor);
   end initialize;





   function  to_b2FrictionJoint (def : in b2FrictionJointDef'Class) return b2FrictionJoint
   is
      Self : b2FrictionJoint;
   begin
      define (b2Joint (Self),  def);

      Self.m_localAnchorA  := def.localAnchorA;
      Self.m_localAnchorB  := def.localAnchorB;

      Self.m_linearImpulse  := (0.0, 0.0);
      Self.m_angularImpulse := 0.0;

      Self.m_maxForce  := def.maxForce;
      Self.m_maxTorque := def.maxTorque;

      return Self;
   end to_b2FrictionJoint;




   overriding procedure InitVelocityConstraints  (Self : in out b2FrictionJoint;   step : in b2TimeStep)
   is
      bA : Solid_view renames Self.m_bodyA;
      bB : Solid_view renames Self.m_bodyB;

      --  Compute the effective mass matrix.
      rA : b2Vec2 := b2Mul (bA.GetTransform.R, Self.m_localAnchorA - bA.GetLocalCenter);
      rB : b2Vec2 := b2Mul (bB.GetTransform.R, Self.m_localAnchorB - bB.GetLocalCenter);

      --  J = [-I -r1_skew I r2_skew]
      --     [ 0       -1 0       1]
      --  r_skew = [-ry; rx]

      --  Matlab
      --  K = [ mA+r1y^2*iA+mB+r2y^2*iB,  -r1y*iA*r1x-r2y*iB*r2x,          -r1y*iA-r2y*iB]
      --     [  -r1y*iA*r1x-r2y*iB*r2x, mA+r1x^2*iA+mB+r2x^2*iB,           r1x*iA+r2x*iB]
      --     [          -r1y*iA-r2y*iB,           r1x*iA+r2x*iB,                   iA+iB]

      mA : constant float32 := bA.m_invMass.all;
      mB : constant float32 := bB.m_invMass.all;
      iA : constant float32 := bA.m_invI.all;
      iB : constant float32 := bB.m_invI.all;

      K, K1, K2, K3 : b2Mat22;

      P : b2Vec2;
   begin
      K1.col1.x := mA + mB;               K1.col2.x := 0.0;
      K1.col1.y := 0.0;                   K1.col2.y := mA + mB;

      K2.col1.x :=  iA * rA.y * rA.y;     K2.col2.x := -iA * rA.x * rA.y;
      K2.col1.y := -iA * rA.x * rA.y;     K2.col2.y :=  iA * rA.x * rA.x;

      K3.col1.x :=  iB * rB.y * rB.y;     K3.col2.x := -iB * rB.x * rB.y;
      K3.col1.y := -iB * rB.x * rB.y;     K3.col2.y :=  iB * rB.x * rB.x;

      K                 := K1 + K2 + K3;
      Self.m_linearMass := GetInverse (K);

      Self.m_angularMass := iA + iB;

      if Self.m_angularMass > 0.0 then
         Self.m_angularMass := 1.0 / Self.m_angularMass;
      end if;

      if step.warmStarting then
         --  Scale impulses to support a variable time step.
         Self.m_linearImpulse  := Self.m_linearImpulse * step.dtRatio;
         Self.m_angularImpulse := Self.m_angularImpulse * step.dtRatio;

         P := (Self.m_linearImpulse.x,  Self.m_linearImpulse.y);

         bA.m_linearVelocity.all  := bA.m_linearVelocity.all - mA * P;
         bA.m_angularVelocity.all := bA.m_angularVelocity.all - iA * (b2Cross (rA, P) + Self.m_angularImpulse);

         bB.m_linearVelocity.all  := bB.m_linearVelocity.all + mB * P;
         bB.m_angularVelocity.all := bB.m_angularVelocity.all + iB * (b2Cross (rB, P) + Self.m_angularImpulse);
      else
         Self.m_linearImpulse  := (0.0, 0.0);
         Self.m_angularImpulse := 0.0;
      end if;

   end InitVelocityConstraints;




   overriding procedure SolveVelocityConstraints (Self : in out b2FrictionJoint;   step : in b2TimeStep)
   is
      bA : Solid_view renames Self.m_bodyA;
      bB : Solid_view renames Self.m_bodyB;

      vA : b2Vec2  := bA.m_linearVelocity.all;
      wA : float32 := bA.m_angularVelocity.all;
      vB : b2Vec2  := bB.m_linearVelocity.all;
      wB : float32 := bB.m_angularVelocity.all;

      mA : float32 := bA.m_invMass.all;
      mB : float32 := bB.m_invMass.all;
      iA : float32 := bA.m_invI.all;
      iB : float32 := bB.m_invI.all;

      rA : constant b2Vec2  := b2Mul (bA.GetTransform.R,  Self.m_localAnchorA - bA.GetLocalCenter);
      rB : constant b2Vec2  := b2Mul (bB.GetTransform.R,  Self.m_localAnchorB - bB.GetLocalCenter);
   begin
      --  Solve angular friction
      declare
         Cdot       : constant float32 := wB - wA;
         impulse    : float32 := -Self.m_angularMass * Cdot;

         oldImpulse : constant float32 := Self.m_angularImpulse;
         maxImpulse : constant float32 := step.dt * Self.m_maxTorque;
      begin
         Self.m_angularImpulse := b2Clamp (Self.m_angularImpulse + impulse,  -maxImpulse, maxImpulse);
         impulse               := Self.m_angularImpulse - oldImpulse;

         wA := wA - iA * impulse;
         wB := wB + iB * impulse;
      end;

      --  Solve linear friction
      declare
         Cdot       : constant b2Vec2 :=   vB + b2Cross (wB, rB)
                                - vA - b2Cross (wA, rA);

         impulse    : b2Vec2 := -b2Mul (Self.m_linearMass, Cdot);
         oldImpulse : constant b2Vec2 := Self.m_linearImpulse;

         maxImpulse : float32;
      begin
         Self.m_linearImpulse := Self.m_linearImpulse + impulse;

         maxImpulse := step.dt * Self.m_maxForce;

         if LengthSquared (Self.m_linearImpulse) > maxImpulse * maxImpulse then
            Normalize (Self.m_linearImpulse);
            Self.m_linearImpulse := Self.m_linearImpulse * maxImpulse;
         end if;

         impulse := Self.m_linearImpulse - oldImpulse;

         vA := vA - mA * impulse;
         wA := wA - iA * b2Cross (rA, impulse);

         vB := vB + mB * impulse;
         wB := wB + iB * b2Cross (rB, impulse);
      end;

      bA.m_linearVelocity.all  := vA;
      bA.m_angularVelocity.all := wA;
      bB.m_linearVelocity.all  := vB;
      bB.m_angularVelocity.all := wB;

   end SolveVelocityConstraints;





   overriding function  SolvePositionConstraints (Self : access b2FrictionJoint;   baumgarte : in float32) return Boolean
   is
      pragma Unreferenced (baumgarte, Self);
   begin
      return True;
   end SolvePositionConstraints;









   overriding function GetAnchorA (Self : in b2FrictionJoint) return b2Vec2
   is
   begin
      return Self.m_bodyA.GetWorldPoint (Self.m_localAnchorA);
   end GetAnchorA;



   overriding function GetAnchorB (Self : in b2FrictionJoint) return b2Vec2
   is
   begin
      return Self.m_bodyB.GetWorldPoint (Self.m_localAnchorB);
   end GetAnchorB;


   overriding function GetReactionForce  (Self : in b2FrictionJoint;   inv_dt : in float32) return b2Vec2
   is
   begin
      return inv_dt * Self.m_linearImpulse;
   end GetReactionForce;




   overriding function GetReactionTorque (Self : in b2FrictionJoint;   inv_dt : in float32) return float32
   is
   begin
      return inv_dt * Self.m_angularImpulse;
   end GetReactionTorque;




   function  GetMaxForce (Self : in     b2FrictionJoint)       return float32
   is
   begin
      return Self.m_maxForce;
   end GetMaxForce;



   procedure SetMaxForce (Self : in out b2FrictionJoint;   force : in float32)
   is
   begin
      pragma Assert (b2IsValid (force) and then force >= 0.0);

      Self.m_maxForce := force;
   end SetMaxForce;




   function  GetMaxTorque (Self : in     b2FrictionJoint)       return float32
   is
   begin
      return Self.m_maxTorque;
   end GetMaxTorque;



   procedure SetMaxTorque (Self : in out b2FrictionJoint;   torque : in float32)
   is
   begin
      pragma Assert (b2IsValid (torque) and then torque >= 0.0);

      Self.m_maxTorque := torque;
   end SetMaxTorque;


end impact.d2.Joint.friction;
