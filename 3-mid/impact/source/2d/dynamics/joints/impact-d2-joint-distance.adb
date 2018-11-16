with impact.d2.Solid;

package body impact.d2.Joint.distance
--
--  1-D constrained system
--  m (v2 - v1) = lambda
--  v2 + (beta/h) * x1 + gamma * lambda = 0, gamma has units of inverse mass.
--  x2 = x1 + h * v2
--
--  1-D mass-damper-spring system
--  m (v2 - v1) + h * d * v2 + h * k *
--
--  C = norm(p2 - p1) - L
--  u = (p2 - p1) / norm(p2 - p1)
--  Cdot = dot(u, v2 + cross(w2, r2) - v1 - cross(w1, r1))
--  J = [-u -cross(r1, u) u cross(r2, u)]
--  K = J * invM * JT
--   = invMass1 + invI1 * cross(r1, u)^2 + invMass2 + invI2 * cross(r2, u)^2
--
is





   function to_b2DistanceJointDef return b2DistanceJointDef
   is
      Self : b2DistanceJointDef;
   begin
      Self.kind         := e_distanceJoint;
      Self.localAnchorA := (0.0, 0.0);
      Self.localAnchorB := (0.0, 0.0);
      Self.length       := 1.0;
      Self.frequencyHz  := 0.0;
      Self.dampingRatio := 0.0;

      return Self;
   end to_b2DistanceJointDef;



   --  Initialize the bodies, anchors, and length using the world anchors.
   --
   procedure initialize (Self : in out b2DistanceJointDef;   bodyA,   bodyB   : Solid_view;
                                                             anchorA, anchorB : in     b2Vec2)
   is
      use impact.d2.Solid;
--        use -- impact.d2.orbs.Solid,
--            impact.d2.Solid;
      d : b2Vec2;
   begin
      Self.bodyA := bodyA;
      Self.bodyB := bodyB;

      Self.localAnchorA := Self.bodyA.GetLocalPoint (anchorA);
      Self.localAnchorB := Self.bodyB.GetLocalPoint (anchorB);

      d           := anchorB - anchorA;
      Self.length := Length (d);
   end initialize;






   function  GetLength (Self : in     b2DistanceJoint)        return float32
   is
   begin
      return Self.m_length;
   end GetLength;



   procedure SetLength (Self : in out b2DistanceJoint;   length : in float32)
   is
   begin
      self.m_length := length;
   end SetLength;



   function  GetFrequency (Self : in     b2DistanceJoint)    return float32
   is
   begin
      return Self.m_frequencyHz;
   end GetFrequency;


   procedure SetFrequency (Self : in out b2DistanceJoint;   hz : in float32)
   is
   begin
      self.m_frequencyHz := hz;
   end SetFrequency;




   function  GetDampingRatio (Self : in     b2DistanceJoint)       return float32
   is
   begin
      return Self.m_dampingRatio;
   end GetDampingRatio;


   procedure SetDampingRatio (Self : in out b2DistanceJoint;   ratio : in float32)
   is
   begin
      self.m_dampingRatio := ratio;
   end SetDampingRatio;




   function to_b2DistanceJoint (def : in b2DistanceJointDef'Class) return b2DistanceJoint
   is
      Self : b2DistanceJoint;
   begin
      define (b2Joint (Self),  def);

      Self.m_localAnchor1 := def.localAnchorA;
      Self.m_localAnchor2 := def.localAnchorB;
      Self.m_length       := def.length;
      Self.m_frequencyHz  := def.frequencyHz;
      Self.m_dampingRatio := def.dampingRatio;
      Self.m_impulse      := 0.0;
      Self.m_gamma        := 0.0;
      Self.m_bias         := 0.0;

      return Self;
   end to_b2DistanceJoint;






   overriding procedure InitVelocityConstraints  (Self : in out b2DistanceJoint;   step : in b2TimeStep)
   is
      b1 : Solid_view renames Self.m_bodyA;
      b2 : Solid_view renames Self.m_bodyB;

      r1, r2     : b2Vec2;
      length     : float32;
      cr1u, cr2u : float32;
      invMass    : float32;

      P          : b2Vec2;
   begin
      --  Compute the effective mass matrix.
      r1       := b2Mul (b1.GetTransform.R, Self.m_localAnchor1 - b1.GetLocalCenter);
      r2       := b2Mul (b2.GetTransform.R, Self.m_localAnchor2 - b2.GetLocalCenter);

      Self.m_u := b2.m_sweep.c + r2 - b1.m_sweep.c - r1;

      --  Handle singularity.
      length := math.Length (Self.m_u);
      if length > b2_linearSlop then
         Self.m_u := Self.m_u * (1.0 / length);
      else
         Self.m_u := (0.0, 0.0);
      end if;

      cr1u    := b2Cross (r1, Self.m_u);
      cr2u    := b2Cross (r2, Self.m_u);
      invMass :=   b1.m_invMass.all + b1.m_invI.all * cr1u * cr1u
                 + b2.m_invMass.all + b2.m_invI.all * cr2u * cr2u;

      if invMass /= 0.0 then
         Self.m_mass := 1.0 / invMass;
      else
         Self.m_mass := 0.0;
      end if;


      if Self.m_frequencyHz > 0.0 then
         declare
            C     : constant float32 := length - Self.m_length;
            omega : constant float32 := 2.0 * b2_pi * Self.m_frequencyHz;                   -- Frequency
            d     : constant float32 := 2.0 * Self.m_mass * Self.m_dampingRatio * omega;   -- Damping coefficient
            k     : constant float32 := Self.m_mass * omega * omega;                        -- Spring stiffness
         begin
            --  magic formulas
            Self.m_gamma := step.dt * (d + step.dt * k);

            if Self.m_gamma /= 0.0 then
               Self.m_gamma := 1.0 / Self.m_gamma;
            else
               Self.m_gamma := 0.0;
            end if;

            Self.m_bias := C * step.dt * k * Self.m_gamma;
            Self.m_mass := invMass + Self.m_gamma;

            if Self.m_mass /= 0.0 then
               Self.m_mass := 1.0 / Self.m_mass;
            else
               Self.m_mass := 0.0;
            end if;
         end;
      end if;

      if step.warmStarting then
         --  Scale the impulse to support a variable time step.
         Self.m_impulse := Self.m_impulse * step.dtRatio;

         P                        := Self.m_impulse * Self.m_u;
         b1.m_linearVelocity.all  := b1.m_linearVelocity.all  - b1.m_invMass.all * P;
         b1.m_angularVelocity.all := b1.m_angularVelocity.all - b1.m_invI.all    * b2Cross (r1, P);
         b2.m_linearVelocity.all  := b2.m_linearVelocity.all  + b2.m_invMass.all * P;
         b2.m_angularVelocity.all := b2.m_angularVelocity.all + b2.m_invI.all    * b2Cross (r2, P);
      else
         Self.m_impulse := 0.0;
      end if;

   end InitVelocityConstraints;




   overriding procedure SolveVelocityConstraints (Self : in out b2DistanceJoint;   step : in b2TimeStep)
   is
      pragma Unreferenced (step);

      b1      : Solid_view renames Self.m_bodyA;
      b2      : Solid_view renames Self.m_bodyB;

      r1      : constant b2Vec2  := b2Mul (b1.GetTransform.R, Self.m_localAnchor1 - b1.GetLocalCenter);
      r2      : constant b2Vec2  := b2Mul (b2.GetTransform.R, Self.m_localAnchor2 - b2.GetLocalCenter);

      --  Cdot = dot(u, v + cross(w, r))
      --
      v1      : constant b2Vec2  := b1.m_linearVelocity.all + b2Cross (b1.m_angularVelocity.all, r1);
      v2      : constant b2Vec2  := b2.m_linearVelocity.all + b2Cross (b2.m_angularVelocity.all, r2);
      Cdot    : constant float32 := b2Dot (Self.m_u, v2 - v1);

      impulse : constant float32 := -Self.m_mass * (Cdot + Self.m_bias + Self.m_gamma * Self.m_impulse);

      P       : b2Vec2;
   begin
      Self.m_impulse := Self.m_impulse + impulse;

      P := impulse * Self.m_u;
      b1.m_linearVelocity.all  := b1.m_linearVelocity.all  - b1.m_invMass.all * P;
      b1.m_angularVelocity.all := b1.m_angularVelocity.all - b1.m_invI.all    * b2Cross (r1, P);
      b2.m_linearVelocity.all  := b2.m_linearVelocity.all  + b2.m_invMass.all * P;
      b2.m_angularVelocity.all := b2.m_angularVelocity.all + b2.m_invI.all    * b2Cross (r2, P);
   end SolveVelocityConstraints;





   overriding function  SolvePositionConstraints (Self : access b2DistanceJoint;   baumgarte : in float32) return Boolean
   is
      pragma Unreferenced (baumgarte);

      b1      : Solid_view renames Self.m_bodyA;
      b2      : Solid_view renames Self.m_bodyB;
   begin
      if Self.m_frequencyHz > 0.0 then
         return True;   -- There is no position correction for soft distance constraints.
      end if;

      declare
         r1      : constant b2Vec2 := b2Mul (b1.GetTransform.R, Self.m_localAnchor1 - b1.GetLocalCenter);
         r2      : constant b2Vec2 := b2Mul (b2.GetTransform.R, Self.m_localAnchor2 - b2.GetLocalCenter);

         d       : aliased b2Vec2 := b2.m_sweep.c + r2 - b1.m_sweep.c - r1;

         length  : constant float32 := Normalize (d'Access);
         C       : float32 := length - Self.m_length;

         impulse : float32;
         P       : b2Vec2;
      begin
         C := b2Clamp (C, -b2_maxLinearCorrection, b2_maxLinearCorrection);

         impulse  := -Self.m_mass * C;
         Self.m_u := d;
         P        := impulse * Self.m_u;

         b1.m_sweep.c := b1.m_sweep.c - b1.m_invMass.all * P;
         b1.m_sweep.a := b1.m_sweep.a - b1.m_invI.all * b2Cross (r1, P);
         b2.m_sweep.c := b2.m_sweep.c + b2.m_invMass.all * P;
         b2.m_sweep.a := b2.m_sweep.a + b2.m_invI.all * b2Cross (r2, P);

         b1.SynchronizeTransform;
         b2.SynchronizeTransform;

         return abs (C) < b2_linearSlop;
      end;
   end SolvePositionConstraints;




   overriding function GetAnchorA (Self : in b2DistanceJoint) return b2Vec2
   is
   begin
      return Self.m_bodyA.GetWorldPoint (Self.m_localAnchor1);
   end GetAnchorA;



   overriding function GetAnchorB (Self : in b2DistanceJoint) return b2Vec2
   is
   begin
      return Self.m_bodyB.GetWorldPoint (Self.m_localAnchor2);
   end GetAnchorB;



   overriding function GetReactionForce  (Self : in b2DistanceJoint;   inv_dt : in float32) return b2Vec2
   is
   begin
      return (inv_dt * Self.m_impulse) * Self.m_u;
   end GetReactionForce;




   overriding function GetReactionTorque (Self : in b2DistanceJoint;   inv_dt : in float32) return float32
   is
      pragma Unreferenced (inv_dt, Self);
   begin
      return 0.0;
   end GetReactionTorque;



end impact.d2.Joint.distance;
