with float_Math,
     Interfaces;


package Impact.d2
--
--  A port of the Box2D physics engine by Erin Catto (http://www.box2d.org).
--
is
   pragma Pure;


   subtype int8  is interfaces.Integer_8;
   subtype int16 is interfaces.Integer_16;
   subtype int32 is interfaces.Integer_32;

   subtype uint8  is interfaces.Unsigned_8;
   subtype uint16 is interfaces.Unsigned_16;
   subtype uint32 is interfaces.Unsigned_32;

   subtype float32 is float_math.Real;


   type   int32_array is array (int32 range <>) of int32;
   type float32_array is array (int32 range <>) of float32;



   b2_maxFloat : constant := Float'Last;
   b2_epsilon  : constant := Float'Epsilon;
   b2_pi       : constant := float_math.Pi;



   --- Global tuning constants based on meters-kilograms-seconds (MKS) units.
   --

   ---  Collision
   --

   b2_maxManifoldPoints         : constant := 2;     -- The maximum number of contact points between two convex shapes.
   b2_maxPolygonVertices : constant := 8;     -- The maximum number of vertices on a convex polygon.

   b2_aabbExtension : constant := 0.1;
   --
   --  This is used to fatten AABBs in the dynamic tree. This allows proxies
   --  to move by a small amount without triggering a tree adjustment.
   --  This is in meters.


   b2_aabbMultiplier : constant := 2.0;
   --
   --  This is used to fatten AABBs in the dynamic tree. This is used to predict
   --  the future position based on the current displacement.
   --  This is a dimensionless multiplier.

   b2_linearSlop : constant := 0.005;
   --
   --  A small length used as a collision and constraint tolerance. Usually it is
   --  chosen to be numerically significant, but visually insignificant.

   b2_angularSlop : constant := 2.0 / 180.0 * b2_pi;
   --
   --  A small angle used as a collision and constraint tolerance. Usually it is
   --  chosen to be numerically significant, but visually insignificant.

   b2_polygonRadius : constant := 2.0 * b2_linearSlop;
   --
   --  The radius of the polygon/edge shape skin. This should not be modified. Making
   --  this smaller means polygons will have an insufficient buffer for continuous collision.
   --  Making it larger may create artifacts for vertex collision.



   --- Dynamics
   --

   b2_maxTOIContacts : constant := 32;      -- Maximum number of contacts to be handled to solve a TOI impact.


   b2_velocityThreshold : constant := 1.0;
   --
   --  A velocity threshold for elastic collisions. Any collision with a relative linear
   --  velocity below this threshold will be treated as inelastic.

   b2_maxLinearCorrection : constant := 0.2;
   --
   --  The maximum linear position correction used when solving constraints. This helps to
   --  prevent overshoot.

   b2_maxAngularCorrection : constant := 8.0 / 180.0 * b2_pi;
   --
   --  The maximum angular position correction used when solving constraints. This helps to
   --  prevent overshoot.

   b2_maxTranslation : constant := 2.0;
   --
   --  The maximum linear velocity of a body. This limit is very large and is used
   --  to prevent numerical problems. You shouldn't need to adjust this.

   b2_maxTranslationSquared : constant := b2_maxTranslation * b2_maxTranslation;

   b2_maxRotation        : constant := 0.5 * b2_pi;
   --
   --  The maximum angular velocity of a body. This limit is very large and is used
   --  to prevent numerical problems. You shouldn't need to adjust this.

   b2_maxRotationSquared : constant := b2_maxRotation * b2_maxRotation;

   b2_contactBaumgarte : constant := 0.2;
   --
   --  This scale factor controls how fast overlap is resolved. Ideally this would be 1 so
   --  that overlap is removed in one time step. However using values close to 1 often lead
   --  to overshoot.


   --- Sleep
   --
   b2_timeToSleep           : constant := 0.5;                     -- The time that a body must be still before it will go to sleep.
   b2_linearSleepTolerance  : constant := 0.01;                    -- A body cannot sleep if its linear velocity is above this tolerance.
   b2_angularSleepTolerance : constant := 2.0 / 180.0 * b2_pi;     -- A body cannot sleep if its angular velocity is above this tolerance.



   --- Mixing laws - Feel free to customize these.
   --
   function b2MixFriction    (Friction1,    Friction2    : float32) return float32;
   function b2MixRestitution (restitution1, restitution2 : float32) return float32;



   --  Ada equivalent of C's void*.
   --
   type Any is interface;


   --- Utility
   --

   generic
      type T is private;
   procedure swap_any (a, b : in out T);




--  private


   --  Internal use.
   --
   type b2TimeStep is
      record
         dt                 : float32;   -- time step
         inv_dt             : float32;   -- inverse time step (0 if dt == 0).
         dtRatio            : float32;   -- dt * inv_dt0
         velocityIterations : int32;
         positionIterations : int32;
         warmStarting       : Boolean;
      end record;



     b2_maxSubSteps : constant := 8;   -- Maximum number of sub-steps per contact in continuous physics simulation.


end Impact.d2;
