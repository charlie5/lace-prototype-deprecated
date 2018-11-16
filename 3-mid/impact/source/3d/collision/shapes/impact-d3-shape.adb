with
     impact.d3.Transform,
     impact.d3.Vector;



package body impact.d3.Shape
is


   function isConvex (Self : in Item'Class) return Boolean
   is
   begin
      return impact.d3.collision.Proxy.isConvex (Self.getShapeType);
   end isConvex;





   function getShapeType (Self : in Item) return impact.d3.collision.Proxy.BroadphaseNativeTypes
   is
   begin
      return Self.m_shapeType;
   end getShapeType;


   procedure setShapeType (Self : in out Item;   To : in impact.d3.collision.Proxy.BroadphaseNativeTypes)
   is
   begin
      self.m_shapeType := To;
   end setShapeType;





   function isPolyhedral (Self : in Item'Class) return Boolean
   is
   begin
      return impact.d3.collision.Proxy.isPolyhedral (Self.getShapeType);
   end isPolyhedral;



   function isConvex2d (Self : in Item'Class) return Boolean
   is
   begin
      return impact.d3.collision.Proxy.isConvex2d (Self.getShapeType);
   end isConvex2d;



   function isNonMoving (Self : in Item'Class) return Boolean
   is
   begin
      return impact.d3.collision.Proxy.isNonMoving (Self.getShapeType);
   end isNonMoving;




   function isConcave  (Self : in Item'Class) return Boolean
   is
   begin
      return impact.d3.collision.Proxy.isConcave (Self.getShapeType);
   end isConcave;





   function isCompound  (Self : in Item'Class) return Boolean
   is
   begin
      return impact.d3.collision.Proxy.isCompound (Self.getShapeType);
   end isCompound;





   function isSoftBody   (Self : in Item'Class) return Boolean
   is
   begin
      return impact.d3.collision.Proxy.isSoftBody (Self.getShapeType);
   end isSoftBody;




   function isInfinite   (Self : in Item'Class) return Boolean
   is
   begin
      return impact.d3.collision.Proxy.isInfinite (Self.getShapeType);
   end isInfinite;





   procedure setUserPointer (Self : in out Item'Class;   userPtr : access Any'Class)
   is
   begin
      Self.m_userPointer := userPtr;
   end setUserPointer;






   function  getUserPointer (Self : in     Item'Class)      return access Any'Class
   is
   begin
      return Self.m_userPointer;
   end getUserPointer;







   --    Make sure this dummy function never changes so that it
   --    can be used by probes that are checking whether the
   --    library is actually installed.
   --
   procedure btBulletCollisionProbe
   is
   begin
      null;
   end btBulletCollisionProbe;






   procedure getBoundingSphere (Self : in Item'Class;   center : out math.Vector_3;
                                                        radius : out math.Real)
   is
      use impact.d3.Vector, math.Vectors;

      tr      : constant Transform_3d := impact.d3.Transform.getIdentity;
      aabbMin,
      aabbMax : math.Vector_3;
   begin
      Self.getAabb (tr, aabbMin, aabbMax);

      radius := length (aabbMax - aabbMin)  * 0.5;
      center :=        (aabbMin + aabbMax)  * 0.5;
   end getBoundingSphere;






   function getAngularMotionDisc (Self : in Item) return math.Real
   is
      use impact.d3.Vector;

      --  todo: cache this value, to improve performance

      center : math.Vector_3;
      disc   : math.Real;
   begin
      Self.getBoundingSphere (center, disc);
      disc := disc + length (center);

      return disc;
   end getAngularMotionDisc;






   function getContactBreakingThreshold (Self : in Item;   defaultContactThresholdFactor : in math.Real) return math.Real
   is
   begin
      return Self.getAngularMotionDisc * defaultContactThresholdFactor;
   end getContactBreakingThreshold;





   procedure calculateTemporalAabb (Self : in Item'Class;   curTrans        : in     Transform_3d;
                                                            linvel          : in     math.Vector_3;
                                                            angvel          : in     math.Vector_3;
                                                            timeStep        : in     math.Real;
                                                            temporalAabbMin,
                                    temporalAabbMax :    out math.Vector_3)
   is
   begin
      -- start with static aabb
      Self.getAabb (curTrans,  temporalAabbMin, temporalAabbMax);

      declare
         use impact.d3.Vector, math.Vectors;

         temporalAabbMaxx : math.Real := temporalAabbMax (1);
         temporalAabbMaxy : math.Real := temporalAabbMax (2);
         temporalAabbMaxz : math.Real := temporalAabbMax (3);

         temporalAabbMinx : math.Real := temporalAabbMin (1);
         temporalAabbMiny : math.Real := temporalAabbMin (2);
         temporalAabbMinz : math.Real := temporalAabbMin (3);

         linMotion        : math.Vector_3 := linvel * timeStep;   -- add linear motion

         angularMotion    : math.Real;
         angularMotion3d  : math.Vector_3;
      begin
         --  todo: simd would have a vector max/min operation, instead of per-element access

         if linMotion (1) > 0.0 then
            temporalAabbMaxx := temporalAabbMaxx + linMotion (1);
         else
            temporalAabbMinx := temporalAabbMinx + linMotion (1);
         end if;

         if linMotion (2) > 0.0 then
            temporalAabbMaxy := temporalAabbMaxy + linMotion (2);
         else
            temporalAabbMiny := temporalAabbMiny + linMotion (2);
         end if;

         if linMotion (3) > 0.0 then
            temporalAabbMaxz := temporalAabbMaxz + linMotion (3);
         else
            temporalAabbMinz := temporalAabbMinz + linMotion (3);
         end if;

         --  add conservative angular motion
         --
         angularMotion   := length (angvel) * Self.getAngularMotionDisc * timeStep;
         angularMotion3d := (angularMotion, angularMotion, angularMotion);

         temporalAabbMin := (temporalAabbMinx, temporalAabbMiny, temporalAabbMinz);
         temporalAabbMax := (temporalAabbMaxx, temporalAabbMaxy, temporalAabbMaxz);

         temporalAabbMin := temporalAabbMin - angularMotion3d;
         temporalAabbMax := temporalAabbMax + angularMotion3d;
      end;
   end calculateTemporalAabb;



end impact.d3.Shape;
