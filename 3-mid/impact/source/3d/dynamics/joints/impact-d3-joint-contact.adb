with impact.d3.Transform;
with impact.d3.Vector;


--  #include "impact.d3.Joint.contact.h"
--  #include "BulletDynamics/Dynamics/impact.d3.Object.rigid.h"
--  #include "LinearMath/impact.d3.Vector.h"
--  #include "impact.d3.jacobian_Entry.h"
--  #include "impact.d3.contact_solver_Info.h"
--  #include "LinearMath/impact.d3.min_max.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.manifold_Point.h"

--  #include "impact.d3.Joint.contact.h"
--  #include "BulletDynamics/Dynamics/impact.d3.Object.rigid.h"
--  #include "LinearMath/impact.d3.Vector.h"
--  #include "impact.d3.jacobian_Entry.h"
--  #include "impact.d3.contact_solver_Info.h"
--  #include "LinearMath/impact.d3.min_max.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.manifold_Point.h"




package body impact.d3.Joint.contact
is


   --- Forge
   --

   function to_contact_Joint (contactManifold : access impact.d3.Manifold.item;
                                    rbA, rbB        : access impact.d3.Object.rigid         .item'Class) return Item
   is
   begin
      return Self : Item
      do
         impact.d3.Joint.define (Self,  impact.d3.Joint.CONTACT_CONSTRAINT_TYPE,  rbA, rbB);
         Self.m_contactManifold := contactManifold.all;
      end return;
   end to_contact_Joint;




   overriding procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;




   --- Attributes
   --

   overriding procedure setParam (Self :    out Item;   num   : in impact.d3.Joint.btConstraintParams;
                                             value : in math.Real;
                       axis  : in Integer := -1)
   is
   begin
      raise Program_Error;
   end setParam;




   overriding function  getParam (Self : in     Item;   num   : in impact.d3.Joint.btConstraintParams;
                       axis  : in Integer := -1) return math.Real
   is
   begin
      raise Program_Error;
      return 0.0;
   end getParam;





   overriding procedure getInfo1 (Self : in out Item;   info : out impact.d3.Joint.btConstraintInfo1)
   is
   begin
      null;
   end getInfo1;



   overriding procedure getInfo2 (Self : in out Item;   info : out impact.d3.Joint.btConstraintInfo2)
   is
   begin
      null;
   end getInfo2;





   procedure setContactManifold (Self : in out Item;   contactManifold : access impact.d3.Manifold.item)
   is
   begin
      Self.m_contactManifold := contactManifold.all;
   end setContactManifold;





   function getContactManifold (Self : access Item) return access impact.d3.Manifold.Item'Class
   is
   begin
      return Self.m_contactManifold'Access;
   end getContactManifold;






   --- Utility
   --


   --  Response  between two dynamic objects without friction, assuming 0 penetration depth.
   --
   function resolveSingleCollision (body1                : access impact.d3.Object.rigid.item'Class;
                                    colObj2              : access impact.d3.Object.item'Class;
                                    contactPositionWorld : in     math.Vector_3;
                                    contactNormalOnB     : in     math.Vector_3;
                                    solverInfo           : in     impact.d3.contact_solver_Info.item'Class;
                                    distance             : in     math.Real                   ) return math.Real
   is
      use impact.d3.Vector, impact.d3.Transform,
          Math, math.Vectors;
      use type impact.d3.Object.rigid.view;

      body2               : constant impact.d3.Object.rigid.view       := impact.d3.Object.rigid.view (colObj2);

      normal              : constant math.Vector_3 := contactNormalOnB;

      rel_pos1            : constant math.Vector_3 := contactPositionWorld - getOrigin (body1  .getWorldTransform).all;
      rel_pos2            : constant math.Vector_3 := contactPositionWorld - getOrigin (colObj2.getWorldTransform).all;

      vel1                : constant math.Vector_3 := body1.getVelocityInLocalPoint (rel_pos1);
      vel2                : constant math.Vector_3 := (if body2 /= null then body2.getVelocityInLocalPoint (rel_pos2) else (0.0, 0.0, 0.0));
      vel                 : constant math.Vector_3 := vel1 - vel2;

      rel_vel             : math.Real     := dot (normal, vel);

      combinedRestitution : constant math.Real     := body1.getRestitution * colObj2.getRestitution;
      restitution         : constant math.Real     := combinedRestitution  * (-rel_vel);

      positionalError     : math.Real     := solverInfo.m_erp * (-distance) / solverInfo.m_timeStep;
      velocityError       : math.Real     := -(1.0 + restitution) * rel_vel;  -- * damping;
      denom0              : math.Real     := body1.computeImpulseDenominator (contactPositionWorld, normal);
      denom1              : constant math.Real     := (if body2 /= null then   body2.computeImpulseDenominator (contactPositionWorld, normal)   else   0.0);
      relaxation          : constant math.Real     := 1.0;
      jacDiagABInv        : constant math.Real     := relaxation / (denom0 + denom1);

      penetrationImpulse  : constant math.Real     := positionalError * jacDiagABInv;
      velocityImpulse     : constant math.Real     := velocityError   * jacDiagABInv;

      normalImpulse       : math.Real     := penetrationImpulse + velocityImpulse;

   begin
      normalImpulse := (if 0.0 > normalImpulse then    0.0   else   normalImpulse);

      body1.applyImpulse (normal * (normalImpulse),  rel_pos1);

      if body2 /= null then
         body2.applyImpulse (-normal * (normalImpulse),  rel_pos2);
      end if;


      return normalImpulse;
   end resolveSingleCollision;




end impact.d3.Joint.contact;








--  //
--
--  impact.d3.Scalar resolveSingleCollision(
--          impact.d3.Object.rigid* body1,
--          impact.d3.Object* colObj2,
--                  const impact.d3.Vector& contactPositionWorld,
--                  const impact.d3.Vector& contactNormalOnB,
--          const impact.d3.contact_solver_Info& solverInfo,
--                  impact.d3.Scalar distance)
--  {
--          impact.d3.Object.rigid* body2 = impact.d3.Object.rigid::upcast(colObj2);
--
--
--      const impact.d3.Vector& normal = contactNormalOnB;
--
--      impact.d3.Vector rel_pos1 = contactPositionWorld - body1->getWorldTransform().getOrigin();
--      impact.d3.Vector rel_pos2 = contactPositionWorld - colObj2->getWorldTransform().getOrigin();
--
--      impact.d3.Vector vel1 = body1->getVelocityInLocalPoint(rel_pos1);
--          impact.d3.Vector vel2 = body2? body2->getVelocityInLocalPoint(rel_pos2) : impact.d3.Vector(0,0,0);
--      impact.d3.Vector vel = vel1 - vel2;
--      impact.d3.Scalar rel_vel;
--      rel_vel = normal.dot(vel);
--
--      impact.d3.Scalar combinedRestitution = body1->getRestitution() * colObj2->getRestitution();
--      impact.d3.Scalar restitution = combinedRestitution* -rel_vel;
--
--      impact.d3.Scalar positionalError = solverInfo.m_erp *-distance /solverInfo.m_timeStep ;
--      impact.d3.Scalar velocityError = -(1.0f + restitution) * rel_vel;// * damping;
--          impact.d3.Scalar denom0 = body1->computeImpulseDenominator(contactPositionWorld,normal);
--          impact.d3.Scalar denom1 = body2? body2->computeImpulseDenominator(contactPositionWorld,normal) : 0.f;
--          impact.d3.Scalar relaxation = 1.f;
--          impact.d3.Scalar jacDiagABInv = relaxation/(denom0+denom1);
--
--      impact.d3.Scalar penetrationImpulse = positionalError * jacDiagABInv;
--      impact.d3.Scalar velocityImpulse = velocityError * jacDiagABInv;
--
--      impact.d3.Scalar normalImpulse = penetrationImpulse+velocityImpulse;
--      normalImpulse = 0.f > normalImpulse ? 0.f: normalImpulse;
--
--          body1->applyImpulse(normal*(normalImpulse), rel_pos1);
--      if (body2)
--                  body2->applyImpulse(-normal*(normalImpulse), rel_pos2);
--
--      return normalImpulse;
--  }
