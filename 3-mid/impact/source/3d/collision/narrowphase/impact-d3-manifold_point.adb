
package body impact.d3.manifold_Point
is

   function to_manifold_Point return Item
   is
      Self : Item;
   begin
      Self.m_userPersistentData         := null;
      Self.m_appliedImpulse             := 0.0;
      Self.m_lateralFrictionInitialized := False;
      Self.m_appliedImpulseLateral1     := 0.0;
      Self.m_appliedImpulseLateral2     := 0.0;
      Self.m_contactMotion1             := 0.0;
      Self.m_contactMotion2             := 0.0;
      Self.m_contactCFM1                := 0.0;
      Self.m_contactCFM2                := 0.0;
      Self.m_lifeTime                   := 0;

      return Self;
   end to_manifold_Point;




   function to_manifold_Point (pointA,
                                pointB   : in Vector_3;
                                normal   : in Vector_3;
                               distance : in Real) return Item
   is
      Self : Item;
   begin
      Self.m_localPointA                := pointA;
      Self.m_localPointB                := pointB;
      Self.m_normalWorldOnB             := normal;
      Self.m_distance1                  := distance;
      Self.m_combinedFriction           := 0.0;
      Self.m_combinedRestitution        := 0.0;
      Self.m_appliedImpulse             := 0.0;
      Self.m_lateralFrictionInitialized := False;
      Self.m_appliedImpulseLateral1     := 0.0;
      Self.m_appliedImpulseLateral2     := 0.0;
      Self.m_contactMotion1             := 0.0;
      Self.m_contactMotion2             := 0.0;
      Self.m_contactCFM1                := 0.0;
      Self.m_contactCFM2                := 0.0;
      Self.m_lifeTime                   := 0;

      Self.mConstraintRow (1).m_accumImpulse := 0.0;
      Self.mConstraintRow (2).m_accumImpulse := 0.0;
      Self.mConstraintRow (3).m_accumImpulse := 0.0;

      return Self;
   end to_manifold_Point;




   function  getLifeTime (Self : in Item) return Integer
   is
   begin
      return Self.m_lifeTime;
   end getLifeTime;




   function  getAppliedImpulse (Self : in Item) return Real
   is
   begin
      return Self.m_appliedImpulse;
   end getAppliedImpulse;



   function  getDistance (Self : in     Item)      return Real
   is
   begin
      return Self.m_distance1;
   end getDistance;



   procedure setDistance (Self : in out Item;   dist : in Real)
   is
   begin
      Self.m_distance1 := dist;
   end setDistance;



   function  getPositionWorldOnA (Self : in Item) return Vector_3
   is
   begin
      return Self.m_positionWorldOnA;
      --  return m_positionWorldOnB + m_normalWorldOnB * m_distance1;
   end getPositionWorldOnA;



   function  getPositionWorldOnB (Self : in Item) return Vector_3
   is
   begin
      return Self.m_positionWorldOnB;
   end getPositionWorldOnB;






end impact.d3.manifold_Point;
