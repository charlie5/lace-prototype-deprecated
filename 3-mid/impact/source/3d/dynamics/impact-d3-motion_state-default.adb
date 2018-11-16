package body impact.d3.motion_State.default
is


   function  to_motion_State (startTrans, centerOfMassOffset : Transform_3d := impact.d3.Transform.getIdentity) return Item
   is
      Self : Item;
   begin
      Self.m_graphicsWorldTrans := startTrans;
      Self.m_centerOfMassOffset := centerOfMassOffset;
      Self.m_startWorldTrans    := startTrans;

      return Self;
   end to_motion_State;



--          impact.d3.motion_State.default(const impact.d3.Transform& startTrans = impact.d3.Transform::getIdentity(),const impact.d3.Transform& centerOfMassOffset = impact.d3.Transform::getIdentity())
--                  : m_graphicsWorldTrans(startTrans),
--                  m_centerOfMassOffset(centerOfMassOffset),
--                  m_startWorldTrans(startTrans),
--                  m_userPointer(0)
--
--          {
--          }



   overriding procedure getWorldTransform (Self : in     Item;   worldTrans :    out Transform_3d)
   is
      use linear_Algebra_3d, impact.d3.Transform;
   begin
      WorldTrans := inverse (Self.m_centerOfMassOffset) * Self.m_graphicsWorldTrans;
   end getWorldTransform;


--          virtual void        getWorldTransform(impact.d3.Transform& centerOfMassWorldTrans ) const
--          {
--                          centerOfMassWorldTrans =         m_centerOfMassOffset.inverse() * m_graphicsWorldTrans ;
--          }




   overriding procedure setWorldTransform (Self : in out Item;   worldTrans : in     Transform_3d)
   is
      use linear_Algebra_3d, impact.d3.Transform;
   begin
      Self.m_graphicsWorldTrans := WorldTrans * Self.m_centerOfMassOffset;
   end setWorldTransform;


--          virtual void        setWorldTransform(const impact.d3.Transform& centerOfMassWorldTrans)
--          {
--                          m_graphicsWorldTrans = centerOfMassWorldTrans * m_centerOfMassOffset ;
--          }


end impact.d3.motion_State.default;
