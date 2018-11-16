with impact.d3.Transform;



package impact.d3.motion_State.default
--
--  The impact.d3.motion_State.default provides a common implementation to synchronize world transforms with offsets.
--
is


   type Item is new impact.d3.motion_State.Item with
      record
         m_graphicsWorldTrans,
         m_centerOfMassOffset,
         m_startWorldTrans    :        Transform_3d;

         m_userPointer        : access Any'Class;
      end record;

   type View is access all Item'Class;



   function  to_motion_State (startTrans, centerOfMassOffset : Transform_3d := impact.d3.Transform.getIdentity) return Item;




   overriding procedure getWorldTransform (Self : in     Item;   worldTrans :    out Transform_3d);
   --
   --  Synchronizes world transform from user to physics.



   overriding procedure setWorldTransform (Self : in out Item;   worldTrans : in     Transform_3d);
   --
   --  Synchronizes world transform from physics to user.
   --  Bullet only calls the update of worldtransform for active objects.


end impact.d3.motion_State.default;
