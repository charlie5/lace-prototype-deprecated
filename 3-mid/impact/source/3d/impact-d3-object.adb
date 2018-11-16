with impact.d3.Transform;




package body impact.d3.Object
is




   procedure define (Self : in out Item)
   is
      use impact.d3.Transform;
   begin
      Self.m_anisotropicFriction        := (1.0, 1.0, 1.0);
      Self.m_hasAnisotropicFriction     := False;
      Self.m_contactProcessingThreshold := math.Real'Last;
      Self.m_collisionFlags             := CF_STATIC_OBJECT;
      Self.m_islandTag1                 := -1;
      Self.m_companionId                := -1;
      Self.m_activationState1           := 1;
      Self.m_deactivationTime           := 0.0;
      Self.m_friction                   := 0.5;
      Self.m_restitution                := 0.0;
      Self.m_internalType               := CO_COLLISION_OBJECT;
      Self.m_hitFraction                := 1.0;
      Self.m_ccdSweptSphereRadius       := 0.0;
      Self.m_ccdMotionThreshold         := 0.0;
      Self.m_checkCollideWith           := False;

      setIdentity (Self.m_worldTransform);
   end define;







   procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;









   function  mergesSimulationIslands (Self : in Item) return Boolean
   is
   begin
      --  Static objects, kinematic and object without contact response don't merge islands.
      --
      return
        (  Self.m_collisionFlags
         and ( CF_STATIC_OBJECT
              or CF_KINEMATIC_OBJECT
              or CF_NO_CONTACT_RESPONSE))  =  0;
   end mergesSimulationIslands;







   function  getAnisotropicFriction (Self : in     Item)     return math.Vector_3
   is
   begin
      return Self.m_anisotropicFriction;
   end getAnisotropicFriction;







   function getBroadphaseHandle (Self : access Item) return access impact.d3.collision.Proxy.item'Class
   is
   begin
      return Self.m_broadphaseHandle;
   end getBroadphaseHandle;



   procedure setBroadphaseHandle (Self : in out Item;  To : access impact.d3.collision.Proxy.item'Class)
   is
   begin
      Self.m_broadphaseHandle := To;
   end setBroadphaseHandle;






   function checkCollideWithOverride (Self : in Item;   co : access Item'Class) return Boolean
   is
      pragma Unreferenced (Self, co);
   begin
      return False;
   end checkCollideWithOverride;





   function getWorldTransform (Self : access Item) return access Transform_3d
   is
   begin
      for row in 1 .. 3 loop
         for col in 1 .. 3 loop
            if not Self.m_worldTransform.Rotation (row, col)'Valid then
               raise Constraint_Error;
            end if;
         end loop;
      end loop;

      return Self.m_worldTransform'Unchecked_Access;
   end getWorldTransform;



   function WorldTransform (Self : in     Item) return        Transform_3d
   is
   begin
      for row in 1 .. 3 loop
         for col in 1 .. 3 loop
            if not Self.m_worldTransform.Rotation (row, col)'Valid then
               raise Constraint_Error;
            end if;
         end loop;
      end loop;

      return Self.m_worldTransform;
   end WorldTransform;





   function getFriction (Self : in Item) return math.Real
   is
   begin
      return Self.m_friction;
   end getFriction;



   function getRestitution (Self : in Item) return math.Real
   is
   begin
      return Self.m_restitution;
   end getRestitution;




   function getCollisionFlags (Self : in Item) return Flags
   is
   begin
      return Self.m_collisionFlags;
   end getCollisionFlags;




   function getCollisionShape (Self : in Item) return access impact.d3.Shape.item'Class
   is
   begin
      return Self.m_collisionShape;
   end getCollisionShape;








   procedure setAnisotropicFriction (Self : in out Item;   To : in  math.Vector_3)
   is
   begin
      Self.m_anisotropicFriction    := To;
      Self.m_hasAnisotropicFriction :=         (To (1) /= 1.0)
                                       or else (To (2) /= 1.0)
                                       or else (To (3) /= 1.0);
   end setAnisotropicFriction;






   function  hasAnisotropicFriction (Self : in Item) return Boolean
   is
   begin
      return Self.m_hasAnisotropicFriction;
   end hasAnisotropicFriction;



   procedure setContactProcessingThreshold (Self : in out Item;   To : in  math.Real)
   is
   begin
      Self.m_contactProcessingThreshold := To;
   end setContactProcessingThreshold;





   function  getContactProcessingThreshold (Self : in     Item)     return math.Real
   is
   begin
      return Self.m_contactProcessingThreshold;
   end getContactProcessingThreshold;




   function  isStaticObject (Self : in Item) return Boolean
   is
   begin
      return (Self.m_collisionFlags and CF_STATIC_OBJECT) /= 0;
   end isStaticObject;




   function  isKinematicObject (Self : in Item) return Boolean
   is
   begin
      return (Self.m_collisionFlags and CF_KINEMATIC_OBJECT) /= 0;
   end isKinematicObject;



   function  isStaticOrKinematicObject (Self : in Item) return Boolean
   is
   begin
      return (Self.m_collisionFlags and (CF_KINEMATIC_OBJECT or CF_STATIC_OBJECT)) /= 0;
   end isStaticOrKinematicObject;




   function  hasContactResponse (Self : in Item) return Boolean
      is
   begin
      return (Self.m_collisionFlags and CF_NO_CONTACT_RESPONSE) = 0;
   end hasContactResponse;







   procedure setCollisionShape (Self : in out Item;   To : access impact.d3.Shape.item'Class)
    is
   begin
      Self.m_collisionShape     := To;
      Self.m_rootCollisionShape := To;
   end setCollisionShape;





   function  getRootCollisionShape (Self : in     Item) return access impact.d3.Shape.item'Class
   is
   begin
      return Self.m_rootCollisionShape;
   end getRootCollisionShape;




   procedure internalSetTemporaryCollisionShape (Self : in out Item;   To : access impact.d3.Shape.item'Class)
   is
   begin
      Self.m_collisionShape := To;
   end internalSetTemporaryCollisionShape;



   function  internalGetExtensionPointer (Self : in Item) return access Any'Class
   is
   begin
      return Self.m_extensionPointer;
   end internalGetExtensionPointer;



   procedure internalSetExtensionPointer (Self : in out Item;   To : access Any'Class)
   is
   begin
      Self.m_extensionPointer := To;
   end internalSetExtensionPointer;




   function  getActivationState (Self : in     Item) return Integer
      is
   begin
      return Self.m_activationState1;
   end getActivationState;




--          void setActivationState(int newState);
--




   procedure setDeactivationTime (Self : in out Item;   To : in math.Real)
      is
   begin
      Self.m_deactivationTime := To;
   end setDeactivationTime;




   function  getDeactivationTime (Self : in     Item)    return math.Real
   is
   begin
      return Self.m_deactivationTime;
   end getDeactivationTime;






   procedure setActivationState (Self : in out Item;   To : in Integer)
   is
   begin
--        if To = 3 then
--           put_Line ("H");
--        end if;

--        put_Line ("new activation state => " & Integer'Image (To));

      if         (Self.m_activationState1 /= DISABLE_DEACTIVATION)
        and then (Self.m_activationState1 /= DISABLE_SIMULATION)
      then
         Self.m_activationState1 := To;
      end if;
   end setActivationState;








   procedure forceActivationState (Self : in out Item;   To              : in Integer)
   is
   begin
      Self.m_activationState1 := To;
   end forceActivationState;





   procedure activate (Self : in out Item;   forceActivation : in Boolean := False)
    is
   begin
      if        forceActivation
        or else ((  Self.m_collisionFlags
                  and (CF_STATIC_OBJECT or CF_KINEMATIC_OBJECT)) = 0)
      then
         Self.setActivationState (ACTIVE_TAG);
         Self.m_deactivationTime := 0.0;
      end if;
   end activate;




   function  isActive (Self : in Item) return Boolean
   is
   begin
      return ((Self.getActivationState /= ISLAND_SLEEPING) and then (Self.getActivationState /= DISABLE_SIMULATION));
   end isActive;




   procedure setRestitution (Self : in out Item;   To : in math.Real)
      is
   begin
      Self.m_restitution := To;
   end setRestitution;




   procedure setFriction (Self : in out Item;   To : in math.Real)
      is
   begin
      Self.m_friction := To;
   end setFriction;



   function  getInternalType (Self : in     Item) return Flags
   is
   begin
      return Self.m_internalType;
   end getInternalType;




   procedure setInternalType (Self :    out Item;   To : in Flags)
   is
   begin
      Self.m_internalType := To;
   end setInternalType;




   procedure setWorldTransform (Self : in out Item;   To : in Transform_3d)
   is
   begin
      Self.m_worldTransform := To;
   end setWorldTransform;













   function  getInterpolationWorldTransform (Self : access Item)    return access Transform_3d
      is
   begin
      return Self.m_interpolationWorldTransform'Access;
   end getInterpolationWorldTransform;



   function  getInterpolationWorldTransform (Self : in     Item)    return Transform_3d
      is
   begin
      return Self.m_interpolationWorldTransform;
   end getInterpolationWorldTransform;





   procedure setInterpolationWorldTransform (Self : in out Item;   To : in Transform_3d)
   is
   begin
      Self.m_interpolationWorldTransform := To;
   end setInterpolationWorldTransform;




   procedure setInterpolationLinearVelocity (Self : in out Item;   To : in math.Vector_3)
   is
   begin
      Self.m_interpolationLinearVelocity := To;
   end setInterpolationLinearVelocity;





   procedure setInterpolationAngularVelocity (Self : in out Item;   To : in math.Vector_3)
   is
   begin
      Self.m_interpolationAngularVelocity := To;
   end setInterpolationAngularVelocity;




   function  getInterpolationLinearVelocity (Self : in     Item)    return math.Vector_3
   is
   begin
      return Self.m_interpolationLinearVelocity;
   end getInterpolationLinearVelocity;





   function  getInterpolationAngularVelocity (Self : in     Item)    return math.Vector_3
      is
   begin
      return Self.m_interpolationAngularVelocity;
   end getInterpolationAngularVelocity;




   function  getIslandTag (Self : in     Item)    return Integer
      is
   begin
      return Self.m_islandTag1;
   end getIslandTag;




   procedure setIslandTag (Self : in out Item;   To : in Integer)
   is
   begin
      Self.m_islandTag1 := To;
   end setIslandTag;




   function  getCompanionId (Self : in     Item)    return Integer
      is
   begin
      return Self.m_companionId;
   end getCompanionId;




   procedure setCompanionId (Self : in out Item;   To : in Integer)
   is
   begin
      Self.m_companionId := To;
   end setCompanionId;






   function  getHitFraction (Self : in     Item)    return math.Real
      is
   begin
      return Self.m_hitFraction;
   end getHitFraction;



   procedure setHitFraction (Self : in out Item;   To : in math.Real)
   is
   begin
      Self.m_hitFraction := To;
   end setHitFraction;



   procedure setCollisionFlags (Self : in out Item;   To : in Flags)
   is
   begin
      Self.m_collisionFlags := To;
   end setCollisionFlags;



   function  getCcdSweptSphereRadius (Self : in     Item)    return math.Real
      is
   begin
      return Self.m_ccdSweptSphereRadius;
   end getCcdSweptSphereRadius;



   procedure setCcdSweptSphereRadius (Self : in out Item;   To : in math.Real)
   is
   begin
      Self.m_ccdSweptSphereRadius := To;
   end setCcdSweptSphereRadius;





   function  getCcdMotionThreshold (Self : in     Item)    return math.Real
      is
   begin
      return Self.m_ccdMotionThreshold;
   end getCcdMotionThreshold;



   function  getCcdSquareMotionThreshold (Self : in Item) return math.Real
      is
   begin
      return Self.m_ccdMotionThreshold * Self.m_ccdMotionThreshold;
   end getCcdSquareMotionThreshold;





   procedure setCcdMotionThreshold (Self : in out Item;   To : in math.Real)
   is
   begin
      Self.m_ccdMotionThreshold := To;
   end setCcdMotionThreshold;




   function  getUserPointer (Self : in     Item) return access Any'Class
   is
   begin
      return Self.m_userObjectPointer;
   end getUserPointer;





   procedure setUserPointer (Self : in out Item;   To : access Any'Class)
   is
   begin
      Self.m_userObjectPointer := To;
   end setUserPointer;




   function  checkCollideWith (Self : in     Item;   co : access Item'Class) return Boolean
   is
   begin
      if Self.m_checkCollideWith then
         return Self.checkCollideWithOverride (co);
      end if;

      return True;
   end checkCollideWith;




   procedure setCheckCollideWith (Self : in out Item;   To : in Boolean)
   is
   begin
      Self.m_checkCollideWith := To;
   end setCheckCollideWith;



end impact.d3.Object;
