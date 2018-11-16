with impact.d3.Shape,
     impact.d3.collision.Proxy,

     ada.Containers.Vectors;
--  with any_Math;


package impact.d3.Object
--
--  Item can be used to manage collision detection objects.
--
--  Item maintains all information that is needed for a collision detection: Shape, Transform and AABB proxy.
--  They can be added to the impact.d3.Space.
--
is
   use Math;


   type Item is new Any with private;
   type View is access all Item'Class;




   --- Containers
   --

   package Vectors is new ada.Containers.Vectors (Positive, View);
   subtype Vector  is     Vectors.Vector;





   --  island management, m_activationState1
   --
   ACTIVE_TAG           : constant := 1;
   ISLAND_SLEEPING      : constant := 2;
   WANTS_DEACTIVATION   : constant := 3;
   DISABLE_DEACTIVATION : constant := 4;
   DISABLE_SIMULATION   : constant := 5;





   --  CollisionFlags
   --

   CF_STATIC_OBJECT                    : constant Flags :=  1;
   CF_KINEMATIC_OBJECT                 : constant Flags :=  2;
   CF_NO_CONTACT_RESPONSE              : constant Flags :=  4;
   CF_CUSTOM_MATERIAL_CALLBACK         : constant Flags :=  8;   -- this allows per-triangle material (friction/restitution)
   CF_CHARACTER_OBJECT                 : constant Flags := 16;
   CF_DISABLE_VISUALIZE_OBJECT         : constant Flags := 32;   -- disable debug drawing
   CF_DISABLE_SPU_COLLISION_PROCESSING : constant Flags := 64;   -- disable parallel/SPU processing



   --  CollisionObjectTypes
   --

   CO_COLLISION_OBJECT : constant Flags :=  1;
   CO_RIGID_BODY       : constant Flags :=  2;
   CO_GHOST_OBJECT     : constant Flags :=  4;  -- CO_GHOST_OBJECT keeps track of all objects overlapping its AABB and that pass its collision filter.
                                                --  It is useful for collision sensors, explosion objects, character controller etc.
   CO_SOFT_BODY        : constant Flags :=  8;
   CO_HF_FLUID         : constant Flags := 16;
   CO_USER_TYPE        : constant Flags := 32;




   type Object_Array is array (Positive range <>) of access Item'Class;




   procedure define   (Self : in out Item);
   procedure destruct (Self : in out Item);






   function  WorldTransform    (Self : in     Item) return        Transform_3d;
   function  getWorldTransform (Self : access Item) return access Transform_3d;


   function  getFriction       (Self : in Item) return math.Real;
   function  getRestitution    (Self : in Item) return math.Real;
   function  getCollisionFlags (Self : in Item) return Flags;


   function  getCollisionShape (Self : in Item) return access impact.d3.Shape.item'Class;



   function  getBroadphaseHandle (Self : access Item) return access impact.d3.collision.Proxy.item'Class;
   procedure setBroadphaseHandle (Self : in out Item;  To : access impact.d3.collision.Proxy.item'Class);



   function  mergesSimulationIslands (Self : in Item) return Boolean;


   function  getAnisotropicFriction (Self : in     Item)     return math.Vector_3;
   procedure setAnisotropicFriction (Self : in out Item;   To : in  math.Vector_3);
   function  hasAnisotropicFriction (Self : in Item) return Boolean;




   procedure setContactProcessingThreshold (Self : in out Item;   To : in  math.Real);
   --
   --  The constraint solver can discard solving contacts, if the distance is above this threshold. 0 by default.
   --  Note that using contacts with positive distance can improve stability. It increases, however, the chance of colliding with degerate contacts, such as 'interior' triangle edges




   function  getContactProcessingThreshold (Self : in     Item)     return math.Real;



   function  isStaticObject            (Self : in Item) return Boolean;
   function  isKinematicObject         (Self : in Item) return Boolean;
   function  isStaticOrKinematicObject (Self : in Item) return Boolean;
   function  hasContactResponse        (Self : in Item) return Boolean;





   procedure setCollisionShape     (Self : in out Item;   To : access impact.d3.Shape.item'Class);
   function  getRootCollisionShape (Self : in     Item) return access impact.d3.Shape.item'Class;


   procedure internalSetTemporaryCollisionShape (Self : in out Item;   To : access impact.d3.Shape.item'Class);
   --
   --  Avoid using this internal API call.
   --  'internalSetTemporaryCollisionShape' is used to temporary replace the actual collision shape by a child collision shape.




   function  internalGetExtensionPointer (Self : in Item) return access Any'Class;
   --
   --  Avoid using this internal API call, the extension pointer is used by some Bullet extensions.
   --  If you need to store your own user pointer, use 'setUserPointer/getUserPointer' instead.


   procedure internalSetExtensionPointer (Self : in out Item;   To : access Any'Class);
   --
   --  Avoid using this internal API call, the extension pointer is used by some Bullet extensions.
   --  If you need to store your own user pointer, use 'setUserPointer/getUserPointer' instead.







   function  getActivationState (Self : in     Item) return Integer;
   procedure setActivationState (Self : in out Item;   To : in Integer);

   procedure forceActivationState (Self : in out Item;   To              : in Integer);
   procedure activate             (Self : in out Item;   forceActivation : in Boolean := False);

   function  isActive (Self : in Item) return Boolean;


   procedure setDeactivationTime (Self : in out Item;   To : in math.Real);
   function  getDeactivationTime (Self : in     Item)    return math.Real;




   procedure setRestitution (Self : in out Item;   To : in math.Real);
   procedure setFriction    (Self : in out Item;   To : in math.Real);




   procedure setInternalType (Self :    out Item;   To : in Flags);
   function  getInternalType (Self : in     Item) return Flags;
   --
   --  reserved for Bullet internal usage




   procedure setWorldTransform (Self : in out Item;   To : in Transform_3d);


   procedure setInterpolationWorldTransform  (Self : in out Item;   To : in Transform_3d);
   function  getInterpolationWorldTransform  (Self : in     Item)    return Transform_3d;
   function  getInterpolationWorldTransform  (Self : access Item)    return access Transform_3d;

   procedure setInterpolationLinearVelocity  (Self : in out Item;   To : in math.Vector_3);
   function  getInterpolationLinearVelocity  (Self : in     Item)    return math.Vector_3;

   procedure setInterpolationAngularVelocity (Self : in out Item;   To : in math.Vector_3);
   function  getInterpolationAngularVelocity (Self : in     Item)    return math.Vector_3;



   function  getIslandTag   (Self : in     Item)    return Integer;
   procedure setIslandTag   (Self : in out Item;   To : in Integer);

   function  getCompanionId (Self : in     Item)    return Integer;
   procedure setCompanionId (Self : in out Item;   To : in Integer);

   function  getHitFraction (Self : in     Item)    return math.Real;
   procedure setHitFraction (Self : in out Item;   To : in math.Real);


   procedure setCollisionFlags (Self : in out Item;   To : in Flags);


   function  getCcdSweptSphereRadius (Self : in     Item)    return math.Real;   -- Swept sphere radius (0.0 by default), see impact.d3.collision.Algorithm.activating.convex_convex::
   procedure setCcdSweptSphereRadius (Self : in out Item;   To : in math.Real);  -- Swept sphere radius (0.0 by default), see impact.d3.collision.Algorithm.activating.convex_convex::


   function  getCcdMotionThreshold (Self : in     Item)    return math.Real;
   procedure setCcdMotionThreshold (Self : in out Item;   To : in math.Real);    -- Don't do continuous collision detection if the motion (in one step) is less then m_ccdMotionThreshold


   function  getCcdSquareMotionThreshold (Self : in Item) return math.Real;

   function  getUserPointer (Self : in     Item) return access Any'Class;        -- Users can point to their objects, userPointer is not used by Bullet
   procedure setUserPointer (Self : in out Item;   To : access Any'Class);       --




   function  checkCollideWith    (Self : in     Item;   co : access Item'Class) return Boolean;
   procedure setCheckCollideWith (Self : in out Item;   To : in Boolean);




private


   type Item is new Any with
      record
         m_worldTransform               : aliased Transform_3d;

         --  'm_interpolationWorldTransform' is used for CCD and interpolation
         --  it can be either previous or future (predicted) transform
         --
         m_interpolationWorldTransform  : aliased Transform_3d;

         --  These two are experimental: just added for bullet time effect, so you can still apply impulses (directly modifying velocities)
         --  without destroying the continuous interpolated motion (which uses this interpolation velocities)
         --
         m_interpolationLinearVelocity  : math.Vector_3;
         m_interpolationAngularVelocity : math.Vector_3;

         m_anisotropicFriction          : Math.Vector_3;
         m_hasAnisotropicFriction       : Boolean;
         m_contactProcessingThreshold   : math.Real;

         m_broadphaseHandle     : access impact.d3.collision.Proxy.item'Class;
         m_collisionShape       : access impact.d3.Shape.item'Class;

         --  'm_extensionPointer' is used by some internal low-level Bullet extensions.
         --
         m_extensionPointer     : access Any'Class;

         --  'm_rootCollisionShape' is temporarily used to store the original collision shape
         --  The m_collisionShape might be temporarily replaced by a child collision shape during collision detection purposes
         --  If it is NULL, the m_collisionShape is not temporarily replaced.
         --
         m_rootCollisionShape   : access impact.d3.Shape.item'Class;

         m_collisionFlags       : Flags;

         m_islandTag1           : Integer;
         m_companionId          : Integer;

         m_activationState1     : Integer;
         m_deactivationTime     : math.Real;

         m_friction             : math.Real;
         m_restitution          : math.Real;

         --  m_internalType is reserved to distinguish Bullet's Item, Item.rigid, btSoftBody, btGhostObject etc.
         --  do not assign your own m_internalType unless you write a new dynamics object class.
         --
         m_internalType         : Flags;

         --  users can point to their objects, m_userPointer is not used by Bullet, see setUserPointer/getUserPointer
         --
         m_userObjectPointer    : access Any'Class;

         --  time of impact calculation
         --
         m_hitFraction          : math.Real;

         --  Swept sphere radius (0.0 by default), see impact.d3.collision.Algorithm.activating.convex_convex::
         --
         m_ccdSweptSphereRadius : math.Real;

         --  Don't do continuous collision detection if the motion (in one step) is less then m_ccdMotionThreshold
         --
         m_ccdMotionThreshold   : math.Real;

         --  If some object should have elaborate collision filtering by sub-classes
         --
         m_checkCollideWith     : Boolean;

      end record;




   function checkCollideWithOverride (Self : in Item;   co : access Item'Class) return Boolean;




end impact.d3.Object;
