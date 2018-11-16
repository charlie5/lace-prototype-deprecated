with impact.d3.collision.Proxy;



package impact.d3.Shape
--
--  The impact.d3.Shape class provides an interface for collision shapes that can be shared among impact.d3.Objects.
--
is
   use Math;



   type Item is abstract tagged private;
   type View is access all Item'Class;



--     type unsigned_char_Pointer  is access all interfaces.c.unsigned_char;
--     type unsigned_char_Pointers is array (Positive range <>) of unsigned_char_Pointer;




   ---------------------
   --- impact.d3.Shape
   --

   procedure destruct (Self : in out Item)   is null;


   function  getShapeType (Self : in     Item)    return impact.d3.collision.Proxy.BroadphaseNativeTypes;
   procedure setShapeType (Self : in out Item;   To : in impact.d3.collision.Proxy.BroadphaseNativeTypes);



   procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                                        aabbMin, aabbMax :    out math.Vector_3)       is abstract;
   --
   --  getAabb returns the axis aligned bounding box in the coordinate frame of the given transform t.



   procedure getBoundingSphere (Self : in Item'Class;   center : out math.Vector_3;
                                radius : out math.Real);


   function getAngularMotionDisc (Self : in Item) return math.Real;
   --
   --  getAngularMotionDisc returns the maximus radius needed for Conservative Advancement to handle time-of-impact with rotations.


   function getContactBreakingThreshold (Self : in Item;   defaultContactThresholdFactor : in math.Real) return math.Real;


   procedure calculateTemporalAabb (Self : in Item'Class;   curTrans        : in     Transform_3d;
                                                            linvel          : in     math.Vector_3;
                                                            angvel          : in     math.Vector_3;
                                                            timeStep        : in     math.Real;
                                                            temporalAabbMin,
                                                            temporalAabbMax :    out math.Vector_3);
   --
   --  calculateTemporalAabb calculates the enclosing aabb for the moving object over interval [0..timeStep)
   --  result is conservative




   function isConvex     (Self : in Item'Class) return Boolean;
   function isPolyhedral (Self : in Item'Class) return Boolean;
   function isConvex2d   (Self : in Item'Class) return Boolean;
   function isNonMoving  (Self : in Item'Class) return Boolean;
   function isConcave    (Self : in Item'Class) return Boolean;
   function isCompound   (Self : in Item'Class) return Boolean;
   function isSoftBody   (Self : in Item'Class) return Boolean;
   function isInfinite   (Self : in Item'Class) return Boolean;   -- used to catch simulation error (aabb check)



   procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3)   is abstract;
   function  getLocalScaling (Self : in     Item)         return math.Vector_3    is abstract;


   procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3)   is abstract;

   function  getName   (Self : in     Item)        return String       is abstract;    -- for debug

   procedure setMargin (Self : in out Item;   margin : in math.Real)   is abstract;
   function  getMargin (Self : in     Item)        return math.Real    is abstract;


   procedure setUserPointer (Self : in out Item'Class;   userPtr : access Any'Class);   -- optional user data pointer
   function  getUserPointer (Self : in     Item'Class)      return access Any'Class;    --









private

   type Item is abstract tagged
      record
         m_shapeType   : impact.d3.collision.Proxy.BroadphaseNativeTypes := impact.d3.collision.Proxy.INVALID_SHAPE_PROXYTYPE;
         m_userPointer : access Any'Class;
      end record;



   procedure btBulletCollisionProbe;


end impact.d3.Shape;
