with ada.Containers.Vectors;
with impact.d3.solver_Constraint;
with impact.d3.Containers;
with impact.d3.Scalar;
with Interfaces.C;

limited
with impact.d3.Object.rigid;




package impact.d3.Joint
--
--  TypedConstraint is the baseclass for Bullet constraints and vehicles.
--
is

   type Item is abstract new impact.d3.Scalar.btTypedObject with private;
   type View is access all Item'Class;





--     package Real_pointers is new interfaces.C.pointers (Natural, math.Real, Bullet.real_Array, math.Real'Last);
--     subtype Real_pointer  is     Real_pointers.Pointer;







   --  Don't change any of the existing enum values, so add enum types at the end for serialization compatibility.
   --

   type Kind is (POINT2POINT_CONSTRAINT_TYPE, -- =3,
                 HINGE_CONSTRAINT_TYPE,
                 CONETWIST_CONSTRAINT_TYPE,
                 D6_CONSTRAINT_TYPE,
                 SLIDER_CONSTRAINT_TYPE,
                 CONTACT_CONSTRAINT_TYPE,
                 D6_SPRING_CONSTRAINT_TYPE,
                 MAX_CONSTRAINT_TYPE);


   type btConstraintParams is (BT_CONSTRAINT_ERP,               -- =1,
                               BT_CONSTRAINT_STOP_ERP,
                               BT_CONSTRAINT_CFM,
                               BT_CONSTRAINT_STOP_CFM);







   --- btConstraintInfo1
   --

   type btConstraintInfo1 is
      record
         m_numConstraintRows,
         nub                : Integer;
      end record;

   type btConstraintInfo1_view is access all btConstraintInfo1;


   package btConstraintInfo1_Vectors is new ada.Containers.Vectors (Positive, btConstraintInfo1_view);
   subtype btConstraintInfo1_Vector  is     btConstraintInfo1_Vectors.Vector;








   --- btConstraintInfo2
   --

   type btConstraintInfo2 is
      record
         --  integrator parameters: frames per second (1/stepsize), default error
         --  reduction parameter (0..1).
         fps,
         erp              : math.Real;


         --  for the first and second body, pointers to two (linear and angular)
         --  n*3 jacobian sub matrices, stored by rows. these matrices will have
         --  been initialized to 0 on entry. if the second body is zero then the
         --  J2xx pointers may be 0.
         --
         m_J1linearAxis,
         m_J1angularAxis,
         m_J2linearAxis,
         m_J2angularAxis  : impact.d3.Containers.Real_pointer;   -- nb : zero-based !


         --  elements to jump from one row to the next in J's
         rowskip          : interfaces.c.ptrdiff_t;

         --  right hand sides of the equation J*v = c + cfm * lambda. cfm is the
         --  "constraint force mixing" vector. c is set to zero on entry, cfm is
         --  set to a constant value (typically very small or zero) value on entry.
         m_constraintError : impact.d3.Containers.Real_pointer;
         cfm              : access math.Real;

         --  lo and hi limits for variables (set to -/+ infinity on entry).
         m_lowerLimit,
         m_upperLimit     : access math.Real;

         --  findex vector for variables. see the LCP solver interface for a
         --  description of what this does. this is set to -1 on entry.
         --  note that the returned indexes are relative to the first index of
         --  the constraint.
         findex           : access Integer;

         m_numIterations  : Integer;         -- number of solver iterations
         m_damping        : math.Real;       -- damping of the velocity
      end record;





   --- Forge
   --

   procedure define   (Self : in out Item'Class;   of_type : in     Kind;
                                                   rbA     : access impact.d3.Object.rigid.Item'Class);

   procedure define   (Self : in out Item'Class;   of_type : in     Kind;
                                                   rbA     : access impact.d3.Object.rigid.Item'Class;
                                                   rbB     : access impact.d3.Object.rigid.Item'Class);

   procedure destruct (Self : in out Item) is null;




   --- Containers
   --

   package Vectors is new ada.Containers.Vectors (Positive, View);
   subtype Vector  is     Vectors.Vector;





   --- Attributes
   --

   function  getBreakingImpulseThreshold (Self : in     Item)           return math.Real;
   procedure setBreakingImpulseThreshold (Self :    out Item;   threshold : in math.Real);


   function  isEnabled  (Self : in     Item)         return Boolean;
   procedure setEnabled (Self :    out Item;   enabled : in Boolean);


   function  getRigidBodyA (Self : in Item) return access impact.d3.Object.rigid.item'Class;
   function  getRigidBodyB (Self : in Item) return access impact.d3.Object.rigid.item'Class;


   function  getUserConstraintType (Self : in Item) return Integer;
   procedure setUserConstraintType (Self : out Item;   userConstraintType : in Integer);


   procedure setUserConstraintId (Self : out Item;   uid : in Integer);
   function  getUserConstraintId (Self : in Item) return Integer;


   procedure setUserConstraintPtr (Self : out Item;   ptr : access Any'Class);
   function  getUserConstraintPtr (Self : in Item) return access Any'Class;


   function  getUid         (Self : in     Item)               return Integer;
   function  needsFeedback  (Self : in     Item)               return Boolean;
   procedure enableFeedback (Self :    out Item;   needsFeedback : in Boolean);
   --
   --  enableFeedback will allow to read the applied linear and angular impulse.
   --  Use getAppliedImpulse, getAppliedLinearImpulse and getAppliedAngularImpulse to read feedback information



   function  getAppliedImpulse (Self : in Item) return math.Real;
   --
   --  getAppliedImpulse is an estimated total applied impulse.
   --  This feedback could be used to determine breaking constraints or playing sounds.



   function  getConstraintType (Self : in Item) return Kind;




   procedure setParam       (Self : out Item;    num   : in btConstraintParams;
                                                 value : in math.Real;
                                                 axis  : in Integer := -1)   is abstract;
   --
   --  override the default global value of a parameter (such as ERP or CFM), optionally provide the axis (0..5).
   --  If no axis is provided, it uses the default axis for this constraint.


   function  getParam (Self : in Item;   num  : in btConstraintParams;
                                         axis : in Integer := -1) return math.Real   is abstract;
   --
   --  Returns the local value of parameter.




   function  btAdjustAngleToLimits (angleInRadians,
                                    angleLowerLimitInRadians,
                                    angleUpperLimitInRadians : in math.Real) return math.Real;
   --
   --  Returns angle in range [-SIMD_2_PI, SIMD_2_PI], closest to one of the limits.
   --  All arguments should be normalized angles (i.e. in range [-SIMD_PI, SIMD_PI]).








   --- btAngularLimit
   --

   type btAngularLimit is tagged private;



   procedure set (Self : out btAngularLimit;   low, high        : in math.Real;
                                               softness         : in math.Real := 0.9;
                                               biasFactor       : in math.Real := 0.3;
                                               relaxationFactor : in math.Real := 1.0);
   --
   --  Sets all limit's parameters.
   --
   --  When 'low        > high' limit becomes inactive.
   --  When 'high - low > 2PI'  limit is ineffective too becouse no angle can exceed the limit.



   procedure test (Self : in out btAngularLimit;   angle : in math.Real);
   --
   --  Checks conastaint angle against limit. If limit is active and the angle violates the limit
   --  correction is calculated.



   function  getSoftness (Self : in btAngularLimit) return math.Real;
   --
   --  Returns limit's softness.



   function  getBiasFactor (Self : in btAngularLimit) return math.Real;
   --
   --  Returns limit's bias factor.



   function  getRelaxationFactor (Self : in btAngularLimit) return math.Real;
   --
   --  Returns limit's relaxation factor.



   function  getCorrection (Self : in btAngularLimit) return math.Real;
   --
   --  Returns correction value evaluated when test() was invoked.



   function  getSign (Self : in btAngularLimit) return math.Real;
   --
   --  Returns sign value evaluated when test() was invoked.



   function  getHalfRange (Self : in btAngularLimit) return math.Real;
   --
   --  Gives half of the distance between min and max limit angle.



   function  isLimit (Self : in btAngularLimit) return Boolean;
   --
   --  Returns true when the last test() invocation recognized limit violation.



   procedure fit (Self : in btAngularLimit;   angle : in out math.Real);
   --
   --  Checks given angle against limit. If limit is active and angle doesn't fit it, the angle
   --  returned is modified so it equals to the limit closest to given angle.




   function  getError (Self : in btAngularLimit) return math.Real;
   --
   --  Returns correction value multiplied by sign value.



   function  getLow  (Self : in btAngularLimit) return math.Real;
   function  getHigh (Self : in btAngularLimit) return math.Real;







   --- Internal methods used by the constraint solver, don't use them directly !
   --

   procedure solveConstraintObsolete (Self : in out Item;   bodyA, bodyB : access impact.d3.Object.rigid.item'Class;
                                                            timeStep     : in     math.Real           )   is null;

   procedure buildJacobian           (Self : in out Item)                                                   is null;


   procedure setupSolverConstraint   (Self : in out Item;   ca                       : in out impact.d3.solver_Constraint.btConstraintArray;
                                                            solverBodyA, solverBodyB : in     Integer;
                                                            timeStep                 : in     math.Real)    is null;


   procedure getInfo1 (Self : in out Item;   info : out btConstraintInfo1)   is abstract;
   procedure getInfo2 (Self : in out Item;   info : out btConstraintInfo2)   is abstract;


   procedure internalSetAppliedImpulse (Self :    out Item;   appliedImpulse : in math.Real);
   function  internalGetAppliedImpulse (Self : in     Item)                return math.Real;




   function getMotorFactor (Self : in Item;   pos,
                                              lowLim, uppLim,
                                              vel,
                                              timeFact      : in math.Real) return math.Real;
   --
   --  Internal method used by the constraint solver, don't use them directly.



   function getFixedBody return access impact.d3.Object.rigid.Item'Class;



private



   type union_Kind is (userConstraintId, userConstraintPtr);

   type union (Kind : union_Kind := userConstraintId) is
      record
         case Kind is
            when userConstraintId  =>   m_userConstraintId  : Integer;
            when userConstraintPtr =>   m_userConstraintPtr : access Any'Class;
         end case;
      end record;



   type Item is abstract new impact.d3.Scalar.btTypedObject with
      record
         m_userConstraintType       : Integer;

         union                      : impact.d3.Joint.union;

         m_breakingImpulseThreshold : math.Real;
         m_isEnabled                : Boolean;

         m_needsFeedback            : Boolean;


         m_rbA            : access impact.d3.Object.rigid.Item'Class;
         m_rbB            : access impact.d3.Object.rigid.Item'Class;

         m_appliedImpulse : math.Real;
      end record;













   for Kind use (POINT2POINT_CONSTRAINT_TYPE =>  3,
                                  HINGE_CONSTRAINT_TYPE       =>  4,
                                  CONETWIST_CONSTRAINT_TYPE   =>  5,
                                  D6_CONSTRAINT_TYPE          =>  6,
                                  SLIDER_CONSTRAINT_TYPE      =>  7,
                                  CONTACT_CONSTRAINT_TYPE     =>  8,
                                  D6_SPRING_CONSTRAINT_TYPE   =>  9,
                                  MAX_CONSTRAINT_TYPE         => 10);


   for btConstraintParams use (BT_CONSTRAINT_ERP      => 1,
                               BT_CONSTRAINT_STOP_ERP => 2,
                               BT_CONSTRAINT_CFM      => 3,
                               BT_CONSTRAINT_STOP_CFM => 4);





   --- btAngularLimit
   --

   type btAngularLimit is tagged
      record
         m_center           : math.Real :=  0.0;
         m_halfRange        : math.Real := -1.0;
         m_softness         : math.Real :=  0.9;
         m_biasFactor       : math.Real :=  0.3;
         m_relaxationFactor : math.Real :=  1.0;
         m_correction       : math.Real :=  0.0;
         m_sign             : math.Real :=  0.0;

         m_solveLimit       : Boolean   := False;
      end record;
   --
   --  Default initialisation limit as inactive, allowing free constraint movement.



end impact.d3.Joint;
