--  #include "LinearMath/impact.d3.Vector.h"
--  #include "impact.d3.jacobian_Entry.h"
--  #include "impact.d3.Joint.h"
--
--  class impact.d3.Object.rigid;


with
     impact.d3.Object.rigid;
with impact.d3.jacobian_Entry;



package impact.d3.Joint.any
  --
  --  impact.d3.Joint.any between two rigidbodies each with a pivotpoint that descibes the axis location in local space
  --
  --  impact.d3.Joint.any can leave any of the 6 degree of freedom 'free' or 'locked'.
  --  currently this limit supports rotational motors<br>
  --
  --  For Linear limits, use impact.d3.Joint.any.setLinearUpperLimit, impact.d3.Joint.any.setLinearLowerLimit. You can
  --  set the parameters with the btTranslationalLimitMotor structure accsesible through the impact.d3.Joint.any.getTranslationalLimitMotor method.
  --  At this moment translational motors are not supported. May be in the future.
  --
  --  For Angular limits, use the btRotationalLimitMotor structure for configuring the limit.
  --  This is accessible through impact.d3.Joint.any.getLimitMotor method,
  --  This brings support for limit parameters and motors.
  --
  --  Angulars limits have these possible ranges:
  --
  --
  --          AXIS
  --             MIN ANGLE
  --             MAX ANGLE
  --
  --          X
  --             -PI
  --              PI
  --
  --          Y
  --             -PI / 2
  --              PI / 2
  --
  --          Z
  --             -PI
  --              PI
  --
is
   use Math;


   type Axes is array (1 .. 3) of Vector_3;



   --  bt6DofFlags
   --
   BT_6DOF_FLAGS_CFM_NORM : Flags := 1;
   BT_6DOF_FLAGS_CFM_STOP : Flags := 2;
   BT_6DOF_FLAGS_ERP_STOP : Flags := 4;


   BT_6DOF_FLAGS_AXIS_SHIFT : constant := 3;    -- bits per axis





   type Item is new impact.d3.Joint.Item with private;   -- tbd: bullet makes this class 'limited' (ie provides a private, aborting '=' operator).




   ---------------------------
   --- btRotationalLimitMotor
   --
   --  Rotation Limit structure for generic joints.

   type btRotationLimitMotor is tagged
      record
         m_loLimit            : Real;   -- joint limit
         m_hiLimit            : Real;   -- joint limit
         m_targetVelocity     : Real;   -- target motor velocity
         m_maxMotorForce      : Real;   -- max force on motor
         m_maxLimitForce      : Real;   -- max force on limit
         m_damping            : Real;   -- Damping
         m_limitSoftness      : Real;   -- Relaxation factor
         m_normalCFM          : Real;   -- Constraint force mixing factor
         m_stopERP            : Real;   -- Error tolerance factor when joint is at limit
         m_stopCFM            : Real;   -- Constraint force mixing factor when joint is at limit
         m_bounce             : Real;   -- restitution factor
         m_enableMotor        : Boolean;

         --  temporary variables
         m_currentLimitError  : Real;   -- How much is violated this limit
         m_currentPosition    : Real;   -- current value of angle
         m_currentLimit       : Integer;  -- 0=free, 1=at lo limit, 2=at hi limit
         m_accumulatedImpulse : Real;
      end record;


   function to_btRotationLimitMotor                                   return btRotationLimitMotor;
   function to_btRotationLimitMotor (Other : in btRotationLimitMotor) return btRotationLimitMotor;


   function isLimited        (Self : in     btRotationLimitMotor) return Boolean;
   function needApplyTorques (Self : in     btRotationLimitMotor) return Boolean;


   function testLimitValue   (Self : access btRotationLimitMotor;   test_value : in     Real) return Integer;
   --
   --  Calculates error (ie 'm_currentLimit' and 'm_currentLimitError').



   function solveAngularLimits (Self : access btRotationLimitMotor;   timeStep     : in     math.Real;
                                                                      axis         : access math.Vector_3;
                                                                      jacDiagABInv : in     math.Real;
                                body0, body1 : in     impact.d3.Object.rigid.view) return math.Real;
   --
   --  Apply the correction impulses for two bodies.




--  class btRotationalLimitMotor
--  {
--  public:
--      //! limit_parameters
--      //!@{
--      impact.d3.Scalar m_loLimit;//!< joint limit
--      impact.d3.Scalar m_hiLimit;//!< joint limit
--      impact.d3.Scalar m_targetVelocity;//!< target motor velocity
--      impact.d3.Scalar m_maxMotorForce;//!< max force on motor
--      impact.d3.Scalar m_maxLimitForce;//!< max force on limit
--      impact.d3.Scalar m_damping;//!< Damping.
--      impact.d3.Scalar m_limitSoftness;//! Relaxation factor
--      impact.d3.Scalar m_normalCFM;//!< Constraint force mixing factor
--      impact.d3.Scalar m_stopERP;//!< Error tolerance factor when joint is at limit
--      impact.d3.Scalar m_stopCFM;//!< Constraint force mixing factor when joint is at limit
--      impact.d3.Scalar m_bounce;//!< restitution factor
--      bool m_enableMotor;
--
--      //!@}
--
--      //! temp_variables
--      //!@{
--      impact.d3.Scalar m_currentLimitError;//!  How much is violated this limit
--      impact.d3.Scalar m_currentPosition;     //!  current value of angle
--      int m_currentLimit;//!< 0=free, 1=at lo limit, 2=at hi limit
--      impact.d3.Scalar m_accumulatedImpulse;
--      //!@}
--  };





   type Angular_Limits is array (1 .. 3) of aliased btRotationLimitMotor;
   type Enable_Motor   is array (1 .. 3) of Boolean;
   type Current_Limit  is array (1 .. 3) of Integer;









   ------------------------------
   --- btTranslationalLimitMotor
   --

   type btTranslationalLimitMotor is tagged
      record
         m_lowerLimit         : Vector_3;        -- the constraint lower limits
         m_upperLimit         : Vector_3;        -- the constraint upper limits
         m_accumulatedImpulse : Vector_3;

         m_limitSoftness      : Real;          -- Softness for linear limit
         m_damping            : Real;          -- Damping for linear limit
         m_restitution        : Real;          -- Bounce parameter for linear limit
         m_normalCFM          : Vector_3;        -- Constraint force mixing factor
         m_stopERP            : Vector_3;        -- Error tolerance factor when joint is at limit
         m_stopCFM            : Vector_3;        -- Constraint force mixing factor when joint is at limit.

         m_enableMotor        : Enable_Motor;
         m_targetVelocity     : Vector_3;        -- target motor velocity
         m_maxMotorForce      : Vector_3;        -- max force on motor
         m_currentLimitError  : Vector_3;        -- How much is violated this limit
         m_currentLinearDiff  : Vector_3;        -- Current relative offset of constraint frames
         m_currentLimit       : Current_Limit;   -- 0=free, 1=at lower limit, 2=at upper limit
      end record;




   function to_btTranslationalLimitMotor                                        return btTranslationalLimitMotor;
   function to_btTranslationalLimitMotor (Other : in btTranslationalLimitMotor) return btTranslationalLimitMotor;



   function isLimited      (Self : in btTranslationalLimitMotor;   limitIndex : in Integer) return Boolean;
   function needApplyForce (Self : in btTranslationalLimitMotor;   limitIndex : in Integer) return Boolean;

   function solveLinearAxis (Self : access btTranslationalLimitMotor;  timeStep         : in math.Real;
                                                                       jacDiagABInv     : in math.Real;
                                                                       body1            : in impact.d3.Object.rigid.view;
                                                                       pointInA         : in math.Vector_3;
                                                                       body2            : in impact.d3.Object.rigid.view;
                                                                       pointInB         : in math.Vector_3;
                                                                       limitIndex       : in Integer;
                                                                       axis_normal_on_a : in math.Vector_3;
                             anchorPos        : in math.Vector_3) return math.Real;





--  class btTranslationalLimitMotor
--  {
--  public:
--          impact.d3.Vector m_lowerLimit;//!< the constraint lower limits
--      impact.d3.Vector m_upperLimit;//!< the constraint upper limits
--      impact.d3.Vector m_accumulatedImpulse;
--      //! Linear_Limit_parameters
--      //!@{
--      impact.d3.Scalar        m_limitSoftness;//!< Softness for linear limit
--      impact.d3.Scalar        m_damping;//!< Damping for linear limit
--      impact.d3.Scalar        m_restitution;//! Bounce parameter for linear limit
--          impact.d3.Vector        m_normalCFM;//!< Constraint force mixing factor
--      impact.d3.Vector        m_stopERP;//!< Error tolerance factor when joint is at limit
--          impact.d3.Vector        m_stopCFM;//!< Constraint force mixing factor when joint is at limit
--      //!@}
--          bool                m_enableMotor[3];
--      impact.d3.Vector        m_targetVelocity;//!< target motor velocity
--      impact.d3.Vector        m_maxMotorForce;//!< max force on motor
--      impact.d3.Vector        m_currentLimitError;//!  How much is violated this limit
--      impact.d3.Vector        m_currentLinearDiff;//!  Current relative offset of constraint frames
--      int                        m_currentLimit[3];//!< 0=free, 1=at lower limit, 2=at upper limit
--


--
--      //! Test limit          --- tbd: where is this ported ?
--          /*!
--      - free means upper < lower,
--      - locked means upper == lower
--      - limited means upper > lower
--      - limitIndex: first 3 are linear, next 3 are angular
--      */

--  };







   ----------------------------
   --- impact.d3.Joint.any
   --

   ----------
   --- Forge
   --

   function to_any_Joint (rbA,      rbB            : in impact.d3.Object.rigid.view;
                          frameInA, frameInB       : in Transform_3d;
                          useLinearReferenceFrameA : in Boolean       ) return Item;

--      impact.d3.Joint.any(impact.d3.Object.rigid& rbA, impact.d3.Object.rigid& rbB, const impact.d3.Transform& frameInA, const impact.d3.Transform& frameInB ,bool useLinearReferenceFrameA);



   function to_any_Joint (rbB                      : in impact.d3.Object.rigid.view;
                                        frameInB                 : in Transform_3d;
                                        useLinearReferenceFrameB : in Boolean       ) return Item;
   --
   --  Not providing rigidbody A means implicitly using worldspace for body A.

--      impact.d3.Joint.any(impact.d3.Object.rigid& rbB, const impact.d3.Transform& frameInB, bool useLinearReferenceFrameB);





   ----------------
   --- Atttributes
   --

   overriding procedure setParam (Self  :    out Item;
                       num   : in     impact.d3.Joint.btConstraintParams;
                       value : in     Math.Real;
                       axis  : in     Integer := -1);
   --
   --  Override the default global value of a parameter (such as ERP or CFM), optionally provide the axis (0..5).
   --  If no axis is provided, it uses the default axis for this constraint.

--          virtual        void setParam(int num, impact.d3.Scalar value, int axis = -1);



   overriding function  getParam (Self : in Item;
                       num  : in impact.d3.Joint.btConstraintParams;
                       axis : in Integer := -1) return Math.Real;
   --
   --  Return the local value of parameter.

--          virtual        impact.d3.Scalar getParam(int num, int axis = -1) const;





   overriding procedure getInfo1 (Self : in out Item;
                       info :    out impact.d3.Joint.btConstraintInfo1);

--          virtual void getInfo1 (btConstraintInfo1* info);



   procedure getInfo1NonVirtual (Self : in out Item;
                                 info :    out impact.d3.Joint.btConstraintInfo1);

--          void getInfo1NonVirtual (btConstraintInfo1* info);



   overriding procedure getInfo2 (Self : in out Item;
                       info :    out impact.d3.Joint.btConstraintInfo2);

--          virtual void getInfo2 (btConstraintInfo2* info);



   procedure getInfo2NonVirtual (Self    : in out Item;
                                 info    :    out impact.d3.Joint.btConstraintInfo2;
                                 transA  : in     Transform_3d;
                                 transB  : in     Transform_3d;
                                 linVelA : in     Vector_3;
                                 linVelB : in     Vector_3;
                                 angVelA : in     Vector_3;
                                 angVelB : in     Vector_3);

--          void getInfo2NonVirtual (btConstraintInfo2* info,const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& linVelA,const impact.d3.Vector& linVelB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB);







   procedure calculateTransforms (Self : in out Item);

--          void calculateTransforms();



   procedure calculateTransforms (Self   : in out Item;
                                  transA : in     Transform_3d;
                                  transB : in     Transform_3d);
   --
   --  Calcs global transform of the offsets.
   --
   --  Calcs the global transform for the joint offset for body A an B, and also calcs the agle differences between the bodies.
   --
   --  See also: impact.d3.Joint.any.getCalculatedTransformA, impact.d3.Joint.any.getCalculatedTransformB, impact.d3.Joint.any.calculateAngleInfo

--      void calculateTransforms(const impact.d3.Transform& transA,const impact.d3.Transform& transB);




   function getCalculatedTransformA (Self : in Item) return Transform_3d;
   --
   --  Gets the global transform of the offset for body A
   --
   --  See also: impact.d3.Joint.any.getFrameOffsetA, impact.d3.Joint.any.getFrameOffsetB, impact.d3.Joint.any.calculateAngleInfo.

--      const impact.d3.Transform & getCalculatedTransformA() const



   function getCalculatedTransformB (Self : in Item) return Transform_3d;
   --
   --  Gets the global transform of the offset for body B
   --
   --  See also: impact.d3.Joint.any.getFrameOffsetA, impact.d3.Joint.any.getFrameOffsetB, impact.d3.Joint.any.calculateAngleInfo.

--      const impact.d3.Transform & getCalculatedTransformB() const




   procedure setFrameOffsetA (Self : in out Item;   To : in Transform_3d);
   procedure setFrameOffsetB (Self : in out Item;   To : in Transform_3d);


   function getFrameOffsetA (Self : in Item) return Transform_3d;

--      const impact.d3.Transform & getFrameOffsetA() const
--      {
--              return m_frameInA;
--      }



   function getFrameOffsetB (Self : in Item) return Transform_3d;

--      const impact.d3.Transform & getFrameOffsetB() const
--      {
--              return m_frameInB;
--      }





   overriding procedure buildJacobian (Self : in out Item);
   --
   --  Performs Jacobian calculation, and also calculates angle differences and axis.

--      virtual void        buildJacobian();




   procedure updateRHS (Self     : in out Item;
                        timeStep : in     Real);

--      void        updateRHS(impact.d3.Scalar        timeStep);





   function getAxis (Self       : in Item;
                     axis_index : in Integer) return Vector_3;
   --
   --  Get the rotation axis in global coordinates.
   --
   --  impact.d3.Joint.any.buildJacobian must be called previously.

--      impact.d3.Vector getAxis(int axis_index) const;





   function getRelativePivotPosition (Self       : in Item;
                                      axis_index : in Integer) return Real;
   --
   --  Get the relative position of the constraint pivot
   --
   --  impact.d3.Joint.any::calculateTransforms() must be called previously.

--          impact.d3.Scalar getRelativePivotPosition(int axis_index) const;





   procedure setFrames (Self   : in out Item;
                        frameA : in     Transform_3d;
                        frameB : in     Transform_3d);

--          void setFrames(const impact.d3.Transform & frameA, const impact.d3.Transform & frameB);




   function getAngle (Self       : in Item;
                      axis_index : in Integer) return Real;
   --
   --  Get the relative Euler angle.
   --
   --  impact.d3.Joint.any::calculateTransforms() must be called previously.

--      impact.d3.Scalar getAngle(int axis_index) const;




   function testAngularLimitMotor (Self       : access Item;
                                   axis_index : in     Integer) return Boolean;
   --
   --  Test angular limit.
   --
   --  Calculates angular correction and returns true if limit needs to be corrected.
   --  impact.d3.Joint.any::calculateTransforms() must be called previously.

--      bool testAngularLimitMotor(int axis_index);





   procedure setLinearLowerLimit (Self        : in out Item;
                                  linearLower : in     Vector_3);

   procedure getLinearLowerLimit (Self        : in     Item;
                                  linearLower :    out Vector_3);

   procedure setLinearUpperLimit (Self        : in out Item;
                                  linearUpper : in     Vector_3);

   procedure getLinearUpperLimit (Self        : in     Item;
                                  linearUpper :    out Vector_3);

   procedure setAngularLowerLimit (Self         : in out Item;
                                   angularLower : in     Vector_3);

   procedure getAngularLowerLimit (Self         : in     Item;
                                   angularLower :    out Vector_3);

   procedure setAngularUpperLimit (Self         : in out Item;
                                   angularUpper : in     Vector_3);

   procedure getAngularUpperLimit (Self         : in     Item;
                                   angularUpper :    out Vector_3);


   procedure setRotationalLimitMotor (Self  : in out Item;   index : in Integer;   To : in btRotationLimitMotor'Class);

   function getRotationalLimitMotor (Self  : in     Item;   index : in Integer) return        btRotationLimitMotor'Class;
   function getRotationalLimitMotor (Self  : access Item;   index : in Integer) return access btRotationLimitMotor'Class;
   --
   --  Retrieves the angular limit information.

--      btRotationalLimitMotor * getRotationalLimitMotor(int index)
--      {
--              return &m_angularLimits[index];
--      }





   function getTranslationalLimitMotor (Self  : access Item) return access btTranslationalLimitMotor'Class;
   --
   --  Retrieves the linear limit information

--      btTranslationalLimitMotor * getTranslationalLimitMotor()
--      {
--              return &m_linearLimits;
--      }
--





   procedure setLimit (Self : in out Item;
                       axis : in     Integer;
                       lo   : in out Real;
                       hi   : in out Real);

   function isLimited (Self        : in Item;
                       limitIndex : in Integer) return Boolean;





   procedure calcAnchorPos (Self : in out Item);     -- overridable

--          virtual void calcAnchorPos(void);




   function get_limit_motor_info2 (Self       : in Item;
                                   limot      : in btRotationLimitMotor'Class;
                                   transA     : in Transform_3d;
                                   transB     : in Transform_3d;
                                   linVelA    : in Vector_3;
                                   linVelB    : in Vector_3;
                                   angVelA    : in Vector_3;
                                   angVelB    : in Vector_3;
                                   info       : in impact.d3.Joint.btConstraintInfo2;
                                   row        : in Integer;
                                   ax1        : in Vector_3;
                                   rotational : in Boolean;
                                   rotAllowed : in Boolean := False) return Integer;

--          int get_limit_motor_info2(        btRotationalLimitMotor * limot,
--                                                                  const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& linVelA,const impact.d3.Vector& linVelB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB,
--                                                                  btConstraintInfo2 *info, int row, impact.d3.Vector& ax1, int rotational, int rotAllowed = false);



   --- Access for UseFrameOffset.
   --

   function getUseFrameOffset (Self : in Item) return Boolean;

--          bool getUseFrameOffset() { return m_useOffsetForConstraintFrame; }



   procedure setUseFrameOffset (Self             : in out Item;
                                frameOffsetOnOff : in     Boolean);

--          void setUseFrameOffset(bool frameOffsetOnOff) { m_useOffsetForConstraintFrame = frameOffsetOnOff; }





   procedure setAxis (Self  : in out Item;
                      axis1 : in     Vector_3;
                      axis2 : in     Vector_3);

--          void setAxis( const impact.d3.Vector& axis1, const impact.d3.Vector& axis2);




private

   type Item is new impact.d3.Joint.Item with
      record
         --  relative frames
         m_frameInA                    : Transform_3d;                      -- the constraint space w.r.t body A
         m_frameInB                        : Transform_3d;                      -- the constraint space w.r.t body B

         --  Jacobians
         m_jacLinear                   : impact.d3.jacobian_Entry.Items (1 .. 3);   -- 3 orthogonal linear constraints
         m_jacAng                      : impact.d3.jacobian_Entry.Items (1 .. 3);   -- 3 orthogonal angular constraints


         m_linearLimits                : aliased btTranslationalLimitMotor := to_btTranslationalLimitMotor;   -- Linear_Limit_parameters
         m_angularLimits               : Angular_Limits := (others => to_btRotationLimitMotor);               -- hinge_parameters

         m_timeStep                    : Real;
         m_calculatedTransformA        : Transform_3d;
         m_calculatedTransformB        : Transform_3d;
         m_calculatedAxisAngleDiff     : aliased Vector_3;
         m_calculatedAxis              : Axes;
         m_calculatedLinearDiff        : Vector_3;
         m_factA                       : Real;
         m_factB                       : Real;
         m_hasStaticBody               : Boolean;
         m_AnchorPos                   : Vector_3;                       -- point betwen pivots of bodies A and B to solve linear axes
         m_useLinearReferenceFrameA    : Boolean;
         m_useOffsetForConstraintFrame : Boolean;
         m_flags                       : Flags;

         m_useSolveConstraintObsolete  : Boolean;                        -- for backwards compatibility during the transition to 'getInfo/getInfo2'
      end record;



--  protected:
--
--          //! relative_frames
--      //!@{
--          impact.d3.Transform        m_frameInA;//!< the constraint space w.r.t body A
--      impact.d3.Transform        m_frameInB;//!< the constraint space w.r.t body B
--      //!@}
--
--      //! Jacobians
--      //!@{
--      impact.d3.jacobian_Entry        m_jacLinear[3];//!< 3 orthogonal linear constraints
--      impact.d3.jacobian_Entry        m_jacAng[3];//!< 3 orthogonal angular constraints
--      //!@}
--
--          //! Linear_Limit_parameters
--      //!@{
--      btTranslationalLimitMotor m_linearLimits;
--      //!@}
--
--
--      //! hinge_parameters
--      //!@{
--      btRotationalLimitMotor m_angularLimits[3];
--          //!@}
--
--
--  protected:
--      //! temporal variables
--      //!@{
--      impact.d3.Scalar m_timeStep;
--      impact.d3.Transform m_calculatedTransformA;
--      impact.d3.Transform m_calculatedTransformB;
--      impact.d3.Vector m_calculatedAxisAngleDiff;
--      impact.d3.Vector m_calculatedAxis[3];
--      impact.d3.Vector m_calculatedLinearDiff;
--          impact.d3.Scalar        m_factA;
--          impact.d3.Scalar        m_factB;
--          bool                m_hasStaticBody;
--
--          impact.d3.Vector m_AnchorPos; // point betwen pivots of bodies A and B to solve linear axes
--
--      bool        m_useLinearReferenceFrameA;
--          bool        m_useOffsetForConstraintFrame;
--
--          int                m_flags;
--
--
--  public:
--
--          ///for backwards compatibility during the transition to 'getInfo/getInfo2'
--          bool                m_useSolveConstraintObsolete;




   function setAngularLimits (Self       : access Item;
                             info       : in impact.d3.Joint.btConstraintInfo2;
                             row_offset : in Integer;
                             transA     : in Transform_3d;
                             transB     : in Transform_3d;
                             linVelA    : in Vector_3;
                             linVelB    : in Vector_3;
                             angVelA    : in Vector_3;
                              angVelB    : in Vector_3) return Integer;
--          int setAngularLimits(btConstraintInfo2 *info, int row_offset,const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& linVelA,const impact.d3.Vector& linVelB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB);



   function setLinearLimits (Self    : in Item;
                            info    : in impact.d3.Joint.btConstraintInfo2;
                            row     : in Integer;
                            transA  : in Transform_3d;
                            transB  : in Transform_3d;
                            linVelA : in Vector_3;
                            linVelB : in Vector_3;
                            angVelA : in Vector_3;
                            angVelB : in Vector_3) return Integer;

--          int setLinearLimits(btConstraintInfo2 *info, int row, const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& linVelA,const impact.d3.Vector& linVelB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB);


   procedure buildLinearJacobian (Self        : in out Item;
                                  jacLinear   :    out impact.d3.jacobian_Entry.item;
                                  normalWorld : in     Vector_3;
                                  pivotAInW   : in     Vector_3;
                                  pivotBInW   : in     Vector_3);
--      void buildLinearJacobian(
--          impact.d3.jacobian_Entry & jacLinear,const impact.d3.Vector & normalWorld,
--          const impact.d3.Vector & pivotAInW,const impact.d3.Vector & pivotBInW);


   procedure buildAngularJacobian (Self       : in out Item;
                                   jacAngular :    out impact.d3.jacobian_Entry.Item;
                                   jointAxisW : in     Vector_3);

   --      void buildAngularJacobian(impact.d3.jacobian_Entry & jacAngular,const impact.d3.Vector & jointAxisW);



   procedure calculateLinearInfo (Self : in out Item);
   --
   --  tests linear limits

--          void calculateLinearInfo();



   procedure calculateAngleInfo  (Self : in out Item);
   --
   --  calcs the euler angles between the two bodies.

--      void calculateAngleInfo();





end impact.d3.Joint.any;
