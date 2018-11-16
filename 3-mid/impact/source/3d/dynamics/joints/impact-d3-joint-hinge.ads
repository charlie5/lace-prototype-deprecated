with impact.d3.Joint;
with impact.d3.Object.rigid;

with impact.d3.jacobian_Entry;
--  #include "LinearMath/impact.d3.Vector.h"
--  #include "impact.d3.jacobian_Entry.h"
--  #include "impact.d3.Joint.h"
--
--  class impact.d3.Object.rigid;




package impact.d3.Joint.hinge
--
--  Hinge constraint between two rigidbodies each with a pivotpoint that descibes the axis location in local space
--  axis defines the orientation of the hinge axis
--
--  Hinge Constraint by Dirk Gregorius. Limits added by Marcus Hennix at Starbreeze Studios.
--
is
   use Math;


   type Item is new impact.d3.Joint.item with private;

   vHinge : Vector_3 := (0.0, 0.0, 1.0);
   --  static impact.d3.Vector vHinge(0, 0, impact.d3.Scalar(1));



   type btHingeFlags is (BT_HINGE_FLAGS_CFM_STOP,
                         BT_HINGE_FLAGS_ERP_STOP,
                         BT_HINGE_FLAGS_CFM_NORM);


   function to_hinge_Joint (rbA                : impact.d3.Object.rigid.view;
                            rbB                : impact.d3.Object.rigid.view;
                            pivotInA           : Vector_3;
                            pivotInB           : Vector_3;
                            axisInA            : Vector_3;
                            axisInB            : Vector_3;
                            useReferenceFrameA : Boolean := False) return Item;

   function to_hinge_Joint (rbA                : impact.d3.Object.rigid.view;
                            pivotInA           : Vector_3;
                            axisInA            : Vector_3;
                            useReferenceFrameA : Boolean := False) return Item;

   function to_hinge_Joint (rbA                : impact.d3.Object.rigid.view;
                            rbB                : impact.d3.Object.rigid.view;
                            rbAFrame           : Transform_3d;
                            rbBFrame           : Transform_3d;
                            useReferenceFrameA : Boolean := False) return Item;

   function to_hinge_Joint (rbA                : impact.d3.Object.rigid.view;
                            rbAFrame           : Transform_3d;
                            useReferenceFrameA : Boolean := False) return Item;



   overriding procedure setParam (Self  :    out Item;
                       num   : in     impact.d3.Joint.btConstraintParams;
                       value : in     math.Real;
                       axis  : in     Integer := -1);


   overriding function  getParam (Self : in Item;
                       num  : in impact.d3.Joint.btConstraintParams;
                       axis : in Integer := -1) return math.Real;


   overriding procedure getInfo1 (Self : in out Item;
                       info :    out impact.d3.Joint.btConstraintInfo1);

   procedure getInfo1NonVirtual (Self : in     Item;
                                 info :    out impact.d3.Joint.btConstraintInfo1);

   overriding procedure getInfo2 (Self : in out Item;
                       info :    out impact.d3.Joint.btConstraintInfo2);


   procedure getInfo2NonVirtual (Self    : in out Item;
                                 info    :    out impact.d3.Joint.btConstraintInfo2;
                                 transA  : in     Transform_3d;
                                 transB  : in     Transform_3d;
                                 angVelA : in     Vector_3;
                                 angVelB : in     Vector_3);

   procedure getInfo2Internal (Self    : in out Item;
                               info    :    out impact.d3.Joint.btConstraintInfo2;
                               transA  : in     Transform_3d;
                               transB  : in     Transform_3d;
                               angVelA : in     Vector_3;
                               angVelB : in     Vector_3);

   procedure getInfo2InternalUsingFrameOffset (Self    : in out Item;
                                               info    :    out impact.d3.Joint.btConstraintInfo2;
                                               transA  : in     Transform_3d;
                                               transB  : in     Transform_3d;
                                               angVelA : in     Vector_3;
                                               angVelB : in     Vector_3);


   procedure updateRHS (Self     : in out Item;
                        timeStep : in     Real);

   overriding procedure buildJacobian (Self : in out Item);



   procedure setFrameOffsetA (Self : in out Item;   To : in Transform_3d);
   procedure setFrameOffsetB (Self : in out Item;   To : in Transform_3d);


   function getFrameOffsetA (Self : in Item) return Transform_3d;
   function getFrameOffsetB (Self : in Item) return Transform_3d;




   procedure setFrames (Self   : in out Item;
                        frameA : in     Transform_3d;
                        frameB : in     Transform_3d);

   procedure setAngularOnly (Self        : in out Item;
                             angularOnly : in     Boolean);

   procedure enableAngularMotor (Self            : in out Item;
                                 enableMotor     : in     Boolean;
                                 targetVelocity  : in     Real;
                                 maxMotorImpulse : in     Real);

   procedure enableMotor (Self        : in out Item;
                          enableMotor : in Boolean);

   procedure setMaxMotorImpulse (Self            : in out Item;
                                 maxMotorImpulse : in     Real);

   procedure setMotorTarget (Self  : in out Item;
                             qAinB : in     Quaternion;
                             dt    : in     Real   );

   procedure setMotorTarget (Self        : in out Item;
                             targetAngle : in out Real;
                             dt          : in     Real);

   procedure setLimit (Self             : in out Item;
                       low              : in     Real;
                       high             : in     Real;
                       softness         : in     Real := 0.9;
                       biasFactor       : in     Real := 0.3;
                       relaxationFactor : in     Real := 1.0);

   procedure setAxis (Self    : in out Item;
                      axisInA : in     Vector_3);

   function getLowerLimit (Self : in Item) return Real;

   function getUpperLimit (Self : in Item) return Real;

   function getHingeAngle (Self : in Item) return Real;

   function getHingeAngle (Self   : in item;
                           transA : in Transform_3d;
                           transB : in Transform_3d) return Real;

   procedure testLimit (Self   : in out Item;
                        transA : in     Transform_3d;
                        transB : in     Transform_3d);

   function getAFrame (Self : in Item) return Transform_3d;
   function getBFrame (Self : in Item) return Transform_3d;

   function getSolveLimit (Self : in Item) return Boolean;

   function getLimitSign (Self : in Item) return Real;

   function getAngularOnly (Self : in Item) return Boolean;

   function getEnableAngularMotor (Self : in Item) return Boolean;

   function getMotorTargetVelosity (Self : in Item) return Real;

   function getMaxMotorImpulse (Self : in Item) return Real;

   function getUseFrameOffset (Self : in Item) return Boolean;

   procedure setUseFrameOffset (Self             : in out Item;
                                frameOffsetOnOff : in     Boolean);


private


   type Item is new impact.d3.Joint.item with
      record
         m_jac                         : impact.d3.jacobian_Entry.Items (1 .. 3);                -- The 3 orthogonal linear constraints.
         m_jacAng                      : impact.d3.jacobian_Entry.Items (1 .. 3);                -- The 2 orthogonal angular constraints and 1 for limit/motor.
         m_rbAFrame                    : Transform_3d;
         m_rbBFrame                    : Transform_3d;
         m_motorTargetVelocity         : Real;
         m_maxMotorImpulse             : Real;
         m_limit                       : impact.d3.Joint.btAngularLimit;
         m_lowerLimit                  : Real;
         m_upperLimit                  : Real;
         m_limitSign                   : Real;
         m_correction                  : Real;
         m_limitSoftness               : Real;
         m_biasFactor                  : Real;
         m_relaxationFactor            : Real;
         m_solveLimit                  : Boolean;
         m_kHinge                      : Real;
         m_accLimitImpulse             : Real;
         m_hingeAngle                  : Real;
         m_referenceSign               : Real;
         m_angularOnly                 : Boolean;
         m_enableAngularMotor          : Boolean;
         m_useSolveConstraintObsolete  : Boolean;
         m_useOffsetForConstraintFrame : Boolean;
         m_useReferenceFrameA          : Boolean;
         m_accMotorImpulse             : Real;
         m_flags                       : Flags;
         m_normalCFM                   : Real;
         m_stopCFM                     : Real;
         m_stopERP                     : Real;
      end record;





   for btHingeFlags use (BT_HINGE_FLAGS_CFM_STOP => 1,
                         BT_HINGE_FLAGS_ERP_STOP => 2,
                         BT_HINGE_FLAGS_CFM_NORM => 4);




end impact.d3.Joint.hinge;



--  #define _BT_USE_CENTER_LIMIT_ 1






--
--  class impact.d3.Joint.hinge : public impact.d3.Joint
--  {

--  #ifdef IN_PARALLELL_SOLVER
--  public:
--  #endif


--          impact.d3.jacobian_Entry        m_jac[3]; //3 orthogonal linear constraints
--          impact.d3.jacobian_Entry        m_jacAng[3]; //2 orthogonal angular constraints+ 1 for limit/motor
--
--          impact.d3.Transform m_rbAFrame; // constraint axii. Assumes z is hinge axis.
--          impact.d3.Transform m_rbBFrame;
--
--          impact.d3.Scalar        m_motorTargetVelocity;
--          impact.d3.Scalar        m_maxMotorImpulse;
--


--  #ifdef        _BT_USE_CENTER_LIMIT_
--          btAngularLimit        m_limit;
--  #else
--          impact.d3.Scalar        m_lowerLimit;
--          impact.d3.Scalar        m_upperLimit;
--          impact.d3.Scalar        m_limitSign;
--          impact.d3.Scalar        m_correction;
--
--          impact.d3.Scalar        m_limitSoftness;
--          impact.d3.Scalar        m_biasFactor;
--          impact.d3.Scalar        m_relaxationFactor;
--
--          bool                m_solveLimit;
--  #endif



--          impact.d3.Scalar        m_kHinge;
--
--
--          impact.d3.Scalar        m_accLimitImpulse;
--          impact.d3.Scalar        m_hingeAngle;
--          impact.d3.Scalar        m_referenceSign;
--
--          bool                m_angularOnly;
--          bool                m_enableAngularMotor;
--          bool                m_useSolveConstraintObsolete;
--          bool                m_useOffsetForConstraintFrame;
--          bool                m_useReferenceFrameA;
--
--          impact.d3.Scalar        m_accMotorImpulse;
--
--          int                        m_flags;
--          impact.d3.Scalar        m_normalCFM;
--          impact.d3.Scalar        m_stopCFM;
--          impact.d3.Scalar        m_stopERP;



--  public:
--
--          impact.d3.Joint.hinge(impact.d3.Object.rigid& rbA,impact.d3.Object.rigid& rbB, const impact.d3.Vector& pivotInA,const impact.d3.Vector& pivotInB, const impact.d3.Vector& axisInA,const impact.d3.Vector& axisInB, bool useReferenceFrameA = false);

--          impact.d3.Joint.hinge(impact.d3.Object.rigid& rbA,const impact.d3.Vector& pivotInA,const impact.d3.Vector& axisInA, bool useReferenceFrameA = false);

--          impact.d3.Joint.hinge(impact.d3.Object.rigid& rbA,impact.d3.Object.rigid& rbB, const impact.d3.Transform& rbAFrame, const impact.d3.Transform& rbBFrame, bool useReferenceFrameA = false);

--          impact.d3.Joint.hinge(impact.d3.Object.rigid& rbA,const impact.d3.Transform& rbAFrame, bool useReferenceFrameA = false);


--          virtual void        buildJacobian();

--          virtual void getInfo1 (btConstraintInfo1* info);

--          void getInfo1NonVirtual(btConstraintInfo1* info);

--          virtual void getInfo2 (btConstraintInfo2* info);

--          void        getInfo2NonVirtual(btConstraintInfo2* info,const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB);

--          void        getInfo2Internal(btConstraintInfo2* info,const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB);
--          void        getInfo2InternalUsingFrameOffset(btConstraintInfo2* info,const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB);


--          void        updateRHS(impact.d3.Scalar        timeStep);

--          void setFrames(const impact.d3.Transform& frameA, const impact.d3.Transform& frameB);

--          void setMotorTarget(const impact.d3.Quaternion& qAinB, impact.d3.Scalar dt); // qAinB is rotation of body A wrt body B.
--          void setMotorTarget(impact.d3.Scalar targetAngle, impact.d3.Scalar dt);

--          impact.d3.Scalar getHingeAngle();

--          impact.d3.Scalar getHingeAngle(const impact.d3.Transform& transA,const impact.d3.Transform& transB);

--          void testLimit(const impact.d3.Transform& transA,const impact.d3.Transform& transB);

--          ///override the default global value of a parameter (such as ERP or CFM), optionally provide the axis (0..5).
--          ///If no axis is provided, it uses the default axis for this constraint.
--          virtual        void        setParam(int num, impact.d3.Scalar value, int axis = -1);


--          ///return the local value of parameter
--          virtual        impact.d3.Scalar getParam(int num, int axis = -1) const;

--  };
