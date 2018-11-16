with impact.d3.Quaternions;
with impact.d3.Vector; use impact.d3.Vector;
with ada.unchecked_Conversion;
with interfaces.C;
with impact.d3.Containers;
with impact.d3.Transform;


package body impact.d3.Joint.hinge
is
   use Interfaces;



   HINGE_USE_OBSOLETE_SOLVER : constant Boolean := False;
   HINGE_USE_FRAME_OFFSET    : constant Boolean := True;



   function to_Flag is new ada.unchecked_Conversion (btHingeFlags, Flags);





   function to_hinge_Joint (rbA                : impact.d3.Object.rigid.view;
                            rbB                : impact.d3.Object.rigid.view;
                            pivotInA           : Vector_3;
                            pivotInB           : Vector_3;
                            axisInA            : Vector_3;
                            axisInB            : Vector_3;
                            useReferenceFrameA : Boolean := False) return Item
   is
      use impact.d3.Quaternions;

      Self     : Item;
   begin
      Self.define (impact.d3.Joint.HINGE_CONSTRAINT_TYPE,  rbA, rbB);

      Self.m_angularOnly                 := False;
      Self.m_enableAngularMotor          := False;
      Self.m_useSolveConstraintObsolete  := HINGE_USE_OBSOLETE_SOLVER;
      Self.m_useOffsetForConstraintFrame := HINGE_USE_FRAME_OFFSET;
      Self.m_useReferenceFrameA          := useReferenceFrameA;
      Self.m_flags                       := 0;


      declare
         use impact.d3.Transform;
         rbAxisA1   : Vector_3 := math.Col (getBasis (getRigidBodyA (Self).getCenterOfMassTransform), 3);
         rbAxisA2   : Vector_3;
         projection : Real := dot (axisInA, rbAxisA1);

      begin
         setOrigin (Self.m_rbAFrame, pivotInA);

         if projection >= 1.0 - impact.d3.Scalar.SIMD_EPSILON then
            rbAxisA1 := math.Col (getBasis (getRigidBodyA (Self).getCenterOfMassTransform), 3); -- FIXME
            rbAxisA2 := math.Col (getBasis (getRigidBodyA (Self).getCenterOfMassTransform), 2);
         elsif projection <= -1.0 + impact.d3.Scalar.SIMD_EPSILON then
            rbAxisA1 := math.Col (getBasis (getRigidBodyA (Self).getCenterOfMassTransform), 3);
            rbAxisA2 := math.Col (getBasis (getRigidBodyA (Self).getCenterOfMassTransform), 2);
         else
            rbAxisA2 := cross (axisInA, rbAxisA2);
            rbAxisA1 := cross (rbAxisA2, axisInA);
         end if;

         setBasis (Self.m_rbAFrame, ((rbAxisA1 (1), rbAxisA2 (1), axisInA (1)),
           (rbAxisA1 (2), rbAxisA2 (2), axisInA (2)),
           (rbAxisA1 (3), rbAxisA2 (3), axisInA (3))));

         declare
            rotationArc : constant Quaternion := shortestArcQuat (axisInA, axisInB);
            rbAxisB1    : constant Vector_3 := quatRotate (rotationArc, rbAxisA1);
            rbAxisB2    : constant Vector_3 := cross (axisInB, rbAxisB1);
         begin
            setOrigin (Self.m_rbBFrame, pivotInB);
            setBasis (Self.m_rbBFrame, ((rbAxisB1 (1), rbAxisB2 (1), axisInB (1)),
              (rbAxisB1 (2), rbAxisB2 (2), axisInB (2)),
              (rbAxisB1 (3), rbAxisB2 (3), axisInB (3))));
            Self.m_lowerLimit := 1.0;
            Self.m_upperLimit := -1.0;
            Self.m_biasFactor := 0.3;
            Self.m_relaxationFactor := 1.0;
            Self.m_limitSoftness := 0.9;
            Self.m_solveLimit := False;
            Self.m_referenceSign := (if Self.m_useReferenceFrameA then -1.0 else 1.0);
         end;

      end;

      return Self;
   end to_hinge_Joint;



   --  impact.d3.Joint.hinge::impact.d3.Joint.hinge(impact.d3.Object.rigid& rbA,impact.d3.Object.rigid& rbB, const impact.d3.Vector& pivotInA,const impact.d3.Vector& pivotInB,
   --                                                                           const impact.d3.Vector& axisInA,const impact.d3.Vector& axisInB, bool useReferenceFrameA)
   --                                                                           :impact.d3.Joint(HINGE_CONSTRAINT_TYPE, rbA,rbB),
   --  #ifdef _BT_USE_CENTER_LIMIT_
   --                                                                           m_limit(),
   --  #endif
   --                                                                           m_angularOnly(false),
   --                                                                           m_enableAngularMotor(false),
   --                                                                           m_useSolveConstraintObsolete(HINGE_USE_OBSOLETE_SOLVER),
   --                                                                           m_useOffsetForConstraintFrame(HINGE_USE_FRAME_OFFSET),
   --                                                                           m_useReferenceFrameA(useReferenceFrameA),
   --                                                                           m_flags(0)
   --  {
   --          m_rbAFrame.getOrigin() = pivotInA;
   --
   --          // since no frame is given, assume this to be zero angle and just pick rb transform axis
   --          impact.d3.Vector rbAxisA1 = rbA.getCenterOfMassTransform().getBasis().getColumn(0);
   --
   --          impact.d3.Vector rbAxisA2;
   --          impact.d3.Scalar projection = axisInA.dot(rbAxisA1);
   --          if (projection >= 1.0f - SIMD_EPSILON) {
   --                  rbAxisA1 = -rbA.getCenterOfMassTransform().getBasis().getColumn(2);
   --                  rbAxisA2 = rbA.getCenterOfMassTransform().getBasis().getColumn(1);
   --          } else if (projection <= -1.0f + SIMD_EPSILON) {
   --                  rbAxisA1 = rbA.getCenterOfMassTransform().getBasis().getColumn(2);
   --                  rbAxisA2 = rbA.getCenterOfMassTransform().getBasis().getColumn(1);
   --          } else {
   --                  rbAxisA2 = axisInA.cross(rbAxisA1);
   --                  rbAxisA1 = rbAxisA2.cross(axisInA);
   --          }
   --
   --          m_rbAFrame.getBasis().setValue( rbAxisA1.getX(),rbAxisA2.getX(),axisInA.getX(),
   --                                                                          rbAxisA1.getY(),rbAxisA2.getY(),axisInA.getY(),
   --                                                                          rbAxisA1.getZ(),rbAxisA2.getZ(),axisInA.getZ() );
   --
   --          impact.d3.Quaternion rotationArc = shortestArcQuat(axisInA,axisInB);
   --          impact.d3.Vector rbAxisB1 =  quatRotate(rotationArc,rbAxisA1);
   --          impact.d3.Vector rbAxisB2 =  axisInB.cross(rbAxisB1);
   --
   --          m_rbBFrame.getOrigin() = pivotInB;
   --          m_rbBFrame.getBasis().setValue( rbAxisB1.getX(),rbAxisB2.getX(),axisInB.getX(),
   --                                                                          rbAxisB1.getY(),rbAxisB2.getY(),axisInB.getY(),
   --                                                                          rbAxisB1.getZ(),rbAxisB2.getZ(),axisInB.getZ() );
   --
   --  #ifndef        _BT_USE_CENTER_LIMIT_
   --          //start with free
   --          m_lowerLimit = impact.d3.Scalar(1.0f);
   --          m_upperLimit = impact.d3.Scalar(-1.0f);
   --          m_biasFactor = 0.3f;
   --          m_relaxationFactor = 1.0f;
   --          m_limitSoftness = 0.9f;
   --          m_solveLimit = false;
   --  #endif
   --          m_referenceSign = m_useReferenceFrameA ? impact.d3.Scalar(-1.f) : impact.d3.Scalar(1.f);
   --  }

   function to_hinge_Joint (rbA                : impact.d3.Object.rigid.view;
                            pivotInA           : Vector_3;
                            axisInA            : Vector_3;
                            useReferenceFrameA : Boolean := False) return Item
   is
      use impact.d3.Quaternions, impact.d3.Transform;
      use math.Vectors;

      Self : Item;
      rbAxisA1, rbAxisA2 : Vector_3;
   begin
      Self.define (impact.d3.Joint.HINGE_CONSTRAINT_TYPE,  rbA);

      Self.m_angularOnly                 := False;
      Self.m_enableAngularMotor          := False;
      Self.m_useSolveConstraintObsolete  := HINGE_USE_OBSOLETE_SOLVER;
      Self.m_useOffsetForConstraintFrame := HINGE_USE_FRAME_OFFSET;
      Self.m_useReferenceFrameA          := useReferenceFrameA;
      Self.m_flags                       := 0;


      btPlaneSpace1 (axisInA, rbAxisA1, rbAxisA2);
      setOrigin (Self.m_rbAFrame, pivotInA);

      setBasis (Self.m_rbAFrame, ((rbAxisA1 (1), rbAxisA2 (1), axisInA (1)),
                                  (rbAxisA1 (2), rbAxisA2 (2), axisInA (2)),
                                  (rbAxisA1 (3), rbAxisA2 (3), axisInA (3))));

      declare
         use linear_Algebra_3d;
         axisInB     : constant Vector_3 := getBasis (getRigidBodyA (Self).getCenterOfMassTransform) * axisInA;
         rotationArc : constant Quaternion := shortestArcQuat (axisInA, axisInB);
         rbAxisB1    : constant Vector_3 := quatRotate (rotationArc, rbAxisA1);
         rbAxisB2    : constant Vector_3 := cross (axisInB, rbAxisB1);
      begin
         setOrigin (Self.m_rbBFrame, getRigidBodyA (Self).getCenterOfMassTransform * pivotInA);
         setBasis (Self.m_rbBFrame, ((rbAxisB1 (1), rbAxisB2 (1), axisInB (1)),
                                     (rbAxisB1 (2), rbAxisB2 (2), axisInB (2)),
                                     (rbAxisB1 (3), rbAxisB2 (3), axisInB (3))));
         Self.m_lowerLimit := 1.0;
         Self.m_upperLimit := -1.0;
         Self.m_biasFactor := 0.3;
         Self.m_relaxationFactor := 1.0;
         Self.m_limitSoftness := 0.9;
         Self.m_solveLimit := False;
         Self.m_referenceSign := (if Self.m_useReferenceFrameA then -1.0 else 1.0);
      end;

      return Self;
   end to_hinge_Joint;

   --  impact.d3.Joint.hinge::impact.d3.Joint.hinge(impact.d3.Object.rigid& rbA,const impact.d3.Vector& pivotInA,const impact.d3.Vector& axisInA, bool useReferenceFrameA)
   --  :impact.d3.Joint(HINGE_CONSTRAINT_TYPE, rbA),
   --  #ifdef _BT_USE_CENTER_LIMIT_
   --  m_limit(),
   --  #endif
   --  m_angularOnly(false), m_enableAngularMotor(false),
   --  m_useSolveConstraintObsolete(HINGE_USE_OBSOLETE_SOLVER),
   --  m_useOffsetForConstraintFrame(HINGE_USE_FRAME_OFFSET),
   --  m_useReferenceFrameA(useReferenceFrameA),
   --  m_flags(0)
   --  {
   --
   --          // since no frame is given, assume this to be zero angle and just pick rb transform axis
   --          // fixed axis in worldspace
   --          impact.d3.Vector rbAxisA1, rbAxisA2;
   --          btPlaneSpace1(axisInA, rbAxisA1, rbAxisA2);
   --
   --          m_rbAFrame.getOrigin() = pivotInA;
   --          m_rbAFrame.getBasis().setValue( rbAxisA1.getX(),rbAxisA2.getX(),axisInA.getX(),
   --                                                                          rbAxisA1.getY(),rbAxisA2.getY(),axisInA.getY(),
   --                                                                          rbAxisA1.getZ(),rbAxisA2.getZ(),axisInA.getZ() );
   --
   --          impact.d3.Vector axisInB = rbA.getCenterOfMassTransform().getBasis() * axisInA;
   --
   --          impact.d3.Quaternion rotationArc = shortestArcQuat(axisInA,axisInB);
   --          impact.d3.Vector rbAxisB1 =  quatRotate(rotationArc,rbAxisA1);
   --          impact.d3.Vector rbAxisB2 = axisInB.cross(rbAxisB1);
   --
   --
   --          m_rbBFrame.getOrigin() = rbA.getCenterOfMassTransform()(pivotInA);
   --          m_rbBFrame.getBasis().setValue( rbAxisB1.getX(),rbAxisB2.getX(),axisInB.getX(),
   --                                                                          rbAxisB1.getY(),rbAxisB2.getY(),axisInB.getY(),
   --                                                                          rbAxisB1.getZ(),rbAxisB2.getZ(),axisInB.getZ() );
   --
   --  #ifndef        _BT_USE_CENTER_LIMIT_
   --          //start with free
   --          m_lowerLimit = impact.d3.Scalar(1.0f);
   --          m_upperLimit = impact.d3.Scalar(-1.0f);
   --          m_biasFactor = 0.3f;
   --          m_relaxationFactor = 1.0f;
   --          m_limitSoftness = 0.9f;
   --          m_solveLimit = false;
   --  #endif
   --          m_referenceSign = m_useReferenceFrameA ? impact.d3.Scalar(-1.f) : impact.d3.Scalar(1.f);
   --  }

   function to_hinge_Joint (rbA                : impact.d3.Object.rigid.view;
                            rbB                : impact.d3.Object.rigid.view;
                            rbAFrame           : Transform_3d;
                            rbBFrame           : Transform_3d;
                            useReferenceFrameA : Boolean := False) return Item
   is
      pragma Unreferenced (rbAFrame, rbBFrame);
      Self : Item;
   begin
      Self.define (impact.d3.Joint.HINGE_CONSTRAINT_TYPE,  rbA, rbB);

      Self.m_angularOnly                 := False;
      Self.m_enableAngularMotor          := False;
      Self.m_useSolveConstraintObsolete  := HINGE_USE_OBSOLETE_SOLVER;
      Self.m_useOffsetForConstraintFrame := HINGE_USE_FRAME_OFFSET;
      Self.m_useReferenceFrameA          := useReferenceFrameA;
      Self.m_flags                       := 0;

      Self.m_lowerLimit := 1.0;
      Self.m_upperLimit := -1.0;
      Self.m_biasFactor := 0.3;
      Self.m_relaxationFactor := 1.0;
      Self.m_limitSoftness := 0.9;
      Self.m_solveLimit := False;
      Self.m_referenceSign := (if Self.m_useReferenceFrameA then -1.0 else 1.0);

      return Self;
   end to_hinge_Joint;

   --  impact.d3.Joint.hinge::impact.d3.Joint.hinge(impact.d3.Object.rigid& rbA,impact.d3.Object.rigid& rbB,
   --                                                                       const impact.d3.Transform& rbAFrame, const impact.d3.Transform& rbBFrame, bool useReferenceFrameA)
   --  :impact.d3.Joint(HINGE_CONSTRAINT_TYPE, rbA,rbB),m_rbAFrame(rbAFrame),m_rbBFrame(rbBFrame),
   --  #ifdef _BT_USE_CENTER_LIMIT_
   --  m_limit(),
   --  #endif
   --  m_angularOnly(false),
   --  m_enableAngularMotor(false),
   --  m_useSolveConstraintObsolete(HINGE_USE_OBSOLETE_SOLVER),
   --  m_useOffsetForConstraintFrame(HINGE_USE_FRAME_OFFSET),
   --  m_useReferenceFrameA(useReferenceFrameA),
   --  m_flags(0)
   --  {
   --  #ifndef        _BT_USE_CENTER_LIMIT_
   --          //start with free
   --          m_lowerLimit = impact.d3.Scalar(1.0f);
   --          m_upperLimit = impact.d3.Scalar(-1.0f);
   --          m_biasFactor = 0.3f;
   --          m_relaxationFactor = 1.0f;
   --          m_limitSoftness = 0.9f;
   --          m_solveLimit = false;
   --  #endif
   --          m_referenceSign = m_useReferenceFrameA ? impact.d3.Scalar(-1.f) : impact.d3.Scalar(1.f);
   --  }

   function to_hinge_Joint (rbA                : impact.d3.Object.rigid.view;
                            rbAFrame           : Transform_3d;
                            useReferenceFrameA : Boolean := False) return Item
   is
      pragma Unreferenced (rbAFrame);
      use linear_Algebra_3d, impact.d3.Transform;
      Self : Item;
   begin
      Self.define (impact.d3.Joint.HINGE_CONSTRAINT_TYPE,  rbA);

      Self.m_angularOnly                 := False;
      Self.m_enableAngularMotor          := False;
      Self.m_useSolveConstraintObsolete  := HINGE_USE_OBSOLETE_SOLVER;
      Self.m_useOffsetForConstraintFrame := HINGE_USE_FRAME_OFFSET;
      Self.m_useReferenceFrameA          := useReferenceFrameA;
      Self.m_flags                       := 0;

      setOrigin (Self.m_rbBFrame, getRigidBodyA (Self).getCenterOfMassTransform * getOrigin (Self.m_rbAFrame));

      Self.m_lowerLimit := 1.0;
      Self.m_upperLimit := -1.0;
      Self.m_biasFactor := 0.3;
      Self.m_relaxationFactor := 1.0;
      Self.m_limitSoftness := 0.9;
      Self.m_solveLimit := False;
      Self.m_referenceSign := (if Self.m_useReferenceFrameA then -1.0 else 1.0);

      return Self;
   end to_hinge_Joint;

   --  impact.d3.Joint.hinge::impact.d3.Joint.hinge(impact.d3.Object.rigid& rbA, const impact.d3.Transform& rbAFrame, bool useReferenceFrameA)
   --  :impact.d3.Joint(HINGE_CONSTRAINT_TYPE, rbA),m_rbAFrame(rbAFrame),m_rbBFrame(rbAFrame),
   --  #ifdef _BT_USE_CENTER_LIMIT_
   --  m_limit(),
   --  #endif
   --  m_angularOnly(false),
   --  m_enableAngularMotor(false),
   --  m_useSolveConstraintObsolete(HINGE_USE_OBSOLETE_SOLVER),
   --  m_useOffsetForConstraintFrame(HINGE_USE_FRAME_OFFSET),
   --  m_useReferenceFrameA(useReferenceFrameA),
   --  m_flags(0)
   --  {
   --          ///not providing rigidbody B means implicitly using worldspace for body B
   --
   --          m_rbBFrame.getOrigin() = m_rbA.getCenterOfMassTransform()(m_rbAFrame.getOrigin());
   --  #ifndef        _BT_USE_CENTER_LIMIT_
   --          //start with free
   --          m_lowerLimit = impact.d3.Scalar(1.0f);
   --          m_upperLimit = impact.d3.Scalar(-1.0f);
   --          m_biasFactor = 0.3f;
   --          m_relaxationFactor = 1.0f;
   --          m_limitSoftness = 0.9f;
   --          m_solveLimit = false;
   --  #endif
   --          m_referenceSign = m_useReferenceFrameA ? impact.d3.Scalar(-1.f) : impact.d3.Scalar(1.f);
   --  }





   overriding procedure setParam (Self  :    out Item;
                       num   : in     impact.d3.Joint.btConstraintParams;
                       value : in     math.Real;
                       axis  : in     Integer := -1)
   is
      use impact.d3.Joint;
   begin
      if axis = -1 or axis = 5 then
         case num is
            when BT_CONSTRAINT_STOP_ERP =>
               Self.m_stopERP := value;
               Self.m_flags := Self.m_flags or to_Flag (BT_HINGE_FLAGS_ERP_STOP);
            when BT_CONSTRAINT_STOP_CFM =>
               Self.m_stopCFM := value;
               Self.m_flags := Self.m_flags or to_Flag (BT_HINGE_FLAGS_CFM_STOP);
            when BT_CONSTRAINT_CFM =>
               Self.m_normalCFM := value;
               Self.m_flags := Self.m_flags or to_Flag (BT_HINGE_FLAGS_CFM_NORM);
            when others => null;
         end case;
      else
         null;
      end if;
   end setParam;

   --  ///override the default global value of a parameter (such as ERP or CFM), optionally provide the axis (0..5).
   --  ///If no axis is provided, it uses the default axis for this constraint.
   --  void impact.d3.Joint.hinge::setParam(int num, impact.d3.Scalar value, int axis)
   --  {
   --          if((axis == -1) || (axis == 5))
   --          {
   --                  switch(num)
   --                  {
   --                          case BT_CONSTRAINT_STOP_ERP :
   --                                  m_stopERP = value;
   --                                  m_flags |= BT_HINGE_FLAGS_ERP_STOP;
   --                                  break;
   --                          case BT_CONSTRAINT_STOP_CFM :
   --                                  m_stopCFM = value;
   --                                  m_flags |= BT_HINGE_FLAGS_CFM_STOP;
   --                                  break;
   --                          case BT_CONSTRAINT_CFM :
   --                                  m_normalCFM = value;
   --                                  m_flags |= BT_HINGE_FLAGS_CFM_NORM;
   --                                  break;
   --                          default :
   --                                  btAssertConstrParams(0);
   --                  }
   --          }
   --          else
   --          {
   --                  btAssertConstrParams(0);
   --          }
   --  }

   overriding function  getParam (Self : in Item;
                       num  : in impact.d3.Joint.btConstraintParams;
                       axis : in Integer := -1) return math.Real
   is
      use impact.d3.Joint;
      retVal : Real := 0.0;
   begin
      if axis = -1 or axis = 5 then
         case num is
            when BT_CONSTRAINT_STOP_ERP =>
               retVal := Self.m_stopERP;
            when BT_CONSTRAINT_STOP_CFM =>
               retVal := Self.m_stopCFM;
            when BT_CONSTRAINT_CFM =>
               retVal := Self.m_normalCFM;
            when others =>
               retVal := 0.0;
         end case;
      else
         retVal := 0.0;
      end if;
      return retVal;
   end getParam;

   --  ///return the local value of parameter
   --  impact.d3.Scalar impact.d3.Joint.hinge::getParam(int num, int axis) const
   --  {
   --          impact.d3.Scalar retVal = 0;
   --          if((axis == -1) || (axis == 5))
   --          {
   --                  switch(num)
   --                  {
   --                          case BT_CONSTRAINT_STOP_ERP :
   --                                  btAssertConstrParams(m_flags & BT_HINGE_FLAGS_ERP_STOP);
   --                                  retVal = m_stopERP;
   --                                  break;
   --                          case BT_CONSTRAINT_STOP_CFM :
   --                                  btAssertConstrParams(m_flags & BT_HINGE_FLAGS_CFM_STOP);
   --                                  retVal = m_stopCFM;
   --                                  break;
   --                          case BT_CONSTRAINT_CFM :
   --                                  btAssertConstrParams(m_flags & BT_HINGE_FLAGS_CFM_NORM);
   --                                  retVal = m_normalCFM;
   --                                  break;
   --                          default :
   --                                  btAssertConstrParams(0);
   --                  }
   --          }
   --          else
   --          {
   --                  btAssertConstrParams(0);
   --          }
   --          return retVal;
   --  }

   overriding procedure getInfo1 (Self : in out Item;
                       info :    out impact.d3.Joint.btConstraintInfo1)
   is
      use impact.d3.Object.rigid;
   begin
      if Self.m_useSolveConstraintObsolete then
         info.m_numConstraintRows := 0;
         info.nub := 0;
      else
         info.m_numConstraintRows := 5;
         info.nub := 1;
         testLimit (Self, Self.getRigidBodyA.getCenterOfMassTransform,
                          Self.getRigidBodyB.getCenterOfMassTransform);
         if Self.getSolveLimit or Self.getEnableAngularMotor then
            info.m_numConstraintRows := info.m_numConstraintRows + 1;
            info.nub := info.nub - 1;
         end if;
      end if;
   end getInfo1;

   --  void impact.d3.Joint.hinge::getInfo1(btConstraintInfo1* info)
   --  {
   --          if (m_useSolveConstraintObsolete)
   --          {
   --                  info->m_numConstraintRows = 0;
   --                  info->nub = 0;
   --          }
   --          else
   --          {
   --                  info->m_numConstraintRows = 5; // Fixed 3 linear + 2 angular
   --                  info->nub = 1;
   --                  //always add the row, to avoid computation (data is not available yet)
   --                  //prepare constraint
   --                  testLimit(m_rbA.getCenterOfMassTransform(),m_rbB.getCenterOfMassTransform());
   --                  if(getSolveLimit() || getEnableAngularMotor())
   --                  {
   --                          info->m_numConstraintRows++; // limit 3rd anguar as well
   --                          info->nub--;
   --                  }
   --
   --          }
   --  }


   procedure getInfo1NonVirtual (Self : in     Item;
                                 info :    out impact.d3.Joint.btConstraintInfo1)
   is
   begin
      if Self.m_useSolveConstraintObsolete then
         info.m_numConstraintRows := 0;
         info.nub := 0;
      else
         info.m_numConstraintRows := 6;
         info.nub := 0;
      end if;
   end getInfo1NonVirtual;

   --  void impact.d3.Joint.hinge::getInfo1NonVirtual(btConstraintInfo1* info)
   --  {
   --          if (m_useSolveConstraintObsolete)
   --          {
   --                  info->m_numConstraintRows = 0;
   --                  info->nub = 0;
   --          }
   --          else
   --          {
   --                  //always add the 'limit' row, to avoid computation (data is not available yet)
   --                  info->m_numConstraintRows = 6; // Fixed 3 linear + 2 angular
   --                  info->nub = 0;
   --          }
   --  }

   overriding procedure getInfo2 (Self : in out Item;
                       info :    out impact.d3.Joint.btConstraintInfo2)
   is
      use impact.d3.Object.rigid;
   begin
      if Self.m_useOffsetForConstraintFrame then
         Self.getInfo2InternalUsingFrameOffset (info,
                                               Self.getRigidBodyA.getCenterOfMassTransform,
                                               Self.getRigidBodyB.getCenterOfMassTransform,
                                               Self.getRigidBodyA.getAngularVelocity,
                                               Self.getRigidBodyB.getAngularVelocity           );
      else
         Self.getInfo2Internal (info,
                               Self.getRigidBodyA.getCenterOfMassTransform,
                               Self.getRigidBodyB.getCenterOfMassTransform,
                               Self.getRigidBodyA.getAngularVelocity,
                               Self.getRigidBodyB.getAngularVelocity           );
      end if;
   end getInfo2;

   --  void impact.d3.Joint.hinge::getInfo2 (btConstraintInfo2* info)
   --  {
   --          if(m_useOffsetForConstraintFrame)
   --          {
   --                  getInfo2InternalUsingFrameOffset(info, m_rbA.getCenterOfMassTransform(),m_rbB.getCenterOfMassTransform(),m_rbA.getAngularVelocity(),m_rbB.getAngularVelocity());
   --          }
   --          else
   --          {
   --                  getInfo2Internal(info, m_rbA.getCenterOfMassTransform(),m_rbB.getCenterOfMassTransform(),m_rbA.getAngularVelocity(),m_rbB.getAngularVelocity());
   --          }
   --  }

   procedure getInfo2NonVirtual (Self    : in out Item;
                                 info    :    out impact.d3.Joint.btConstraintInfo2;
                                 transA  : in     Transform_3d;
                                 transB  : in     Transform_3d;
                                 angVelA : in     Vector_3;
                                 angVelB : in     Vector_3)
   is
   begin
      testLimit (Self, transA, transB);
      getInfo2Internal (Self, info, transA, transB, angVelA, angVelB);
   end getInfo2NonVirtual;

   --  void        impact.d3.Joint.hinge::getInfo2NonVirtual (btConstraintInfo2* info,const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB)
   --  {
   --          ///the regular (virtual) implementation getInfo2 already performs 'testLimit' during getInfo1, so we need to do it now
   --          testLimit(transA,transB);
   --
   --          getInfo2Internal(info,transA,transB,angVelA,angVelB);
   --  }
   --
   --

   procedure getInfo2Internal (Self    : in out Item;
                               info    :    out impact.d3.Joint.btConstraintInfo2;
                               transA  : in     Transform_3d;
                               transB  : in     Transform_3d;
                               angVelA : in     Vector_3;
                               angVelB : in     Vector_3)
   is
      use linear_Algebra_3d,
          impact.d3.Transform, impact.d3.Containers, impact.d3.Containers.Real_pointers;
      use math.Vectors;
      use impact.d3.Scalar;
      use type C.ptrdiff_t;

      skip      : constant C.ptrdiff_t   := info.rowskip;
      trA       : constant Transform_3d     := transA * Self.m_rbAFrame;
      trB       : constant Transform_3d     := transB * Self.m_rbBFrame;
      pivotAinW : math.Vector_3 := getOrigin (trA);
      pivotBinW : math.Vector_3 := getOrigin (trB);
      k         : Real        := info.fps * info.erp;
   begin
      if not Self.m_angularOnly then
         Real_pointer (info.m_J1linearAxis + 0 * skip + 0).all := 1.0;
         Real_pointer (info.m_J1linearAxis + 1 * skip + 1).all := 1.0;
         Real_pointer (info.m_J1linearAxis + 2 * skip + 2).all := 1.0;
      end if;

      declare
         a1       : constant math.Vector_3 := pivotAinW - getOrigin (transA);
         angular0 : math.Vector_3 := math.Vector_3 (Value (info.m_J1angularAxis + 0 * skip));
         angular1 : math.Vector_3 := math.Vector_3 (Value (info.m_J1angularAxis + 1 * skip));
         angular2 : math.Vector_3 := math.Vector_3 (Value (info.m_J1angularAxis + 2 * skip));
         a1neg    : constant math.Vector_3 := -a1;
      begin
         getSkewSymmetricMatrix (a1neg, angular0, angular1, angular2);
      end;

      declare
         a2       : constant math.Vector_3 := pivotBinW - getOrigin (transB);
         angular0 : math.Vector_3 := math.Vector_3 (Value (info.m_J2angularAxis + 0 * skip));
         angular1 : math.Vector_3 := math.Vector_3 (Value (info.m_J2angularAxis + 1 * skip));
         angular2 : math.Vector_3 := math.Vector_3 (Value (info.m_J2angularAxis + 2 * skip));
      begin
         getSkewSymmetricMatrix (a2, angular0, angular1, angular2);
      end;

      if not Self.m_angularOnly then
         for I in C.ptrdiff_t'(0) .. 2 loop
            Real_pointer (info.m_constraintError + I * skip).all := k * (pivotBinW (Integer (I + 1)) - pivotAinW (Integer (I + 1)));
         end loop;
      end if;

      declare
         ax1       : math.Vector_3 := Col (getBasis (trA), 3);
         ax2       : constant math.Vector_3 := Col (getBasis (trB), 3);
         u         : constant math.Vector_3 := cross (ax1, ax2);
         p         : math.Vector_3 := Col (getBasis (trA), 1);
         q         : math.Vector_3 := Col (getBasis (trA), 2);
         s3        : constant C.ptrdiff_t  := 3 * info.rowskip;
         s4        : constant C.ptrdiff_t  := 4 * info.rowskip;
         nrow      : C.ptrdiff_t  := 4;
         srow      : C.ptrdiff_t;
         limit     : Integer  := 0;
         limit_err : Real   := 0.0;
         powered   : Integer  := 0;
         lostop    : constant Integer  := Integer (Self.getLowerLimit);
         histop    : constant Integer  := Integer (Self.getUpperLimit);
         currERP   : constant Real   := (if (Self.m_flags and to_Flag (BT_HINGE_FLAGS_ERP_STOP)) /= 0 then Self.m_stopERP else info.erp);
         mot_fact  : constant math.Real := Self.getMotorFactor (Self.m_hingeAngle, math.Real (lostop), math.Real (histop), Self.m_motorTargetVelocity, info.fps * currERP);
         bounce    : constant Real   := getRelaxationFactor (Self.m_limit);
         vel       : Real   := dot (angVelA, ax1);
         newc      : constant Real   := -bounce * vel;
      begin
         Real_pointer (info.m_J1angularAxis + s3 + 0).all := p (1);
         Real_pointer (info.m_J1angularAxis + s3 + 1).all := p (2);
         Real_pointer (info.m_J1angularAxis + s3 + 2).all := p (3);
         Real_pointer (info.m_J1angularAxis + s4 + 0).all := q (1);
         Real_pointer (info.m_J1angularAxis + s4 + 1).all := q (2);
         Real_pointer (info.m_J1angularAxis + s4 + 2).all := q (3);

         Real_pointer (info.m_J2angularAxis + s3 + 0).all := -p (1);
         Real_pointer (info.m_J2angularAxis + s3 + 1).all := -p (2);
         Real_pointer (info.m_J2angularAxis + s3 + 2).all := -p (3);
         Real_pointer (info.m_J2angularAxis + s4 + 0).all := -q (1);
         Real_pointer (info.m_J2angularAxis + s4 + 1).all := -q (2);
         Real_pointer (info.m_J2angularAxis + s4 + 2).all := -q (3);

         Real_pointer (info.m_constraintError + s3).all   := k * dot (u, p);

         if Self.getSolveLimit then
            limit_err := getCorrection (Self.m_limit) * Self.m_referenceSign;
         end if;

         limit := (if limit_err > 0.0 then 1 else 2);

         if Self.getEnableAngularMotor then
            powered := 1;
         end if;

         if limit /= 0 or powered /= 0 then
            nrow := nrow + 1;
            srow := nrow * info.rowskip;
            Real_pointer (info.m_J1angularAxis + srow + 0).all := ax1 (1);
            Real_pointer (info.m_J1angularAxis + srow + 1).all := ax1 (2);
            Real_pointer (info.m_J1angularAxis + srow + 2).all := ax1 (3);

            Real_pointer (info.m_J2angularAxis + srow + 0).all := -ax1 (1);
            Real_pointer (info.m_J2angularAxis + srow + 1).all := -ax1 (2);
            Real_pointer (info.m_J2angularAxis + srow + 3).all := -ax1 (3);



            if limit /= 0 and (lostop = histop) then
               powered := 0;
            end if;

            if powered /= 0 then
               if (Self.m_flags and to_Flag (BT_HINGE_FLAGS_CFM_NORM)) /= 0 then
                  Real_pointer (info.cfm + srow).all := Self.m_normalCFM;
               end if;
               Real_pointer (info.m_constraintError + srow).all := mot_fact * Self.m_motorTargetVelocity * Self.m_referenceSign;
               Real_pointer (info.m_lowerLimit + srow).all := -Self.m_maxMotorImpulse;
               Real_pointer (info.m_upperLimit + srow).all :=   Self.m_maxMotorImpulse;
            end if;

            if limit /= 0 then
               k := info.fps * currERP;
               Real_pointer (info.m_constraintError + srow).all := Real_pointer (info.m_constraintError + srow).all + k * limit_err;

               if (Self.m_flags and to_Flag (BT_HINGE_FLAGS_CFM_STOP)) /= 0 then
                  Real_pointer (info.cfm + srow).all := Self.m_stopCFM;
               end if;

               if lostop = histop then
                  Real_pointer (info.m_lowerLimit + srow).all := -SIMD_INFINITY;
                  Real_pointer (info.m_upperLimit + srow).all :=  SIMD_INFINITY;
               elsif limit = 1 then
                  Real_pointer (info.m_lowerLimit +  srow).all := 0.0;
                  Real_pointer (info.m_upperLimit +  srow).all := SIMD_INFINITY;
               else
                  Real_pointer (info.m_lowerLimit +  srow).all := -SIMD_INFINITY;
                  Real_pointer (info.m_upperLimit + srow).all := 0.0;
               end if;

               if bounce > 0.0 then
                  vel := vel - dot (angVelB, ax1);
                  if limit = 1 then
                     if vel < 0.0 then
                        if newc > Real_pointer (info.m_constraintError + srow).all then
                           Real_pointer (info.m_constraintError + srow).all := newc;
                        end if;
                     end if;
                  else
                     if vel > 0.0 then
                        if newc < Real_pointer (info.m_constraintError + srow).all then
                           Real_pointer (info.m_constraintError + srow).all := newc;
                        end if;
                     end if;
                  end if;
               end if;

            end if;
         end if;
      end;
   end getInfo2Internal;

   --  void impact.d3.Joint.hinge::getInfo2Internal(btConstraintInfo2* info, const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB)
   --  {
   --
   --          btAssert(!m_useSolveConstraintObsolete);
   --          int i, skip = info->rowskip;
   --          // transforms in world space
   --          impact.d3.Transform trA = transA*m_rbAFrame;
   --          impact.d3.Transform trB = transB*m_rbBFrame;
   --          // pivot point
   --          impact.d3.Vector pivotAInW = trA.getOrigin();
   --          impact.d3.Vector pivotBInW = trB.getOrigin();
   --  #if 0
   --          if (0)
   --          {
   --                  for (i=0;i<6;i++)
   --                  {
   --                          info->m_J1linearAxis[i*skip]=0;
   --                          info->m_J1linearAxis[i*skip+1]=0;
   --                          info->m_J1linearAxis[i*skip+2]=0;
   --
   --                          info->m_J1angularAxis[i*skip]=0;
   --                          info->m_J1angularAxis[i*skip+1]=0;
   --                          info->m_J1angularAxis[i*skip+2]=0;
   --
   --                          info->m_J2angularAxis[i*skip]=0;
   --                          info->m_J2angularAxis[i*skip+1]=0;
   --                          info->m_J2angularAxis[i*skip+2]=0;
   --
   --                          info->m_constraintError[i*skip]=0.f;
   --                  }
   --          }
   --  #endif //#if 0
   --          // linear (all fixed)
   --
   --          if (!m_angularOnly)
   --          {
   --                  info->m_J1linearAxis[0] = 1;
   --                  info->m_J1linearAxis[skip + 1] = 1;
   --                  info->m_J1linearAxis[2 * skip + 2] = 1;
   --          }
   --
   --
   --
   --
   --          impact.d3.Vector a1 = pivotAInW - transA.getOrigin();
   --          {
   --                  impact.d3.Vector* angular0 = (impact.d3.Vector*)(info->m_J1angularAxis);
   --                  impact.d3.Vector* angular1 = (impact.d3.Vector*)(info->m_J1angularAxis + skip);
   --                  impact.d3.Vector* angular2 = (impact.d3.Vector*)(info->m_J1angularAxis + 2 * skip);
   --                  impact.d3.Vector a1neg = -a1;
   --                  a1neg.getSkewSymmetricMatrix(angular0,angular1,angular2);
   --          }
   --          impact.d3.Vector a2 = pivotBInW - transB.getOrigin();
   --          {
   --                  impact.d3.Vector* angular0 = (impact.d3.Vector*)(info->m_J2angularAxis);
   --                  impact.d3.Vector* angular1 = (impact.d3.Vector*)(info->m_J2angularAxis + skip);
   --                  impact.d3.Vector* angular2 = (impact.d3.Vector*)(info->m_J2angularAxis + 2 * skip);
   --                  a2.getSkewSymmetricMatrix(angular0,angular1,angular2);
   --          }
   --          // linear RHS
   --      impact.d3.Scalar k = info->fps * info->erp;
   --          if (!m_angularOnly)
   --          {
   --                  for(i = 0; i < 3; i++)
   --                  {
   --                          info->m_constraintError[i * skip] = k * (pivotBInW[i] - pivotAInW[i]);
   --                  }
   --          }
   --          // make rotations around X and Y equal
   --          // the hinge axis should be the only unconstrained
   --          // rotational axis, the angular velocity of the two bodies perpendicular to
   --          // the hinge axis should be equal. thus the constraint equations are
   --          //    p*w1 - p*w2 = 0
   --          //    q*w1 - q*w2 = 0
   --          // where p and q are unit vectors normal to the hinge axis, and w1 and w2
   --          // are the angular velocity vectors of the two bodies.
   --          // get hinge axis (Z)
   --          impact.d3.Vector ax1 = trA.getBasis().getColumn(2);
   --          // get 2 orthos to hinge axis (X, Y)
   --          impact.d3.Vector p = trA.getBasis().getColumn(0);
   --          impact.d3.Vector q = trA.getBasis().getColumn(1);
   --          // set the two hinge angular rows
   --      int s3 = 3 * info->rowskip;
   --      int s4 = 4 * info->rowskip;
   --
   --          info->m_J1angularAxis[s3 + 0] = p[0];
   --          info->m_J1angularAxis[s3 + 1] = p[1];
   --          info->m_J1angularAxis[s3 + 2] = p[2];
   --          info->m_J1angularAxis[s4 + 0] = q[0];
   --          info->m_J1angularAxis[s4 + 1] = q[1];
   --          info->m_J1angularAxis[s4 + 2] = q[2];
   --
   --          info->m_J2angularAxis[s3 + 0] = -p[0];
   --          info->m_J2angularAxis[s3 + 1] = -p[1];
   --          info->m_J2angularAxis[s3 + 2] = -p[2];
   --          info->m_J2angularAxis[s4 + 0] = -q[0];
   --          info->m_J2angularAxis[s4 + 1] = -q[1];
   --          info->m_J2angularAxis[s4 + 2] = -q[2];
   --      // compute the right hand side of the constraint equation. set relative
   --      // body velocities along p and q to bring the hinge back into alignment.
   --      // if ax1,ax2 are the unit length hinge axes as computed from body1 and
   --      // body2, we need to rotate both bodies along the axis u = (ax1 x ax2).
   --      // if `theta' is the angle between ax1 and ax2, we need an angular velocity
   --      // along u to cover angle erp*theta in one step :
   --      //   |angular_velocity| = angle/time = erp*theta / stepsize
   --      //                      = (erp*fps) * theta
   --      //    angular_velocity  = |angular_velocity| * (ax1 x ax2) / |ax1 x ax2|
   --      //                      = (erp*fps) * theta * (ax1 x ax2) / sin(theta)
   --      // ...as ax1 and ax2 are unit length. if theta is smallish,
   --      // theta ~= sin(theta), so
   --      //    angular_velocity  = (erp*fps) * (ax1 x ax2)
   --      // ax1 x ax2 is in the plane space of ax1, so we project the angular
   --      // velocity to p and q to find the right hand side.
   --      impact.d3.Vector ax2 = trB.getBasis().getColumn(2);
   --          impact.d3.Vector u = ax1.cross(ax2);
   --          info->m_constraintError[s3] = k * u.dot(p);
   --          info->m_constraintError[s4] = k * u.dot(q);
   --          // check angular limits
   --          int nrow = 4; // last filled row
   --          int srow;
   --          impact.d3.Scalar limit_err = impact.d3.Scalar(0.0);
   --          int limit = 0;
   --          if(getSolveLimit())
   --          {
   --  #ifdef        _BT_USE_CENTER_LIMIT_
   --          limit_err = m_limit.getCorrection() * m_referenceSign;
   --  #else
   --          limit_err = m_correction * m_referenceSign;
   --  #endif
   --          limit = (limit_err > impact.d3.Scalar(0.0)) ? 1 : 2;
   --
   --          }
   --          // if the hinge has joint limits or motor, add in the extra row
   --          int powered = 0;
   --          if(getEnableAngularMotor())
   --          {
   --                  powered = 1;
   --          }
   --          if(limit || powered)
   --          {
   --                  nrow++;
   --                  srow = nrow * info->rowskip;
   --                  info->m_J1angularAxis[srow+0] = ax1[0];
   --                  info->m_J1angularAxis[srow+1] = ax1[1];
   --                  info->m_J1angularAxis[srow+2] = ax1[2];
   --
   --                  info->m_J2angularAxis[srow+0] = -ax1[0];
   --                  info->m_J2angularAxis[srow+1] = -ax1[1];
   --                  info->m_J2angularAxis[srow+2] = -ax1[2];
   --
   --                  impact.d3.Scalar lostop = getLowerLimit();
   --                  impact.d3.Scalar histop = getUpperLimit();
   --                  if(limit && (lostop == histop))
   --                  {  // the joint motor is ineffective
   --                          powered = 0;
   --                  }
   --                  info->m_constraintError[srow] = impact.d3.Scalar(0.0f);
   --                  impact.d3.Scalar currERP = (m_flags & BT_HINGE_FLAGS_ERP_STOP) ? m_stopERP : info->erp;
   --                  if(powered)
   --                  {
   --                          if(m_flags & BT_HINGE_FLAGS_CFM_NORM)
   --                          {
   --                                  info->cfm[srow] = m_normalCFM;
   --                          }
   --                          impact.d3.Scalar mot_fact = getMotorFactor(m_hingeAngle, lostop, histop, m_motorTargetVelocity, info->fps * currERP);
   --                          info->m_constraintError[srow] += mot_fact * m_motorTargetVelocity * m_referenceSign;
   --                          info->m_lowerLimit[srow] = - m_maxMotorImpulse;
   --                          info->m_upperLimit[srow] =   m_maxMotorImpulse;
   --                  }
   --                  if(limit)
   --                  {
   --                          k = info->fps * currERP;
   --                          info->m_constraintError[srow] += k * limit_err;
   --                          if(m_flags & BT_HINGE_FLAGS_CFM_STOP)
   --                          {
   --                                  info->cfm[srow] = m_stopCFM;
   --                          }
   --                          if(lostop == histop)
   --                          {
   --                                  // limited low and high simultaneously
   --                                  info->m_lowerLimit[srow] = -SIMD_INFINITY;
   --                                  info->m_upperLimit[srow] = SIMD_INFINITY;
   --                          }
   --                          else if(limit == 1)
   --                          { // low limit
   --                                  info->m_lowerLimit[srow] = 0;
   --                                  info->m_upperLimit[srow] = SIMD_INFINITY;
   --                          }
   --                          else
   --                          { // high limit
   --                                  info->m_lowerLimit[srow] = -SIMD_INFINITY;
   --                                  info->m_upperLimit[srow] = 0;
   --                          }
   --                          // bounce (we'll use slider parameter abs(1.0 - m_dampingLimAng) for that)
   --  #ifdef        _BT_USE_CENTER_LIMIT_
   --                          impact.d3.Scalar bounce = m_limit.getRelaxationFactor();
   --  #else
   --                          impact.d3.Scalar bounce = m_relaxationFactor;
   --  #endif
   --                          if(bounce > impact.d3.Scalar(0.0))
   --                          {
   --                                  impact.d3.Scalar vel = angVelA.dot(ax1);
   --                                  vel -= angVelB.dot(ax1);
   --                                  // only apply bounce if the velocity is incoming, and if the
   --                                  // resulting c[] exceeds what we already have.
   --                                  if(limit == 1)
   --                                  {        // low limit
   --                                          if(vel < 0)
   --                                          {
   --                                                  impact.d3.Scalar newc = -bounce * vel;
   --                                                  if(newc > info->m_constraintError[srow])
   --                                                  {
   --                                                          info->m_constraintError[srow] = newc;
   --                                                  }
   --                                          }
   --                                  }
   --                                  else
   --                                  {        // high limit - all those computations are reversed
   --                                          if(vel > 0)
   --                                          {
   --                                                  impact.d3.Scalar newc = -bounce * vel;
   --                                                  if(newc < info->m_constraintError[srow])
   --                                                  {
   --                                                          info->m_constraintError[srow] = newc;
   --                                                  }
   --                                          }
   --                                  }
   --                          }
   --  #ifdef        _BT_USE_CENTER_LIMIT_
   --                          info->m_constraintError[srow] *= m_limit.getBiasFactor();
   --  #else
   --                          info->m_constraintError[srow] *= m_biasFactor;
   --  #endif
   --                  } // if(limit)
   --          } // if angular limit or powered
   --  }





   procedure getInfo2InternalUsingFrameOffset (Self    : in out Item;
                                               info    :    out impact.d3.Joint.btConstraintInfo2;
                                               transA  : in     Transform_3d;
                                               transB  : in     Transform_3d;
                                               angVelA : in     Vector_3;
                                               angVelB : in     Vector_3)
   is
   begin
      null;
   end getInfo2InternalUsingFrameOffset;





   procedure updateRHS (Self     : in out Item;
                        timeStep : in     Real)
   is
   begin
      null;
   end updateRHS;

   --  void        impact.d3.Joint.hinge::updateRHS(impact.d3.Scalar        timeStep)
   --  {
   --          (void)timeStep;
   --
   --  }





   overriding procedure buildJacobian (Self : in out Item)
   is
      use math.Vectors, impact.d3.Transform;
      use impact.d3.jacobian_Entry;

      jointAxis0local : Vector_3;
      jointAxis1local : Vector_3;
   begin
      if Self.m_useSolveConstraintObsolete then
         --  Self.m_appliedImpulse := 0.0;
         Self.m_accMotorImpulse := 0.0;
         if not Self.m_angularOnly then
            declare
               use linear_Algebra_3d, impact.d3.Transform;
               pivotAinW : constant Vector_3 := getRigidBodyA (Self).getCenterOfMassTransform * getOrigin (Self.m_rbAFrame);
               pivotBinW : constant Vector_3 := getRigidBodyB (Self).getCenterOfMassTransform * getOrigin (Self.m_rbBFrame);
               relPos    : constant Vector_3 := pivotBinW - pivotAinW;
               normal    : array (1 .. 3) of Vector_3;
            begin
               if length2 (relPos) > impact.d3.Scalar.SIMD_EPSILON then
                  normal (1) := normalized (relPos);
               else
                  normal (1) := (1.0, 0.0, 0.0);
               end if;

               btPlaneSpace1 (normal (1), normal (2), normal (3));

               for I in 1 .. 3 loop
                  Self.m_jac (I) := to_jacobian_Entry (transpose (getBasis (getRigidBodyA (Self).getCenterOfMassTransform)),
                                                        transpose (getBasis (getRigidBodyB (Self).getCenterOfMassTransform)),
                                                        pivotAInW - getRigidBodyA (Self).getCenterOfMassPosition,
                                                        pivotBInW - getRigidBodyB (Self).getCenterOfMassPosition,
                                                        normal (I),
                                                        getRigidBodyA (Self).getInvInertiaDiagLocal,
                                                        getRigidBodyA (Self).getInvMass,
                                                        getRigidBodyB (Self).getInvInertiaDiagLocal,
                                                        getRigidBodyB (Self).getInvMass);
               end loop;
            end;
         end if;

         btPlaneSpace1 (math.Col (getBasis (Self.m_rbAFrame), 3), jointAxis0local, jointAxis1local);

         declare
            jointAxis0     : constant Vector_3 := getBasis (getRigidBodyA (Self).getCenterOfMassTransform) * jointAxis0local;
            jointAxis1     : constant Vector_3 := getBasis (getRigidBodyA (Self).getCenterOfMassTransform) * jointAxis1local;
            hingeAxisWorld : constant Vector_3 := getBasis (getRigidBodyA (Self).getCenterOfMassTransform) * math.Col (getBasis (Self.m_rbAFrame), 3);
            axisA          : constant Vector_3 := getBasis (getRigidBodyA (Self).getCenterOfMassTransform) * math.Col (getBasis (Self.m_rbAFrame), 3);
         begin
            Self.m_jacAng (1) := to_jacobian_Entry (jointAxis0,
                                                     transpose (getBasis (getRigidBodyA (Self).getCenterOfMassTransform)),
                                                     transpose (getBasis (getRigidBodyB (Self).getCenterOfMassTransform)),
                                                     getRigidBodyA (Self).getInvInertiaDiagLocal,
                                                     getRigidBodyB (Self).getInvInertiaDiagLocal);

            Self.m_jacAng (2) := to_jacobian_Entry (jointAxis1,
                                                     transpose (getBasis (getRigidBodyA (Self).getCenterOfMassTransform)),
                                                     transpose (getBasis (getRigidBodyB (Self).getCenterOfMassTransform)),
                                                     getRigidBodyA (Self).getInvInertiaDiagLocal,
                                                     getRigidBodyB (Self).getInvInertiaDiagLocal);

            Self.m_jacAng (3) := to_jacobian_Entry (hingeAxisWorld,
                                                     transpose (getBasis (getRigidBodyA (Self).getCenterOfMassTransform)),
                                                     transpose (getBasis (getRigidBodyB (Self).getCenterOfMassTransform)),
                                                     getRigidBodyA (Self).getInvInertiaDiagLocal,
                                                     getRigidBodyB (Self).getInvInertiaDiagLocal);
            Self.m_accLimitImpulse := 0.0;
            testLimit (Self, getRigidBodyA (Self).getCenterOfMassTransform, getRigidBodyB (Self).getCenterOfMassTransform);
            Self.m_kHinge := 1.0 / (getRigidBodyA (Self).computeAngularImpulseDenominator (axisA) +
                                    getRigidBodyB (Self).computeAngularImpulseDenominator (axisA));
         end;
      end if;
      null;
   end buildJacobian;

   --  void        impact.d3.Joint.hinge::buildJacobian()
   --  {
   --          if (m_useSolveConstraintObsolete)
   --          {
   --                  m_appliedImpulse = impact.d3.Scalar(0.);
   --                  m_accMotorImpulse = impact.d3.Scalar(0.);
   --
   --                  if (!m_angularOnly)
   --                  {
   --                          impact.d3.Vector pivotAInW = m_rbA.getCenterOfMassTransform()*m_rbAFrame.getOrigin();
   --                          impact.d3.Vector pivotBInW = m_rbB.getCenterOfMassTransform()*m_rbBFrame.getOrigin();
   --                          impact.d3.Vector relPos = pivotBInW - pivotAInW;
   --
   --                          impact.d3.Vector normal[3];
   --                          if (relPos.length2() > SIMD_EPSILON)
   --                          {
   --                                  normal[0] = relPos.normalized();
   --                          }
   --                          else
   --                          {
   --                                  normal[0].setValue(impact.d3.Scalar(1.0),0,0);
   --                          }
   --
   --                          btPlaneSpace1(normal[0], normal[1], normal[2]);
   --
   --                          for (int i=0;i<3;i++)
   --                          {
   --                                  new (&m_jac[i]) impact.d3.jacobian_Entry(
   --                                  m_rbA.getCenterOfMassTransform().getBasis().transpose(),
   --                                  m_rbB.getCenterOfMassTransform().getBasis().transpose(),
   --                                  pivotAInW - m_rbA.getCenterOfMassPosition(),
   --                                  pivotBInW - m_rbB.getCenterOfMassPosition(),
   --                                  normal[i],
   --                                  m_rbA.getInvInertiaDiagLocal(),
   --                                  m_rbA.getInvMass(),
   --                                  m_rbB.getInvInertiaDiagLocal(),
   --                                  m_rbB.getInvMass());
   --                          }
   --                  }
   --
   --                  //calculate two perpendicular jointAxis, orthogonal to hingeAxis
   --                  //these two jointAxis require equal angular velocities for both bodies
   --
   --                  //this is unused for now, it's a todo
   --                  impact.d3.Vector jointAxis0local;
   --                  impact.d3.Vector jointAxis1local;
   --
   --                  btPlaneSpace1(m_rbAFrame.getBasis().getColumn(2),jointAxis0local,jointAxis1local);
   --
   --                  impact.d3.Vector jointAxis0 = getRigidBodyA().getCenterOfMassTransform().getBasis() * jointAxis0local;
   --                  impact.d3.Vector jointAxis1 = getRigidBodyA().getCenterOfMassTransform().getBasis() * jointAxis1local;
   --                  impact.d3.Vector hingeAxisWorld = getRigidBodyA().getCenterOfMassTransform().getBasis() * m_rbAFrame.getBasis().getColumn(2);
   --
   --                  new (&m_jacAng[0])        impact.d3.jacobian_Entry(jointAxis0,
   --                          m_rbA.getCenterOfMassTransform().getBasis().transpose(),
   --                          m_rbB.getCenterOfMassTransform().getBasis().transpose(),
   --                          m_rbA.getInvInertiaDiagLocal(),
   --                          m_rbB.getInvInertiaDiagLocal());
   --
   --                  new (&m_jacAng[1])        impact.d3.jacobian_Entry(jointAxis1,
   --                          m_rbA.getCenterOfMassTransform().getBasis().transpose(),
   --                          m_rbB.getCenterOfMassTransform().getBasis().transpose(),
   --                          m_rbA.getInvInertiaDiagLocal(),
   --                          m_rbB.getInvInertiaDiagLocal());
   --
   --                  new (&m_jacAng[2])        impact.d3.jacobian_Entry(hingeAxisWorld,
   --                          m_rbA.getCenterOfMassTransform().getBasis().transpose(),
   --                          m_rbB.getCenterOfMassTransform().getBasis().transpose(),
   --                          m_rbA.getInvInertiaDiagLocal(),
   --                          m_rbB.getInvInertiaDiagLocal());
   --
   --                          // clear accumulator
   --                          m_accLimitImpulse = impact.d3.Scalar(0.);
   --
   --                          // test angular limit
   --                          testLimit(m_rbA.getCenterOfMassTransform(),m_rbB.getCenterOfMassTransform());
   --
   --                  //Compute K = J*W*J' for hinge axis
   --                  impact.d3.Vector axisA =  getRigidBodyA().getCenterOfMassTransform().getBasis() *  m_rbAFrame.getBasis().getColumn(2);
   --                  m_kHinge =   1.0f / (getRigidBodyA().computeAngularImpulseDenominator(axisA) +
   --                                                           getRigidBodyB().computeAngularImpulseDenominator(axisA));
   --
   --          }
   --  }




   procedure setFrameOffsetA (Self : in out Item;   To : in Transform_3d)
   is
   begin
      Self.m_rbAFrame := To;
   end setFrameOffsetA;


   procedure setFrameOffsetB (Self : in out Item;   To : in Transform_3d)
   is
   begin
      Self.m_rbBFrame := To;
   end setFrameOffsetB;




   function getFrameOffsetA (Self : in Item) return Transform_3d
   is
   begin
      return Self.m_rbAFrame;
   end getFrameOffsetA;

   --          impact.d3.Transform& getFrameOffsetA()
   --          {
   --          return m_rbAFrame;
   --          }




   function getFrameOffsetB (Self : in Item) return Transform_3d
   is
   begin
      return Self.m_rbBFrame;
   end getFrameOffsetB;

   --          impact.d3.Transform& getFrameOffsetB()
   --          {
   --                  return m_rbBFrame;
   --          }

   procedure setFrames (Self   : in out Item;
                        frameA : in     Transform_3d;
                        frameB : in     Transform_3d)
   is
   begin
      Self.m_rbAFrame := frameA;
      Self.m_rbBFrame := frameB;
      --  buildJacobian (Self);
   end setFrames;

   --  void impact.d3.Joint.hinge::setFrames(const impact.d3.Transform & frameA, const impact.d3.Transform & frameB)
   --  {
   --          m_rbAFrame = frameA;
   --          m_rbBFrame = frameB;
   --          buildJacobian();
   --  }

   procedure setAngularOnly (Self        : in out Item;
                             angularOnly : in     Boolean)
   is
   begin
      Self.m_angularOnly := angularOnly;
   end setAngularOnly;

--          void        setAngularOnly(bool angularOnly)
--          {
--                  m_angularOnly = angularOnly;
--          }

   procedure enableAngularMotor (Self            : in out Item;
                                 enableMotor     : in     Boolean;
                                 targetVelocity  : in     Real;
                                 maxMotorImpulse : in     Real)
   is
   begin
      Self.m_enableAngularMotor := enableMotor;
      Self.m_motorTargetVelocity := targetVelocity;
      Self.m_maxMotorImpulse := maxMotorImpulse;
   end enableAngularMotor;

   --          void        enableAngularMotor(bool enableMotor,impact.d3.Scalar targetVelocity,impact.d3.Scalar maxMotorImpulse)
   --          {
   --                  m_enableAngularMotor  = enableMotor;
   --                  m_motorTargetVelocity = targetVelocity;
   --                  m_maxMotorImpulse = maxMotorImpulse;
   --          }

   --          // extra motor API, including ability to set a target rotation (as opposed to angular velocity)
   --          // note: setMotorTarget sets angular velocity under the hood, so you must call it every tick to
   --          //       maintain a given angular target.


   procedure enableMotor (Self        : in out Item;
                          enableMotor : in Boolean)
   is
   begin
      Self.m_enableAngularMotor := enableMotor;
   end enableMotor;

   --          void enableMotor(bool enableMotor)         { m_enableAngularMotor = enableMotor; }

   procedure setMaxMotorImpulse (Self            : in out Item;
                                 maxMotorImpulse : in     Real)
   is
   begin
      Self.m_maxMotorImpulse := maxMotorImpulse;
   end setMaxMotorImpulse;

   --          void setMaxMotorImpulse(impact.d3.Scalar maxMotorImpulse) { m_maxMotorImpulse = maxMotorImpulse; }

   procedure setMotorTarget (Self  : in out Item;
                             qAinB : in     Quaternion;
                             dt    : in     Real   )
   is
      use impact.d3.Quaternions, impact.d3.Transform;

      qConstraint : aliased Quaternion := multiply (Inverse (getRotation (Self.m_rbBFrame)),
                                                    multiply (qAinB, getRotation (Self.m_rbAFrame)));

      vNoHinge    : aliased Vector_3   := quatRotate (normalize (qConstraint'Access), vHinge);
      qNoHinge    : constant Quaternion := shortestArcQuat (vHinge, normalize (vNoHinge'Access));
      qHinge      : aliased Quaternion := multiply (Inverse (qNoHinge),  qConstraint);
      targetAngle :         Real     := getAngle (normalize (qHinge'Access));
   begin
      if targetAngle > impact.d3.Scalar.SIMD_PI then
         qHinge := impact.d3.Quaternions."-" (qHinge);
         targetAngle := getAngle (qHinge);
      end if;
      if qHinge.V (3) < 0.0 then
         targetAngle := -targetAngle;
      end if;
      setMotorTarget (Self, targetAngle, dt);
   end setMotorTarget;

   --  void impact.d3.Joint.hinge::setMotorTarget(const impact.d3.Quaternion& qAinB, impact.d3.Scalar dt)
   --  {
   --          // convert target from body to constraint space
   --          impact.d3.Quaternion qConstraint = m_rbBFrame.getRotation().inverse() * qAinB * m_rbAFrame.getRotation();
   --          qConstraint.normalize();
   --
   --          // extract "pure" hinge component
   --          impact.d3.Vector vNoHinge = quatRotate(qConstraint, vHinge); vNoHinge.normalize();
   --          impact.d3.Quaternion qNoHinge = shortestArcQuat(vHinge, vNoHinge);
   --          impact.d3.Quaternion qHinge = qNoHinge.inverse() * qConstraint;
   --          qHinge.normalize();
   --
   --          // compute angular target, clamped to limits
   --          impact.d3.Scalar targetAngle = qHinge.getAngle();
   --          if (targetAngle > SIMD_PI) // long way around. flip quat and recalculate.
   --          {
   --                  qHinge = -(qHinge);
   --                  targetAngle = qHinge.getAngle();
   --          }
   --          if (qHinge.getZ() < 0)
   --                  targetAngle = -targetAngle;
   --
   --          setMotorTarget(targetAngle, dt);
   --  }

   procedure setMotorTarget (Self        : in out Item;
                             targetAngle : in out Real;
                             dt          : in     Real)
   is
      curAngle : constant Real := getHingeAngle (Self,
                                          Self.getRigidBodyA.getCenterOfMassTransform,
                                          Self.getRigidBodyB.getCenterOfMassTransform);
      dAngle   : constant Real := targetAngle - curAngle;
   begin
      Self.m_limit.fit (targetAngle);
      Self.m_motorTargetVelocity := dAngle / dt;
   end setMotorTarget;

   --  void impact.d3.Joint.hinge::setMotorTarget(impact.d3.Scalar targetAngle, impact.d3.Scalar dt)
   --  {
   --  #ifdef        _BT_USE_CENTER_LIMIT_
   --          m_limit.fit(targetAngle);
   --  #else
   --          if (m_lowerLimit < m_upperLimit)
   --          {
   --                  if (targetAngle < m_lowerLimit)
   --                          targetAngle = m_lowerLimit;
   --                  else if (targetAngle > m_upperLimit)
   --                          targetAngle = m_upperLimit;
   --          }
   --  #endif
   --          // compute angular velocity
   --          impact.d3.Scalar curAngle  = getHingeAngle(m_rbA.getCenterOfMassTransform(),m_rbB.getCenterOfMassTransform());
   --          impact.d3.Scalar dAngle = targetAngle - curAngle;
   --          m_motorTargetVelocity = dAngle / dt;
   --  }
   --

   procedure setLimit (Self             : in out Item;
                       low              : in     Real;
                       high             : in     Real;
                       softness         : in     Real := 0.9;
                       biasFactor       : in     Real := 0.3;
                       relaxationFactor : in     Real := 1.0)
   is
   begin
      Self.m_limit.set (low, high, softness, biasFactor, relaxationFactor);
   end setLimit;

   --          void        setLimit(impact.d3.Scalar low,impact.d3.Scalar high,impact.d3.Scalar _softness = 0.9f, impact.d3.Scalar _biasFactor = 0.3f, impact.d3.Scalar _relaxationFactor = 1.0f)
   --          {
   --  #ifdef        _BT_USE_CENTER_LIMIT_
   --                  m_limit.set(low, high, _softness, _biasFactor, _relaxationFactor);
   --  #else
   --                  m_lowerLimit = btNormalizeAngle(low);
   --                  m_upperLimit = btNormalizeAngle(high);
   --                  m_limitSoftness =  _softness;
   --                  m_biasFactor = _biasFactor;
   --                  m_relaxationFactor = _relaxationFactor;
   --  #endif
   --          }






   procedure setAxis (Self    : in out Item;
                      axisInA : in     Vector_3)
   is
      use linear_Algebra_3d,
          impact.d3.Transform;
      use impact.d3.Quaternions;
      use math.Vectors;

      rbAxisA1,
      rbAxisA2    : Vector_3;
      pivotInA    : Vector_3;
      axisInB     : Vector_3;
      rotationArc : Quaternion;
      rbAxisB1    : Vector_3;
      rbAxisB2    : Vector_3;
   begin
      btPlaneSpace1 (axisInA, rbAxisA1, rbAxisA2);

      pivotInA     := getOrigin  (Self.m_rbAFrame);
      axisInB      := getBasis   (Self.getRigidBodyA.getCenterOfMassTransform) * axisInA;
      rotationArc  := shortestArcQuat (axisInA, axisInB);
      rbAxisB1     := quatRotate (rotationArc, rbAxisA1);
      rbAxisB2     := cross      (axisInB,     rbAxisB1);


      setBasis (Self.m_rbAFrame, ((rbAxisA1 (1), rbAxisA2 (1), axisInA (1)),
                                  (rbAxisA1 (2), rbAxisA2 (2), axisInA (2)),
                                  (rbAxisA1 (3), rbAxisA2 (3), axisInA (3))));

      setOrigin (Self.m_rbBFrame, inverse (getRigidBodyB (Self).getCenterOfMassTransform) *
                                  (getRigidBodyA (Self).getCenterOfMassTransform * pivotInA));

      setBasis (Self.m_rbBFrame, ((rbAxisB1 (1), rbAxisB2 (1), axisInB (1)),
                                  (rbAxisA1 (2), rbAxisB2 (2), axisInB (2)),
                                  (rbAxisA1 (3), rbAxisB2 (3), axisInB (3))));

      setBasis (Self.m_rbBFrame, inverse (getBasis (getRigidBodyB (Self).getCenterOfMassTransform)) * getBasis (Self.m_rbBFrame));
   end setAxis;


   --          void        setAxis(impact.d3.Vector& axisInA)
   --          {
   --                  impact.d3.Vector rbAxisA1, rbAxisA2;
   --                  btPlaneSpace1(axisInA, rbAxisA1, rbAxisA2);
   --                  impact.d3.Vector pivotInA = m_rbAFrame.getOrigin();
   --  //                m_rbAFrame.getOrigin() = pivotInA;
   --                  m_rbAFrame.getBasis().setValue( rbAxisA1.getX(),rbAxisA2.getX(),axisInA.getX(),
   --                                                                                  rbAxisA1.getY(),rbAxisA2.getY(),axisInA.getY(),
   --                                                                                  rbAxisA1.getZ(),rbAxisA2.getZ(),axisInA.getZ() );
   --
   --                  impact.d3.Vector axisInB = m_rbA.getCenterOfMassTransform().getBasis() * axisInA;
   --
   --                  impact.d3.Quaternion rotationArc = shortestArcQuat(axisInA,axisInB);
   --                  impact.d3.Vector rbAxisB1 =  quatRotate(rotationArc,rbAxisA1);
   --                  impact.d3.Vector rbAxisB2 = axisInB.cross(rbAxisB1);
   --
   --                  m_rbBFrame.getOrigin() = m_rbB.getCenterOfMassTransform().inverse()(m_rbA.getCenterOfMassTransform()(pivotInA));
   --
   --                  m_rbBFrame.getBasis().setValue( rbAxisB1.getX(),rbAxisB2.getX(),axisInB.getX(),
   --                                                                                  rbAxisB1.getY(),rbAxisB2.getY(),axisInB.getY(),
   --                                                                                  rbAxisB1.getZ(),rbAxisB2.getZ(),axisInB.getZ() );
   --                  m_rbBFrame.getBasis() = m_rbB.getCenterOfMassTransform().getBasis().inverse() * m_rbBFrame.getBasis();
   --
   --          }

   function getLowerLimit (Self : in Item) return Real
   is
   begin
      return Self.m_limit.getLow;
   end getLowerLimit;

   --          impact.d3.Scalar        getLowerLimit() const
   --          {
   --  #ifdef        _BT_USE_CENTER_LIMIT_
   --          return m_limit.getLow();
   --  #else
   --          return m_lowerLimit;
   --  #endif
   --          }

   function getUpperLimit (Self : in Item) return Real
   is
   begin
      return Self.m_limit.getHigh;
   end getUpperLimit;

   --          impact.d3.Scalar        getUpperLimit() const
   --          {
   --  #ifdef        _BT_USE_CENTER_LIMIT_
   --          return m_limit.getHigh();
   --  #else
   --          return m_upperLimit;
   --  #endif
   --          }

   function getHingeAngle (Self : in Item) return Real
   is
   begin
      return getHingeAngle (Self,
                           Self.getRigidBodyA.getCenterOfMassTransform,
                           Self.getRigidBodyB.getCenterOfMassTransform);
   end getHingeAngle;

   --  impact.d3.Scalar impact.d3.Joint.hinge::getHingeAngle()
   --  {
   --          return getHingeAngle(m_rbA.getCenterOfMassTransform(),m_rbB.getCenterOfMassTransform());
   --  }

   function getHingeAngle (Self   : in item;
                           transA : in Transform_3d;
                           transB : in Transform_3d) return Real
   is
      use math.Vectors, impact.d3.Transform;
      use math.Functions;

      refAxis0  : constant Vector_3 := getBasis (transA) * math.Col (getBasis (Self.m_rbAFrame), 1);
      refAxis1  : constant Vector_3 := getBasis (transA) * math.Col (getBasis (Self.m_rbAFrame), 2);
      swingAxis : constant Vector_3 := getBasis (transB) * math.Col (getBasis (Self.m_rbBFrame), 2);
      angle     : constant Real   := Arctan (dot (swingAxis, refAxis0), dot (swingAxis, refAxis1));
   begin
      return Self.m_referenceSign * angle;
   end getHingeAngle;

   --  impact.d3.Scalar impact.d3.Joint.hinge::getHingeAngle(const impact.d3.Transform& transA,const impact.d3.Transform& transB)
   --  {
   --          const impact.d3.Vector refAxis0  = transA.getBasis() * m_rbAFrame.getBasis().getColumn(0);
   --          const impact.d3.Vector refAxis1  = transA.getBasis() * m_rbAFrame.getBasis().getColumn(1);
   --          const impact.d3.Vector swingAxis = transB.getBasis() * m_rbBFrame.getBasis().getColumn(1);
   --  //        impact.d3.Scalar angle = btAtan2Fast(swingAxis.dot(refAxis0), swingAxis.dot(refAxis1));
   --          impact.d3.Scalar angle = btAtan2(swingAxis.dot(refAxis0), swingAxis.dot(refAxis1));
   --          return m_referenceSign * angle;
   --  }

   procedure testLimit (Self   : in out Item;
                        transA : in     Transform_3d;
                        transB : in     Transform_3d)
   is
   begin
      Self.m_hingeAngle := getHingeAngle (Self, transA, transB);
      Self.m_limit.test (Self.m_hingeAngle);
   end testLimit;

   --  void impact.d3.Joint.hinge::testLimit(const impact.d3.Transform& transA,const impact.d3.Transform& transB)
   --  {
   --          // Compute limit information
   --          m_hingeAngle = getHingeAngle(transA,transB);
   --  #ifdef        _BT_USE_CENTER_LIMIT_
   --          m_limit.test(m_hingeAngle);
   --  #else
   --          m_correction = impact.d3.Scalar(0.);
   --          m_limitSign = impact.d3.Scalar(0.);
   --          m_solveLimit = false;
   --          if (m_lowerLimit <= m_upperLimit)
   --          {
   --                  m_hingeAngle = btAdjustAngleToLimits(m_hingeAngle, m_lowerLimit, m_upperLimit);
   --                  if (m_hingeAngle <= m_lowerLimit)
   --                  {
   --                          m_correction = (m_lowerLimit - m_hingeAngle);
   --                          m_limitSign = 1.0f;
   --                          m_solveLimit = true;
   --                  }
   --                  else if (m_hingeAngle >= m_upperLimit)
   --                  {
   --                          m_correction = m_upperLimit - m_hingeAngle;
   --                          m_limitSign = -1.0f;
   --                          m_solveLimit = true;
   --                  }
   --          }
   --  #endif
   --          return;
   --  }

   function getAFrame (Self : in Item) return Transform_3d
   is
   begin
      return Self.m_rbAFrame;
   end getAFrame;

   --          impact.d3.Transform& getAFrame() { return m_rbAFrame; };

   function getBFrame (Self : in Item) return Transform_3d
   is
   begin
      return Self.m_rbBFrame;
   end getBFrame;

   --          impact.d3.Transform& getAFrame() { return m_rbAFrame; };

   function getSolveLimit (Self : in Item) return Boolean
   is
   begin
      return Self.m_limit.isLimit;
   end getSolveLimit;

   --          inline int getSolveLimit()
   --          {
   --  #ifdef        _BT_USE_CENTER_LIMIT_
   --          return m_limit.isLimit();
   --  #else
   --          return m_solveLimit;
   --  #endif
   --          }

   function getLimitSign (Self : in Item) return Real
   is
   begin
      return Self.m_limit.getSign;
   end getLimitSign;

   --          inline impact.d3.Scalar getLimitSign()
   --          {
   --  #ifdef        _BT_USE_CENTER_LIMIT_
   --          return m_limit.getSign();
   --  #else
   --                  return m_limitSign;
   --  #endif
   --          }

   function getAngularOnly (Self : in Item) return Boolean
   is
   begin
      return Self.m_angularOnly;
   end getAngularOnly;

   --          inline bool getAngularOnly()
   --          {
   --                  return m_angularOnly;
   --          }

   function getEnableAngularMotor (Self : in Item) return Boolean
   is
   begin
      return Self.m_enableAngularMotor;
   end getEnableAngularMotor;

   --          inline bool getEnableAngularMotor()
   --          {
   --                  return m_enableAngularMotor;
   --          }

   function getMotorTargetVelosity (Self : in Item) return Real
   is
   begin
      return Self.m_motorTargetVelocity;
   end getMotorTargetVelosity;

   --          inline impact.d3.Scalar getMotorTargetVelosity()
   --          {
   --                  return m_motorTargetVelocity;
   --          }

   function getMaxMotorImpulse (Self : in Item) return Real
   is
   begin
      return Self.m_maxMotorImpulse;
   end getMaxMotorImpulse;

   --          inline impact.d3.Scalar getMaxMotorImpulse()
   --          {
   --                  return m_maxMotorImpulse;
   --          }

   function getUseFrameOffset (Self : in Item) return Boolean
   is
   begin
      return Self.m_useOffsetForConstraintFrame;
   end getUseFrameOffset;

   --          // access for UseFrameOffset
   --          bool getUseFrameOffset() { return m_useOffsetForConstraintFrame; }

   procedure setUseFrameOffset (Self             : in out Item;
                                frameOffsetOnOff : in     Boolean)
   is
   begin
      Self.m_useOffsetForConstraintFrame := frameOffsetOnOff;
   end setUseFrameOffset;

   --          void setUseFrameOffset(bool frameOffsetOnOff) { m_useOffsetForConstraintFrame = frameOffsetOnOff; }

end impact.d3.Joint.hinge;




--  void impact.d3.Joint.hinge::getInfo2InternalUsingFrameOffset(btConstraintInfo2* info, const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB)
--  {
--          btAssert(!m_useSolveConstraintObsolete);
--          int i, s = info->rowskip;
--          // transforms in world space
--          impact.d3.Transform trA = transA*m_rbAFrame;
--          impact.d3.Transform trB = transB*m_rbBFrame;
--          // pivot point
--          impact.d3.Vector pivotAInW = trA.getOrigin();
--          impact.d3.Vector pivotBInW = trB.getOrigin();
--  #if 1
--          // difference between frames in WCS
--          impact.d3.Vector ofs = trB.getOrigin() - trA.getOrigin();
--          // now get weight factors depending on masses
--          impact.d3.Scalar miA = getRigidBodyA().getInvMass();
--          impact.d3.Scalar miB = getRigidBodyB().getInvMass();
--          bool hasStaticBody = (miA < SIMD_EPSILON) || (miB < SIMD_EPSILON);
--          impact.d3.Scalar miS = miA + miB;
--          impact.d3.Scalar factA, factB;
--          if(miS > impact.d3.Scalar(0.f))
--          {
--                  factA = miB / miS;
--          }
--          else
--          {
--                  factA = impact.d3.Scalar(0.5f);
--          }
--          factB = impact.d3.Scalar(1.0f) - factA;
--          // get the desired direction of hinge axis
--          // as weighted sum of Z-orthos of frameA and frameB in WCS
--          impact.d3.Vector ax1A = trA.getBasis().getColumn(2);
--          impact.d3.Vector ax1B = trB.getBasis().getColumn(2);
--          impact.d3.Vector ax1 = ax1A * factA + ax1B * factB;
--          ax1.normalize();
--          // fill first 3 rows
--          // we want: velA + wA x relA == velB + wB x relB
--          impact.d3.Transform bodyA_trans = transA;
--          impact.d3.Transform bodyB_trans = transB;
--          int s0 = 0;
--          int s1 = s;
--          int s2 = s * 2;
--          int nrow = 2; // last filled row
--          impact.d3.Vector tmpA, tmpB, relA, relB, p, q;
--          // get vector from bodyB to frameB in WCS
--          relB = trB.getOrigin() - bodyB_trans.getOrigin();
--          // get its projection to hinge axis
--          impact.d3.Vector projB = ax1 * relB.dot(ax1);
--          // get vector directed from bodyB to hinge axis (and orthogonal to it)
--          impact.d3.Vector orthoB = relB - projB;
--          // same for bodyA
--          relA = trA.getOrigin() - bodyA_trans.getOrigin();
--          impact.d3.Vector projA = ax1 * relA.dot(ax1);
--          impact.d3.Vector orthoA = relA - projA;
--          impact.d3.Vector totalDist = projA - projB;
--          // get offset vectors relA and relB
--          relA = orthoA + totalDist * factA;
--          relB = orthoB - totalDist * factB;
--          // now choose average ortho to hinge axis
--          p = orthoB * factA + orthoA * factB;
--          impact.d3.Scalar len2 = p.length2();
--          if(len2 > SIMD_EPSILON)
--          {
--                  p /= btSqrt(len2);
--          }
--          else
--          {
--                  p = trA.getBasis().getColumn(1);
--          }
--          // make one more ortho
--          q = ax1.cross(p);
--          // fill three rows
--          tmpA = relA.cross(p);
--          tmpB = relB.cross(p);
--      for (i=0; i<3; i++) info->m_J1angularAxis[s0+i] = tmpA[i];
--      for (i=0; i<3; i++) info->m_J2angularAxis[s0+i] = -tmpB[i];
--          tmpA = relA.cross(q);
--          tmpB = relB.cross(q);
--          if(hasStaticBody && getSolveLimit())
--          { // to make constraint between static and dynamic objects more rigid
--                  // remove wA (or wB) from equation if angular limit is hit
--                  tmpB *= factB;
--                  tmpA *= factA;
--          }
--          for (i=0; i<3; i++) info->m_J1angularAxis[s1+i] = tmpA[i];
--      for (i=0; i<3; i++) info->m_J2angularAxis[s1+i] = -tmpB[i];
--          tmpA = relA.cross(ax1);
--          tmpB = relB.cross(ax1);
--          if(hasStaticBody)
--          { // to make constraint between static and dynamic objects more rigid
--                  // remove wA (or wB) from equation
--                  tmpB *= factB;
--                  tmpA *= factA;
--          }
--          for (i=0; i<3; i++) info->m_J1angularAxis[s2+i] = tmpA[i];
--      for (i=0; i<3; i++) info->m_J2angularAxis[s2+i] = -tmpB[i];
--
--          impact.d3.Scalar k = info->fps * info->erp;
--
--          if (!m_angularOnly)
--          {
--                  for (i=0; i<3; i++) info->m_J1linearAxis[s0+i] = p[i];
--                  for (i=0; i<3; i++) info->m_J1linearAxis[s1+i] = q[i];
--                  for (i=0; i<3; i++) info->m_J1linearAxis[s2+i] = ax1[i];
--
--          // compute three elements of right hand side
--
--                  impact.d3.Scalar rhs = k * p.dot(ofs);
--                  info->m_constraintError[s0] = rhs;
--                  rhs = k * q.dot(ofs);
--                  info->m_constraintError[s1] = rhs;
--                  rhs = k * ax1.dot(ofs);
--                  info->m_constraintError[s2] = rhs;
--          }
--          // the hinge axis should be the only unconstrained
--          // rotational axis, the angular velocity of the two bodies perpendicular to
--          // the hinge axis should be equal. thus the constraint equations are
--          //    p*w1 - p*w2 = 0
--          //    q*w1 - q*w2 = 0
--          // where p and q are unit vectors normal to the hinge axis, and w1 and w2
--          // are the angular velocity vectors of the two bodies.
--          int s3 = 3 * s;
--          int s4 = 4 * s;
--          info->m_J1angularAxis[s3 + 0] = p[0];
--          info->m_J1angularAxis[s3 + 1] = p[1];
--          info->m_J1angularAxis[s3 + 2] = p[2];
--          info->m_J1angularAxis[s4 + 0] = q[0];
--          info->m_J1angularAxis[s4 + 1] = q[1];
--          info->m_J1angularAxis[s4 + 2] = q[2];
--
--          info->m_J2angularAxis[s3 + 0] = -p[0];
--          info->m_J2angularAxis[s3 + 1] = -p[1];
--          info->m_J2angularAxis[s3 + 2] = -p[2];
--          info->m_J2angularAxis[s4 + 0] = -q[0];
--          info->m_J2angularAxis[s4 + 1] = -q[1];
--          info->m_J2angularAxis[s4 + 2] = -q[2];
--          // compute the right hand side of the constraint equation. set relative
--          // body velocities along p and q to bring the hinge back into alignment.
--          // if ax1A,ax1B are the unit length hinge axes as computed from bodyA and
--          // bodyB, we need to rotate both bodies along the axis u = (ax1 x ax2).
--          // if "theta" is the angle between ax1 and ax2, we need an angular velocity
--          // along u to cover angle erp*theta in one step :
--          //   |angular_velocity| = angle/time = erp*theta / stepsize
--          //                      = (erp*fps) * theta
--          //    angular_velocity  = |angular_velocity| * (ax1 x ax2) / |ax1 x ax2|
--          //                      = (erp*fps) * theta * (ax1 x ax2) / sin(theta)
--          // ...as ax1 and ax2 are unit length. if theta is smallish,
--          // theta ~= sin(theta), so
--          //    angular_velocity  = (erp*fps) * (ax1 x ax2)
--          // ax1 x ax2 is in the plane space of ax1, so we project the angular
--          // velocity to p and q to find the right hand side.
--          k = info->fps * info->erp;
--          impact.d3.Vector u = ax1A.cross(ax1B);
--          info->m_constraintError[s3] = k * u.dot(p);
--          info->m_constraintError[s4] = k * u.dot(q);
--  #endif
--          // check angular limits
--          nrow = 4; // last filled row
--          int srow;
--          impact.d3.Scalar limit_err = impact.d3.Scalar(0.0);
--          int limit = 0;
--          if(getSolveLimit())
--          {
--  #ifdef        _BT_USE_CENTER_LIMIT_
--          limit_err = m_limit.getCorrection() * m_referenceSign;
--  #else
--          limit_err = m_correction * m_referenceSign;
--  #endif
--          limit = (limit_err > impact.d3.Scalar(0.0)) ? 1 : 2;
--
--          }
--          // if the hinge has joint limits or motor, add in the extra row
--          int powered = 0;
--          if(getEnableAngularMotor())
--          {
--                  powered = 1;
--          }
--          if(limit || powered)
--          {
--                  nrow++;
--                  srow = nrow * info->rowskip;
--                  info->m_J1angularAxis[srow+0] = ax1[0];
--                  info->m_J1angularAxis[srow+1] = ax1[1];
--                  info->m_J1angularAxis[srow+2] = ax1[2];
--
--                  info->m_J2angularAxis[srow+0] = -ax1[0];
--                  info->m_J2angularAxis[srow+1] = -ax1[1];
--                  info->m_J2angularAxis[srow+2] = -ax1[2];
--
--                  impact.d3.Scalar lostop = getLowerLimit();
--                  impact.d3.Scalar histop = getUpperLimit();
--                  if(limit && (lostop == histop))
--                  {  // the joint motor is ineffective
--                          powered = 0;
--                  }
--                  info->m_constraintError[srow] = impact.d3.Scalar(0.0f);
--                  impact.d3.Scalar currERP = (m_flags & BT_HINGE_FLAGS_ERP_STOP) ? m_stopERP : info->erp;
--                  if(powered)
--                  {
--                          if(m_flags & BT_HINGE_FLAGS_CFM_NORM)
--                          {
--                                  info->cfm[srow] = m_normalCFM;
--                          }
--                          impact.d3.Scalar mot_fact = getMotorFactor(m_hingeAngle, lostop, histop, m_motorTargetVelocity, info->fps * currERP);
--                          info->m_constraintError[srow] += mot_fact * m_motorTargetVelocity * m_referenceSign;
--                          info->m_lowerLimit[srow] = - m_maxMotorImpulse;
--                          info->m_upperLimit[srow] =   m_maxMotorImpulse;
--                  }
--                  if(limit)
--                  {
--                          k = info->fps * currERP;
--                          info->m_constraintError[srow] += k * limit_err;
--                          if(m_flags & BT_HINGE_FLAGS_CFM_STOP)
--                          {
--                                  info->cfm[srow] = m_stopCFM;
--                          }
--                          if(lostop == histop)
--                          {
--                                  // limited low and high simultaneously
--                                  info->m_lowerLimit[srow] = -SIMD_INFINITY;
--                                  info->m_upperLimit[srow] = SIMD_INFINITY;
--                          }
--                          else if(limit == 1)
--                          { // low limit
--                                  info->m_lowerLimit[srow] = 0;
--                                  info->m_upperLimit[srow] = SIMD_INFINITY;
--                          }
--                          else
--                          { // high limit
--                                  info->m_lowerLimit[srow] = -SIMD_INFINITY;
--                                  info->m_upperLimit[srow] = 0;
--                          }
--                          // bounce (we'll use slider parameter abs(1.0 - m_dampingLimAng) for that)
--  #ifdef        _BT_USE_CENTER_LIMIT_
--                          impact.d3.Scalar bounce = m_limit.getRelaxationFactor();
--  #else
--                          impact.d3.Scalar bounce = m_relaxationFactor;
--  #endif
--                          if(bounce > impact.d3.Scalar(0.0))
--                          {
--                                  impact.d3.Scalar vel = angVelA.dot(ax1);
--                                  vel -= angVelB.dot(ax1);
--                                  // only apply bounce if the velocity is incoming, and if the
--                                  // resulting c[] exceeds what we already have.
--                                  if(limit == 1)
--                                  {        // low limit
--                                          if(vel < 0)
--                                          {
--                                                  impact.d3.Scalar newc = -bounce * vel;
--                                                  if(newc > info->m_constraintError[srow])
--                                                  {
--                                                          info->m_constraintError[srow] = newc;
--                                                  }
--                                          }
--                                  }
--                                  else
--                                  {        // high limit - all those computations are reversed
--                                          if(vel > 0)
--                                          {
--                                                  impact.d3.Scalar newc = -bounce * vel;
--                                                  if(newc < info->m_constraintError[srow])
--                                                  {
--                                                          info->m_constraintError[srow] = newc;
--                                                  }
--                                          }
--                                  }
--                          }
--  #ifdef        _BT_USE_CENTER_LIMIT_
--                          info->m_constraintError[srow] *= m_limit.getBiasFactor();
--  #else
--                          info->m_constraintError[srow] *= m_biasFactor;
--  #endif
--                  } // if(limit)
--          } // if angular limit or powered
--  }
--
--

--



