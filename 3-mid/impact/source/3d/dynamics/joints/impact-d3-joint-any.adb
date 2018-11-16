--  #include "impact.d3.Joint.any.h"
--  #include "BulletDynamics/Dynamics/impact.d3.Object.rigid.h"
--  #include "LinearMath/impact.d3.TransformUtil.h"
--  #include "LinearMath/impact.d3.TransformUtil.h"
--  #include <new>

with impact.d3.Transform; use impact.d3.Transform;
with impact.d3.Scalar;
with impact.d3.Matrix;
with Interfaces.C;
with impact.d3.Containers;
with impact.d3.Joint;
with impact.d3.Vector;
with impact.d3.jacobian_Entry;



package body impact.d3.Joint.any
is

   use Interfaces;



   --- Globals
   --

   D6_USE_OBSOLETE_METHOD : constant Boolean := False;
   D6_USE_FRAME_OFFSET    : constant Boolean := True;


--  #define GENERIC_D6_DISABLE_WARMSTARTING 1









   ------------
   --- Utility
   --


   function btGetMatrixElem (mat   : in Matrix_3x3;
                             index : in Integer  ) return Real
   is
      i : constant Integer := index rem 3;
      j : constant Integer := index / 3;
   begin
      return mat (i, j);
   end btGetMatrixElem;

   --  impact.d3.Scalar btGetMatrixElem(const impact.d3.Matrix& mat, int index);
   --  impact.d3.Scalar btGetMatrixElem(const impact.d3.Matrix& mat, int index)
   --  {
   --          int i = index%3;
   --          int j = index/3;
   --          return mat[i][j];
   --  }



   --  MatrixToEulerXYZ from    http://www.geometrictools.com/LibFoundation/Mathematics/Wm4Matrix3.inl.html


   function MatrixToEulerXYZ (mat : in     Matrix_3x3;
                              xyz : access Vector_3 ) return Boolean
   is
      use math.Functions;
      use impact.d3.Scalar;

      fi : constant Real := btGetMatrixElem (mat, 2);
   begin
      if fi < 1.0 then
         if fi > -1.0 then
            xyz (1) := Arctan (-btGetMatrixElem (mat, 5), btGetMatrixElem (mat, 8));
            xyz (2) := Arcsin (btGetMatrixElem (mat, 2));
            xyz (3) := Arctan (-btGetMatrixElem (mat, 1), btGetMatrixElem (mat, 0));
            return True;
         else
            xyz (1) := -Arctan (btGetMatrixElem (mat, 3), btGetMatrixElem (mat, 4));
            xyz (2) := -SIMD_HALF_PI;
            xyz (3) := 0.0;
            return False;
         end if;
      else
         xyz (1) := Arctan (btGetMatrixElem (mat, 3), btGetMatrixElem (mat, 4));
         xyz (2) := SIMD_HALF_PI;
         xyz (3) := 0.0;
      end if;
      return False;
   end MatrixToEulerXYZ;

   --  bool        matrixToEulerXYZ(const impact.d3.Matrix& mat,impact.d3.Vector& xyz)
   --  {
   --          //        // rot =  cy*cz          -cy*sz           sy
   --          //        //        cz*sx*sy+cx*sz  cx*cz-sx*sy*sz -cy*sx
   --          //        //       -cx*cz*sy+sx*sz  cz*sx+cx*sy*sz  cx*cy
   --          //
   --
   --          impact.d3.Scalar fi = btGetMatrixElem(mat,2);
   --          if (fi < impact.d3.Scalar(1.0f))
   --          {
   --                  if (fi > impact.d3.Scalar(-1.0f))
   --                  {
   --                          xyz[0] = btAtan2(-btGetMatrixElem(mat,5),btGetMatrixElem(mat,8));
   --                          xyz[1] = btAsin(btGetMatrixElem(mat,2));
   --                          xyz[2] = btAtan2(-btGetMatrixElem(mat,1),btGetMatrixElem(mat,0));
   --                          return true;
   --                  }
   --                  else
   --                  {
   --                          // WARNING.  Not unique.  XA - ZA = -atan2(r10,r11)
   --                          xyz[0] = -btAtan2(btGetMatrixElem(mat,3),btGetMatrixElem(mat,4));
   --                          xyz[1] = -SIMD_HALF_PI;
   --                          xyz[2] = impact.d3.Scalar(0.0);
   --                          return false;
   --                  }
   --          }
   --          else
   --          {
   --                  // WARNING.  Not unique.  XAngle + ZAngle = atan2(r10,r11)
   --                  xyz[0] = btAtan2(btGetMatrixElem(mat,3),btGetMatrixElem(mat,4));
   --                  xyz[1] = SIMD_HALF_PI;
   --                  xyz[2] = 0.0;
   --          }
   --          return false;
   --  }





   -------------------------
   --- btRotationLimitMotor
   --

   function to_btRotationLimitMotor return btRotationLimitMotor
   is
      Self : btRotationLimitMotor;
   begin
      Self.m_accumulatedImpulse := 0.0;
      Self.m_targetVelocity     := 0.0;
      Self.m_maxMotorForce      := 0.1;
      Self.m_maxLimitForce      := 300.0;
      Self.m_loLimit            := 1.0;
      Self.m_hiLimit            := -1.0;
      Self.m_normalCFM          := 0.0;
      Self.m_stopERP            := 0.2;
      Self.m_stopCFM            := 0.0;
      Self.m_bounce             := 0.0;
      Self.m_damping            := 1.0;
      Self.m_limitSoftness      := 0.5;
      Self.m_currentLimit       := 0;
      Self.m_currentLimitError  := 0.0;
      Self.m_enableMotor        := False;
      return Self;
   end to_btRotationLimitMotor;

   --      btRotationalLimitMotor()
   --      {
   --              m_accumulatedImpulse = 0.f;
   --          m_targetVelocity = 0;
   --          m_maxMotorForce = 0.1f;
   --          m_maxLimitForce = 300.0f;
   --          m_loLimit = 1.0f;
   --          m_hiLimit = -1.0f;
   --                  m_normalCFM = 0.f;
   --                  m_stopERP = 0.2f;
   --                  m_stopCFM = 0.f;
   --          m_bounce = 0.0f;
   --          m_damping = 1.0f;
   --          m_limitSoftness = 0.5f;
   --          m_currentLimit = 0;
   --          m_currentLimitError = 0;
   --          m_enableMotor = false;
   --      }



   function to_btRotationLimitMotor (Other : in btRotationLimitMotor) return btRotationLimitMotor
   is
      Self : btRotationLimitMotor;
   begin
      Self.m_targetVelocity    := Other.m_targetVelocity;
      Self.m_maxMotorForce     := Other.m_maxMotorForce;
      Self.m_limitSoftness     := Other.m_limitSoftness;
      Self.m_loLimit           := Other.m_loLimit;
      Self.m_hiLimit           := Other.m_hiLimit;
      Self.m_normalCFM         := Other.m_normalCFM;
      Self.m_stopERP           := Other.m_stopERP;
      Self.m_stopCFM           := Other.m_stopCFM;
      Self.m_bounce            := Other.m_bounce;
      Self.m_currentLimit      := Other.m_currentLimit;
      Self.m_currentLimitError := Other.m_currentLimitError;
      Self.m_enableMotor       := Other.m_enableMotor;

      return Self;
   end to_btRotationLimitMotor;






   function isLimited (Self : in btRotationLimitMotor) return Boolean
   is
   begin
      if Self.m_loLimit > Self.m_hiLimit then
         return False;
      else
         return True;
      end if;
   end isLimited;

   --          //! Is limited
   --      bool isLimited()
   --      {
   --              if(m_loLimit > m_hiLimit) return false;
   --              return true;
   --      }






   function needApplyTorques (Self : in btRotationLimitMotor) return Boolean
   is
   begin
      if Self.m_currentLimit = 0 and Self.m_enableMotor = False then
         return False;
      else
         return True;
      end if;
   end needApplyTorques;

   --          //! Need apply correction
   --      bool needApplyTorques()
   --      {
   --              if(m_currentLimit == 0 && m_enableMotor == false) return false;
   --              return true;
   --      }





   function testLimitValue (Self       : access btRotationLimitMotor;
                            test_value : in     Real             ) return Integer
   is
   begin
      if Self.m_loLimit > Self.m_hiLimit then
         Self.m_currentLimit := 0;
         return 0;
      end if;

      if test_value < Self.m_loLimit then
         Self.m_currentLimit := 1;
         Self.m_currentLimitError := test_value - Self.m_loLimit;
         return 1;
      elsif test_value > Self.m_hiLimit then
         Self.m_currentLimit := 2;
         Self.m_currentLimitError := test_value - Self.m_hiLimit;
         return 2;
      end if;

      Self.m_currentLimit := 0;

      return 0;
   end testLimitValue;

   --  int btRotationalLimitMotor::testLimitValue(impact.d3.Scalar test_value)
   --  {
   --          if(m_loLimit>m_hiLimit)
   --          {
   --                  m_currentLimit = 0;//Free from violation
   --                  return 0;
   --          }
   --          if (test_value < m_loLimit)
   --          {
   --                  m_currentLimit = 1;//low limit violation
   --                  m_currentLimitError =  test_value - m_loLimit;
   --                  return 1;
   --          }
   --          else if (test_value> m_hiLimit)
   --          {
   --                  m_currentLimit = 2;//High limit violation
   --                  m_currentLimitError = test_value - m_hiLimit;
   --                  return 2;
   --          };
   --
   --          m_currentLimit = 0;//Free from violation
   --          return 0;
   --
   --  }




   function solveAngularLimits (Self : access btRotationLimitMotor;   timeStep     : in     math.Real;
                                                                      axis         : access math.Vector_3;
                                                                      jacDiagABInv : in     math.Real;
                                                                      body0, body1 : in     impact.d3.Object.rigid.view) return math.Real
   is
      use math.Vectors, impact.d3.Vector;

      target_velocity,
      maxMotorForce        : math.Real;

      vel_diff             : math.Vector_3;

      rel_vel,
      motor_relvel         : math.Real;

      unclippedMotorImpulse,
      clippedMotorImpulse  : math.Real;    -- clip correction impulse

   begin
      if not Self.needApplyTorques then
         return 0.0;
      end if;


      target_velocity := Self.m_targetVelocity;
      maxMotorForce   := Self.m_maxMotorForce;

      --  current error correction
      --
      if Self.m_currentLimit /= 0 then
         target_velocity := -Self.m_stopERP * Self.m_currentLimitError / timeStep;
         maxMotorForce   :=  Self.m_maxLimitForce;
      end if;

      maxMotorForce := maxMotorForce * timeStep;

      --  current velocity difference
      --
      declare
         angVelA : math.Vector_3;
         angVelB : math.Vector_3;
      begin
         body0.internalGetAngularVelocity (angVelA);
         body1.internalGetAngularVelocity (angVelB);

         vel_diff := angVelA - angVelB;
      end;


      rel_vel      := dot (axis.all, vel_diff);
      motor_relvel := Self.m_limitSoftness * (target_velocity  - Self.m_damping * rel_vel);   -- correction velocity


      if         motor_relvel <  impact.d3.Scalar.SIMD_EPSILON
        and then motor_relvel > -impact.d3.Scalar.SIMD_EPSILON
      then
         return 0.0;   -- no need for applying force
      end if;



      unclippedMotorImpulse := (1.0 + Self.m_bounce) * motor_relvel * jacDiagABInv;   -- correction impulse


      --  todo: should clip against accumulated impulse

      if unclippedMotorImpulse > 0.0 then
         clippedMotorImpulse := (if unclippedMotorImpulse >  maxMotorForce then  maxMotorForce else unclippedMotorImpulse);
      else
         clippedMotorImpulse := (if unclippedMotorImpulse < -maxMotorForce then -maxMotorForce else unclippedMotorImpulse);
      end if;


      --  sort with accumulated impulses
      --
      declare
         lo              : constant math.Real := -BT_LARGE_FLOAT;
         hi              : constant math.Real :=  BT_LARGE_FLOAT;

         oldaccumImpulse : constant math.Real := Self.m_accumulatedImpulse;
         sum             : constant math.Real := oldaccumImpulse + clippedMotorImpulse;

         motorImp        : math.Vector_3;
         pragma Unreferenced (motorImp);

      begin
         Self.m_accumulatedImpulse := (if sum > hi then 0.0
                                                   else (if sum < lo then 0.0 else sum));

         clippedMotorImpulse := Self.m_accumulatedImpulse - oldaccumImpulse;

         motorImp := clippedMotorImpulse * axis.all;

        -- // body0->applyTorqueImpulse(motorImp);
        -- // body1->applyTorqueImpulse(-motorImp);

         body0.internalApplyImpulse ((0.0, 0.0, 0.0),  body0.getInvInertiaTensorWorld * axis.all,   clippedMotorImpulse);
         body1.internalApplyImpulse ((0.0, 0.0, 0.0),  body1.getInvInertiaTensorWorld * axis.all,  -clippedMotorImpulse);
      end;


      return clippedMotorImpulse;
   end solveAngularLimits;


--  impact.d3.Scalar btRotationalLimitMotor::solveAngularLimits(
--          impact.d3.Scalar timeStep,impact.d3.Vector& axis,impact.d3.Scalar jacDiagABInv,
--          impact.d3.Object.rigid * body0, impact.d3.Object.rigid * body1 )
--  {
--          if (needApplyTorques()==false) return 0.0f;
--
--          impact.d3.Scalar target_velocity = m_targetVelocity;
--          impact.d3.Scalar maxMotorForce = m_maxMotorForce;
--
--          //current error correction
--          if (m_currentLimit!=0)
--          {
--                  target_velocity = -m_stopERP*m_currentLimitError/(timeStep);
--                  maxMotorForce = m_maxLimitForce;
--          }
--
--          maxMotorForce *= timeStep;
--
--          // current velocity difference
--
--          impact.d3.Vector angVelA;
--          body0->internalGetAngularVelocity(angVelA);
--          impact.d3.Vector angVelB;
--          body1->internalGetAngularVelocity(angVelB);
--
--          impact.d3.Vector vel_diff;
--          vel_diff = angVelA-angVelB;
--
--
--
--          impact.d3.Scalar rel_vel = axis.dot(vel_diff);
--
--          // correction velocity
--          impact.d3.Scalar motor_relvel = m_limitSoftness*(target_velocity  - m_damping*rel_vel);
--
--
--          if ( motor_relvel < SIMD_EPSILON && motor_relvel > -SIMD_EPSILON  )
--          {
--                  return 0.0f;//no need for applying force
--          }
--
--
--          // correction impulse
--          impact.d3.Scalar unclippedMotorImpulse = (1+m_bounce)*motor_relvel*jacDiagABInv;
--
--          // clip correction impulse
--          impact.d3.Scalar clippedMotorImpulse;
--
--          ///@todo: should clip against accumulated impulse
--          if (unclippedMotorImpulse>0.0f)
--          {
--                  clippedMotorImpulse =  unclippedMotorImpulse > maxMotorForce? maxMotorForce: unclippedMotorImpulse;
--          }
--          else
--          {
--                  clippedMotorImpulse =  unclippedMotorImpulse < -maxMotorForce ? -maxMotorForce: unclippedMotorImpulse;
--          }
--
--
--          // sort with accumulated impulses
--          impact.d3.Scalar        lo = impact.d3.Scalar(-BT_LARGE_FLOAT);
--          impact.d3.Scalar        hi = impact.d3.Scalar(BT_LARGE_FLOAT);
--
--          impact.d3.Scalar oldaccumImpulse = m_accumulatedImpulse;
--          impact.d3.Scalar sum = oldaccumImpulse + clippedMotorImpulse;
--          m_accumulatedImpulse = sum > hi ? impact.d3.Scalar(0.) : sum < lo ? impact.d3.Scalar(0.) : sum;
--
--          clippedMotorImpulse = m_accumulatedImpulse - oldaccumImpulse;
--
--          impact.d3.Vector motorImp = clippedMotorImpulse * axis;
--
--          //body0->applyTorqueImpulse(motorImp);
--          //body1->applyTorqueImpulse(-motorImp);
--
--          body0->internalApplyImpulse(impact.d3.Vector(0,0,0), body0->getInvInertiaTensorWorld()*axis,clippedMotorImpulse);
--          body1->internalApplyImpulse(impact.d3.Vector(0,0,0), body1->getInvInertiaTensorWorld()*axis,-clippedMotorImpulse);
--
--
--          return clippedMotorImpulse;
--
--
--  }








   ------------------------------
   --- btTranslationalLimitMotor
   --

   function to_btTranslationalLimitMotor return btTranslationalLimitMotor
   is
      Self : btTranslationalLimitMotor;
   begin
      Self.m_lowerLimit         := (0.0, 0.0, 0.0);
      Self.m_upperLimit         := (0.0, 0.0, 0.0);
      Self.m_accumulatedImpulse := (0.0, 0.0, 0.0);
      Self.m_normalCFM          := (0.0, 0.0, 0.0);
      Self.m_stopERP            := (0.2, 0.2, 0.2);
      Self.m_stopCFM            := (0.0, 0.0, 0.0);
      Self.m_limitSoftness      := 0.7;
      Self.m_damping            := 1.0;
      Self.m_restitution        := 0.5;

      for I in 1 .. 3 loop
         Self.m_enableMotor (I)    := False;
         Self.m_targetVelocity (I) := 0.0;
         Self.m_maxMotorForce (I)  := 0.0;
      end loop;

      return Self;
   end to_btTranslationalLimitMotor;

   --      btTranslationalLimitMotor()
   --      {
   --              m_lowerLimit.setValue(0.f,0.f,0.f);
   --              m_upperLimit.setValue(0.f,0.f,0.f);
   --              m_accumulatedImpulse.setValue(0.f,0.f,0.f);
   --                  m_normalCFM.setValue(0.f, 0.f, 0.f);
   --                  m_stopERP.setValue(0.2f, 0.2f, 0.2f);
   --                  m_stopCFM.setValue(0.f, 0.f, 0.f);
   --
   --              m_limitSoftness = 0.7f;
   --              m_damping = impact.d3.Scalar(1.0f);
   --              m_restitution = impact.d3.Scalar(0.5f);
   --                  for(int i=0; i < 3; i++)
   --                  {
   --                          m_enableMotor[i] = false;
   --                          m_targetVelocity[i] = impact.d3.Scalar(0.f);
   --                          m_maxMotorForce[i] = impact.d3.Scalar(0.f);
   --                  }
   --      }




   function to_btTranslationalLimitMotor (Other : in btTranslationalLimitMotor) return btTranslationalLimitMotor
   is
      Self : btTranslationalLimitMotor;
   begin
      Self.m_lowerLimit         := other.m_lowerLimit;
      Self.m_upperLimit         := other.m_upperLimit;
      Self.m_accumulatedImpulse := other.m_accumulatedImpulse;

      Self.m_limitSoftness      := other.m_limitSoftness;
      Self.m_damping            := other.m_damping;
      Self.m_restitution        := other.m_restitution;
      Self.m_normalCFM          := other.m_normalCFM;
      Self.m_stopERP            := other.m_stopERP;
      Self.m_stopCFM            := other.m_stopCFM;

      for i in 1 .. 3
      loop
         Self.m_enableMotor    (i) := other.m_enableMotor    (i);
         Self.m_targetVelocity (i) := other.m_targetVelocity (i);
         Self.m_maxMotorForce  (i) := other.m_maxMotorForce  (i);
      end loop;


      return Self;
   end to_btTranslationalLimitMotor;





   function isLimited (Self       : in btTranslationalLimitMotor;
                       limitIndex : in Integer                 ) return Boolean
   is
   begin
      return Self.m_upperLimit (limitIndex) >= Self.m_lowerLimit (limitIndex);
   end isLimited;

   --      inline bool        isLimited(int limitIndex)
   --      {
   --         return (m_upperLimit[limitIndex] >= m_lowerLimit[limitIndex]);
   --      }









   function solveLinearAxis (Self : access btTranslationalLimitMotor;  timeStep         : in math.Real;
                                                                       jacDiagABInv     : in math.Real;
                                                                       body1            : in impact.d3.Object.rigid.view;
                                                                       pointInA         : in math.Vector_3;
                                                                       body2            : in impact.d3.Object.rigid.view;
                                                                       pointInB         : in math.Vector_3;
                                                                       limitIndex       : in Integer;
                                                                       axis_normal_on_a : in math.Vector_3;
                                                                       anchorPos        : in math.Vector_3) return math.Real
   is
      use math.Vectors, impact.d3.Vector;

      rel_pos1   : constant math.Vector_3 := anchorPos - body1.getCenterOfMassPosition;
      rel_pos2   : constant math.Vector_3 := anchorPos - body2.getCenterOfMassPosition;

      vel1, vel2 : math.Vector_3;
      vel        : math.Vector_3;

      rel_vel    : math.Real;

   begin
      --- find relative velocity
      --
      body1.internalGetVelocityInLocalPointObsolete (rel_pos1, vel1);
      body2.internalGetVelocityInLocalPointObsolete (rel_pos2, vel2);

      vel     := vel1 - vel2;
      rel_vel := dot (axis_normal_on_a, vel);


      --- apply displacement correction
      --

      --  positional error (zeroth order error)
      declare
         depth    : math.Real := -dot (pointInA - pointInB,  axis_normal_on_a);

         lo       : math.Real := -BT_LARGE_FLOAT;
         hi       : math.Real :=  BT_LARGE_FLOAT;

         minLimit : constant math.Real := Self.m_lowerLimit (limitIndex);
         maxLimit : constant math.Real := Self.m_upperLimit (limitIndex);

      begin
         --  handle the limits
         --
         if minLimit < maxLimit then

            if depth > maxLimit then
               depth := depth - maxLimit;
               lo := 0.0;

            else
               if depth < minLimit then
                  depth := depth - minLimit;
                  hi    := 0.0;
               else
                  return 0.0;
               end if;
            end if;

         end if;


         declare
            normalImpulse    : math.Real :=   Self.m_limitSoftness
                                            * (Self.m_restitution * depth / timeStep  -  Self.m_damping * rel_vel)
                                            * jacDiagABInv;

            oldNormalImpulse : constant math.Real := Self.m_accumulatedImpulse (limitIndex);
            sum              : constant math.Real := oldNormalImpulse + normalImpulse;

            impulse_vector,
            ftorqueAxis1,
            ftorqueAxis2     : math.Vector_3;
            pragma Unreferenced (impulse_vector);

         begin
            Self.m_accumulatedImpulse (limitIndex) := (if sum > hi then 0.0
                                                                   else (if sum < lo then 0.0 else sum));

            normalImpulse                           := Self.m_accumulatedImpulse (limitIndex) - oldNormalImpulse;
            impulse_vector                          := axis_normal_on_a * normalImpulse;

            --  //body1.applyImpulse( impulse_vector, rel_pos1);
            --  //body2.applyImpulse(-impulse_vector, rel_pos2);

            ftorqueAxis1 := cross (rel_pos1, axis_normal_on_a);
            ftorqueAxis2 := cross (rel_pos2, axis_normal_on_a);

            body1.internalApplyImpulse (axis_normal_on_a * body1.getInvMass,  body1.getInvInertiaTensorWorld * ftorqueAxis1,   normalImpulse);
            body2.internalApplyImpulse (axis_normal_on_a * body2.getInvMass,  body2.getInvInertiaTensorWorld * ftorqueAxis2,  -normalImpulse);


            return normalImpulse;
         end;
      end;

   end solveLinearAxis;


--  impact.d3.Scalar btTranslationalLimitMotor::solveLinearAxis(
--          impact.d3.Scalar timeStep,
--          impact.d3.Scalar jacDiagABInv,
--          impact.d3.Object.rigid& body1,const impact.d3.Vector &pointInA,
--          impact.d3.Object.rigid& body2,const impact.d3.Vector &pointInB,
--          int limit_index,
--          const impact.d3.Vector & axis_normal_on_a,
--          const impact.d3.Vector & anchorPos)
--  {
--
--          ///find relative velocity
--          //    impact.d3.Vector rel_pos1 = pointInA - body1.getCenterOfMassPosition();
--          //    impact.d3.Vector rel_pos2 = pointInB - body2.getCenterOfMassPosition();
--          impact.d3.Vector rel_pos1 = anchorPos - body1.getCenterOfMassPosition();
--          impact.d3.Vector rel_pos2 = anchorPos - body2.getCenterOfMassPosition();
--
--          impact.d3.Vector vel1;
--          body1.internalGetVelocityInLocalPointObsolete(rel_pos1,vel1);
--          impact.d3.Vector vel2;
--          body2.internalGetVelocityInLocalPointObsolete(rel_pos2,vel2);
--          impact.d3.Vector vel = vel1 - vel2;
--
--          impact.d3.Scalar rel_vel = axis_normal_on_a.dot(vel);
--
--
--
--          /// apply displacement correction
--
--          //positional error (zeroth order error)
--          impact.d3.Scalar depth = -(pointInA - pointInB).dot(axis_normal_on_a);
--          impact.d3.Scalar        lo = impact.d3.Scalar(-BT_LARGE_FLOAT);
--          impact.d3.Scalar        hi = impact.d3.Scalar(BT_LARGE_FLOAT);
--
--          impact.d3.Scalar minLimit = m_lowerLimit[limit_index];
--          impact.d3.Scalar maxLimit = m_upperLimit[limit_index];
--
--          //handle the limits
--          if (minLimit < maxLimit)
--          {
--                  {
--                          if (depth > maxLimit)
--                          {
--                                  depth -= maxLimit;
--                                  lo = impact.d3.Scalar(0.);
--
--                          }
--                          else
--                          {
--                                  if (depth < minLimit)
--                                  {
--                                          depth -= minLimit;
--                                          hi = impact.d3.Scalar(0.);
--                                  }
--                                  else
--                                  {
--                                          return 0.0f;
--                                  }
--                          }
--                  }
--          }
--
--          impact.d3.Scalar normalImpulse= m_limitSoftness*(m_restitution*depth/timeStep - m_damping*rel_vel) * jacDiagABInv;
--
--
--
--
--          impact.d3.Scalar oldNormalImpulse = m_accumulatedImpulse[limit_index];
--          impact.d3.Scalar sum = oldNormalImpulse + normalImpulse;
--          m_accumulatedImpulse[limit_index] = sum > hi ? impact.d3.Scalar(0.) : sum < lo ? impact.d3.Scalar(0.) : sum;
--          normalImpulse = m_accumulatedImpulse[limit_index] - oldNormalImpulse;
--
--          impact.d3.Vector impulse_vector = axis_normal_on_a * normalImpulse;
--          //body1.applyImpulse( impulse_vector, rel_pos1);
--          //body2.applyImpulse(-impulse_vector, rel_pos2);
--
--          impact.d3.Vector ftorqueAxis1 = rel_pos1.cross(axis_normal_on_a);
--          impact.d3.Vector ftorqueAxis2 = rel_pos2.cross(axis_normal_on_a);
--          body1.internalApplyImpulse(axis_normal_on_a*body1.getInvMass(), body1.getInvInertiaTensorWorld()*ftorqueAxis1,normalImpulse);
--          body2.internalApplyImpulse(axis_normal_on_a*body2.getInvMass(), body2.getInvInertiaTensorWorld()*ftorqueAxis2,-normalImpulse);
--
--
--
--
--          return normalImpulse;
--  }



   function testLimitValue (Self       : access btTranslationalLimitMotor;
                            limitIndex : in     Integer;
                            test_value : in     Real                  ) return Integer
   is
      loLimit : constant Real := Self.m_lowerLimit (limitIndex);
      hiLimit : constant Real := Self.m_upperLimit (limitIndex);

   begin
      if loLimit > hiLimit then
         Self.m_currentLimit (limitIndex)      := 0;
         Self.m_currentLimitError (limitIndex) := 0.0;
         return 0;
      end if;


      if test_value < loLimit then
         Self.m_currentLimit (limitIndex)      := 2;
         Self.m_currentLimitError (limitIndex) := test_value - loLimit;
         return 2;

      elsif test_value > hiLimit then
         Self.m_currentLimit (limitIndex)      := 1;
         Self.m_currentLimitError (limitIndex) := test_value - hiLimit;
         return 1;
      end if;


      Self.m_currentLimit (limitIndex)      := 0;
      Self.m_currentLimitError (limitIndex) := 0.0;

      return 0;
   end testLimitValue;

   --  int btTranslationalLimitMotor::testLimitValue(int limitIndex, impact.d3.Scalar test_value)
   --  {
   --          impact.d3.Scalar loLimit = m_lowerLimit[limitIndex];
   --          impact.d3.Scalar hiLimit = m_upperLimit[limitIndex];
   --          if(loLimit > hiLimit)
   --          {
   --                  m_currentLimit[limitIndex] = 0;//Free from violation
   --                  m_currentLimitError[limitIndex] = impact.d3.Scalar(0.f);
   --                  return 0;
   --          }
   --
   --          if (test_value < loLimit)
   --          {
   --                  m_currentLimit[limitIndex] = 2;//low limit violation
   --                  m_currentLimitError[limitIndex] =  test_value - loLimit;
   --                  return 2;
   --          }
   --          else if (test_value> hiLimit)
   --          {
   --                  m_currentLimit[limitIndex] = 1;//High limit violation
   --                  m_currentLimitError[limitIndex] = test_value - hiLimit;
   --                  return 1;
   --          };
   --
   --          m_currentLimit[limitIndex] = 0;//Free from violation
   --          m_currentLimitError[limitIndex] = impact.d3.Scalar(0.f);
   --          return 0;
   --  }






   function needApplyForce (Self       : in btTranslationalLimitMotor;
                            limitIndex : in Integer                 ) return Boolean
   is
   begin
      if Self.m_currentLimit (limitIndex) = 0 and Self.m_enableMotor (limitIndex) = False then
         return False;
      else
         return True;
      end if;
   end needApplyForce;

   --      inline bool needApplyForce(int limitIndex)
   --      {
   --              if(m_currentLimit[limitIndex] == 0 && m_enableMotor[limitIndex] == false) return false;
   --              return true;
   --      }
   --          int testLimitValue(int limitIndex, impact.d3.Scalar test_value);








   ----------------------------
   --- impact.d3.Joint.any
   --




   ----------
   --- Forge
   --

   function to_any_Joint (rbA,      rbB            : in impact.d3.Object.rigid.view;
                          frameInA, frameInB       : in Transform_3d;
                          useLinearReferenceFrameA : in Boolean       ) return Item
   is
      Self : Item;
   begin
      Self.define (impact.d3.Joint.D6_CONSTRAINT_TYPE,         -- Define base class.
                   rbA,  rbB);

      Self.m_frameInA := frameInA;
      Self.m_frameInB := frameInB;

      Self.m_useLinearReferenceFrameA    := useLinearReferenceFrameA;
      Self.m_useOffsetForConstraintFrame := D6_USE_FRAME_OFFSET;
      Self.m_flags                       := 0;
      Self.m_useSolveConstraintObsolete  := D6_USE_OBSOLETE_METHOD;


      Self.calculateTransforms;

      return Self;
   end to_any_Joint;



--  impact.d3.Joint.any::impact.d3.Joint.any(impact.d3.Object.rigid& rbA, impact.d3.Object.rigid& rbB, const impact.d3.Transform& frameInA, const impact.d3.Transform& frameInB, bool useLinearReferenceFrameA)
--  : impact.d3.Joint(D6_CONSTRAINT_TYPE, rbA, rbB)
--  , m_frameInA(frameInA)
--  , m_frameInB(frameInB),
--  m_useLinearReferenceFrameA(useLinearReferenceFrameA),
--  m_useOffsetForConstraintFrame(D6_USE_FRAME_OFFSET),
--  m_flags(0),
--  m_useSolveConstraintObsolete(D6_USE_OBSOLETE_METHOD)
--  {
--          calculateTransforms();
--  }






   function to_any_Joint (rbB                      : in impact.d3.Object.rigid.view;
                                        frameInB                 : in Transform_3d;
                          useLinearReferenceFrameB : in Boolean       ) return Item
   is
      use linear_Algebra_3d;
      Self : Item;
   begin
      Self.define (impact.d3.Joint.D6_CONSTRAINT_TYPE,         -- Define base class.
                   impact.d3.Joint.getFixedBody,  rbB);

      Self.m_frameInB := frameInB;

      Self.m_useLinearReferenceFrameA    := useLinearReferenceFrameB;   -- tbd: this looks odd
      Self.m_useOffsetForConstraintFrame := D6_USE_FRAME_OFFSET;
      Self.m_flags                       := 0;
      Self.m_useSolveConstraintObsolete  := False;

      Self.m_frameInA                    := rbB.getCenterOfMassTransform * Self.m_frameInB;   -- not providing rigidbody A means implicitly using worldspace for body A

      Self.calculateTransforms;


      return Self;
   end to_any_Joint;



--  impact.d3.Joint.any::impact.d3.Joint.any(impact.d3.Object.rigid& rbB, const impact.d3.Transform& frameInB, bool useLinearReferenceFrameB)
--          : impact.d3.Joint(D6_CONSTRAINT_TYPE, getFixedBody(), rbB),
--                  m_frameInB(frameInB),
--                  m_useLinearReferenceFrameA(useLinearReferenceFrameB),
--                  m_useOffsetForConstraintFrame(D6_USE_FRAME_OFFSET),
--                  m_flags(0),
--                  m_useSolveConstraintObsolete(false)
--  {
--          ///not providing rigidbody A means implicitly using worldspace for body A
--          m_frameInA = rbB.getCenterOfMassTransform() * m_frameInB;
--          calculateTransforms();
--  }






   ---------------
   --- Attributes
   --


   function getRotationalLimitMotor (Self  : in     Item;   index : in Integer) return btRotationLimitMotor'Class
   is
   begin
      return Self.m_angularLimits (index);
   end getRotationalLimitMotor;


   procedure setRotationalLimitMotor (Self  : in out Item;   index : in Integer;   To : in btRotationLimitMotor'Class)
   is
   begin
      Self.m_angularLimits (index) := btRotationLimitMotor (To);
   end setRotationalLimitMotor;



   function getRotationalLimitMotor (Self  : access Item;   index : in Integer) return access btRotationLimitMotor'Class
   is
   begin
      return Self.m_angularLimits (index)'Access;
   end getRotationalLimitMotor;

--      btRotationalLimitMotor * getRotationalLimitMotor(int index)
--      {
--              return &m_angularLimits[index];
--      }





   function getTranslationalLimitMotor (Self  : access Item) return access btTranslationalLimitMotor'Class
   is
   begin
      return Self.m_linearLimits'Access;
   end getTranslationalLimitMotor;

--      btTranslationalLimitMotor * getTranslationalLimitMotor()
--      {
--              return &m_linearLimits;
--      }
--





   procedure calcAnchorPos (Self : in out Item)     -- overridable
   is
      use math.Vectors;

      imA : math.Real := Self.getRigidBodyA.getInvMass;
      imB : constant math.Real := Self.getRigidBodyA.getInvMass;

      weight : math.Real;

      pa, pb : math.Vector_3;

   begin
      if imB = 0.0 then
         weight := 1.0;
      else
         weight := imA / (imA + imB);
      end if;

      pA := getOrigin (Self.m_calculatedTransformA);
      pB := getOrigin (Self.m_calculatedTransformB);

      Self.m_AnchorPos :=   pA * weight
                          + pB * (1.0 - weight);
   end calcAnchorPos;




--  void impact.d3.Joint.any::calcAnchorPos(void)
--  {
--          impact.d3.Scalar imA = m_rbA.getInvMass();
--          impact.d3.Scalar imB = m_rbB.getInvMass();
--          impact.d3.Scalar weight;
--          if(imB == impact.d3.Scalar(0.0))
--          {
--                  weight = impact.d3.Scalar(1.0);
--          }
--          else
--          {
--                  weight = imA / (imA + imB);
--          }
--          const impact.d3.Vector& pA = m_calculatedTransformA.getOrigin();
--          const impact.d3.Vector& pB = m_calculatedTransformB.getOrigin();
--          m_AnchorPos = pA * weight + pB * (impact.d3.Scalar(1.0) - weight);
--          return;
--  }
--
--
--






   --  Override the default global value of a parameter (such as ERP or CFM), optionally provide the axis (0..5).
   --  If no axis is provided, it uses the default axis for this constraint.
   --
   overriding procedure setParam (Self  :    out Item;
                       num   : in     impact.d3.Joint.btConstraintParams;
                       value : in     Math.Real;
                       axis  : in     Integer := -1)
   is
      use impact.d3.Joint;
   begin

      if         axis >= 1
        and then axis <  4
      then
         case num
         is
         when impact.d3.Joint.BT_CONSTRAINT_STOP_ERP =>
            Self.m_linearLimits.m_stopERP (axis)   := value;
            Self.m_flags                           := Self.m_flags or  BT_6DOF_FLAGS_ERP_STOP * 2**((axis - 2) * BT_6DOF_FLAGS_AXIS_SHIFT);

         when BT_CONSTRAINT_STOP_CFM =>
            Self.m_linearLimits.m_stopCFM (axis)   := value;
            Self.m_flags                           := Self.m_flags or  BT_6DOF_FLAGS_CFM_STOP * 2**((axis - 2) * BT_6DOF_FLAGS_AXIS_SHIFT);

         when BT_CONSTRAINT_CFM =>
            Self.m_linearLimits.m_normalCFM (axis) := value;
            Self.m_flags                           := Self.m_flags or  BT_6DOF_FLAGS_CFM_NORM * 2**((axis - 2) * BT_6DOF_FLAGS_AXIS_SHIFT);

         when others =>
            raise Program_Error;      -- btAssertConstrParams(0);
         end case;


      elsif      axis >= 4
        and then axis <  7
      then
         case num
         is
         when BT_CONSTRAINT_STOP_ERP =>
            Self.m_angularLimits (axis - 3).m_stopERP   := value;
            Self.m_flags                                := Self.m_flags or  BT_6DOF_FLAGS_ERP_STOP * 2**((axis - 2) * BT_6DOF_FLAGS_AXIS_SHIFT);

         when BT_CONSTRAINT_STOP_CFM =>
            Self.m_angularLimits (axis - 3).m_stopCFM   := value;
            Self.m_flags                                := Self.m_flags or  BT_6DOF_FLAGS_CFM_STOP * 2**((axis - 2) * BT_6DOF_FLAGS_AXIS_SHIFT);

         when BT_CONSTRAINT_CFM =>
            Self.m_angularLimits (axis - 3).m_normalCFM := value;
            Self.m_flags                                := Self.m_flags or  BT_6DOF_FLAGS_CFM_NORM * 2**((axis - 2) * BT_6DOF_FLAGS_AXIS_SHIFT);

         when others =>
            raise Program_Error;      -- btAssertConstrParams(0);
         end case;


      else
         raise Program_Error;     -- btAssertConstrParams(0);
      end if;

   end setParam;





--  void impact.d3.Joint.any::setParam(int num, impact.d3.Scalar value, int axis)
--  {
--          if((axis >= 0) && (axis < 3))
--          {
--                  switch(num)
--                  {
--                          case BT_CONSTRAINT_STOP_ERP :
--                                  m_linearLimits.m_stopERP[axis] = value;
--                                  m_flags |= BT_6DOF_FLAGS_ERP_STOP << (axis * BT_6DOF_FLAGS_AXIS_SHIFT);
--                                  break;
--                          case BT_CONSTRAINT_STOP_CFM :
--                                  m_linearLimits.m_stopCFM[axis] = value;
--                                  m_flags |= BT_6DOF_FLAGS_CFM_STOP << (axis * BT_6DOF_FLAGS_AXIS_SHIFT);
--                                  break;
--                          case BT_CONSTRAINT_CFM :
--                                  m_linearLimits.m_normalCFM[axis] = value;
--                                  m_flags |= BT_6DOF_FLAGS_CFM_NORM << (axis * BT_6DOF_FLAGS_AXIS_SHIFT);
--                                  break;
--                          default :
--                                  btAssertConstrParams(0);
--                  }
--          }
--          else if((axis >=3) && (axis < 6))
--          {
--                  switch(num)
--                  {
--                          case BT_CONSTRAINT_STOP_ERP :
--                                  m_angularLimits[axis - 3].m_stopERP = value;
--                                  m_flags |= BT_6DOF_FLAGS_ERP_STOP << (axis * BT_6DOF_FLAGS_AXIS_SHIFT);
--                                  break;
--                          case BT_CONSTRAINT_STOP_CFM :
--                                  m_angularLimits[axis - 3].m_stopCFM = value;
--                                  m_flags |= BT_6DOF_FLAGS_CFM_STOP << (axis * BT_6DOF_FLAGS_AXIS_SHIFT);
--                                  break;
--                          case BT_CONSTRAINT_CFM :
--                                  m_angularLimits[axis - 3].m_normalCFM = value;
--                                  m_flags |= BT_6DOF_FLAGS_CFM_NORM << (axis * BT_6DOF_FLAGS_AXIS_SHIFT);
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





   --  Return the local value of parameter
   --
   overriding function  getParam (Self : in Item;
                       num  : in impact.d3.Joint.btConstraintParams;
                       axis : in Integer := -1) return Math.Real
   is
      use impact.d3.Joint;
      retVal : math.Real := 0.0;

   begin

      if         axis >= 1
        and then axis <  4
      then
         case num
         is
            when BT_CONSTRAINT_STOP_ERP =>
               pragma Assert ((Self.m_flags and (BT_6DOF_FLAGS_ERP_STOP * 2**((axis - 2) * BT_6DOF_FLAGS_AXIS_SHIFT))) /= 0);
               retVal := Self.m_linearLimits.m_stopERP (axis);

            when BT_CONSTRAINT_STOP_CFM =>
               pragma Assert ((Self.m_flags and (BT_6DOF_FLAGS_CFM_STOP  * 2**((axis - 2) * BT_6DOF_FLAGS_AXIS_SHIFT))) /= 0);
               retVal := Self.m_linearLimits.m_stopCFM (axis);

            when BT_CONSTRAINT_CFM =>
               pragma Assert ((Self.m_flags and (BT_6DOF_FLAGS_CFM_NORM * 2**((axis - 2) * BT_6DOF_FLAGS_AXIS_SHIFT))) /= 0);
               retVal := Self.m_linearLimits.m_normalCFM (axis);

            when others =>
               raise Program_Error;    -- btAssertConstrParams(0);
         end case;


      elsif      axis >= 4
        and then axis <  7
      then
         case num
         is
            when BT_CONSTRAINT_STOP_ERP =>
               pragma Assert ((Self.m_flags and BT_6DOF_FLAGS_ERP_STOP * 2**((axis - 2) * BT_6DOF_FLAGS_AXIS_SHIFT)) /= 0);
               retVal := Self.m_angularLimits (axis - 3).m_stopERP;

            when BT_CONSTRAINT_STOP_CFM =>
               pragma Assert ((Self.m_flags and (BT_6DOF_FLAGS_CFM_STOP * 2**((axis - 2) * BT_6DOF_FLAGS_AXIS_SHIFT))) /= 0);
               retVal := Self.m_angularLimits (axis - 3).m_stopCFM;

            when BT_CONSTRAINT_CFM =>
               pragma Assert ((Self.m_flags and (BT_6DOF_FLAGS_CFM_NORM * 2**((axis - 2) * BT_6DOF_FLAGS_AXIS_SHIFT))) /= 0);
               retVal := Self.m_angularLimits (axis - 3).m_normalCFM;

            when others =>
               raise Program_Error;    -- btAssertConstrParams(0);
         end case;

      else
         raise Program_Error;    --  btAssertConstrParams(0);
      end if;


      return retVal;
   end getParam;







--  impact.d3.Scalar impact.d3.Joint.any::getParam(int num, int axis) const
--  {
--          impact.d3.Scalar retVal = 0;
--          if((axis >= 0) && (axis < 3))
--          {
--                  switch(num)
--                  {
--                          case BT_CONSTRAINT_STOP_ERP :
--                                  btAssertConstrParams(m_flags & (BT_6DOF_FLAGS_ERP_STOP << (axis * BT_6DOF_FLAGS_AXIS_SHIFT)));
--                                  retVal = m_linearLimits.m_stopERP[axis];
--                                  break;
--                          case BT_CONSTRAINT_STOP_CFM :
--                                  btAssertConstrParams(m_flags & (BT_6DOF_FLAGS_CFM_STOP << (axis * BT_6DOF_FLAGS_AXIS_SHIFT)));
--                                  retVal = m_linearLimits.m_stopCFM[axis];
--                                  break;
--                          case BT_CONSTRAINT_CFM :
--                                  btAssertConstrParams(m_flags & (BT_6DOF_FLAGS_CFM_NORM << (axis * BT_6DOF_FLAGS_AXIS_SHIFT)));
--                                  retVal = m_linearLimits.m_normalCFM[axis];
--                                  break;
--                          default :
--                                  btAssertConstrParams(0);
--                  }
--          }
--          else if((axis >=3) && (axis < 6))
--          {
--                  switch(num)
--                  {
--                          case BT_CONSTRAINT_STOP_ERP :
--                                  btAssertConstrParams(m_flags & (BT_6DOF_FLAGS_ERP_STOP << (axis * BT_6DOF_FLAGS_AXIS_SHIFT)));
--                                  retVal = m_angularLimits[axis - 3].m_stopERP;
--                                  break;
--                          case BT_CONSTRAINT_STOP_CFM :
--                                  btAssertConstrParams(m_flags & (BT_6DOF_FLAGS_CFM_STOP << (axis * BT_6DOF_FLAGS_AXIS_SHIFT)));
--                                  retVal = m_angularLimits[axis - 3].m_stopCFM;
--                                  break;
--                          case BT_CONSTRAINT_CFM :
--                                  btAssertConstrParams(m_flags & (BT_6DOF_FLAGS_CFM_NORM << (axis * BT_6DOF_FLAGS_AXIS_SHIFT)));
--                                  retVal = m_angularLimits[axis - 3].m_normalCFM;
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
         calculateTransforms (Self,
                              getRigidBodyA (Self).getCenterOfMassTransform,
                              getRigidBodyB (Self).getCenterOfMassTransform);
         info.m_numConstraintRows := 0;
         info.nub := 6;

         for I in 1 .. 3 loop
            if needApplyForce (Self.m_linearLimits, (I)) then
               info.m_numConstraintRows := info.m_numConstraintRows + 1;
               info.nub := info.nub - 1;
            end if;
         end loop;

         for I in 1 .. 2 loop
            if testAngularLimitMotor (Self'Access, I) then
               info.m_numConstraintRows := info.m_numConstraintRows + 1;
               info.nub := info.nub - 1;
            end if;
         end loop;

      end if;
   end getInfo1;

   --  void impact.d3.Joint.any::getInfo1 (btConstraintInfo1* info)
   --  {
   --          if (m_useSolveConstraintObsolete)
   --          {
   --                  info->m_numConstraintRows = 0;
   --                  info->nub = 0;
   --          } else
   --          {
   --                  //prepare constraint
   --                  calculateTransforms(m_rbA.getCenterOfMassTransform(),m_rbB.getCenterOfMassTransform());
   --                  info->m_numConstraintRows = 0;
   --                  info->nub = 6;
   --                  int i;
   --                  //test linear limits
   --                  for(i = 0; i < 3; i++)
   --                  {
   --                          if(m_linearLimits.needApplyForce(i))
   --                          {
   --                                  info->m_numConstraintRows++;
   --                                  info->nub--;
   --                          }
   --                  }
   --                  //test angular limits
   --                  for (i=0;i<3 ;i++ )
   --                  {
   --                          if(testAngularLimitMotor(i))
   --                          {
   --                                  info->m_numConstraintRows++;
   --                                  info->nub--;
   --                          }
   --                  }
   --          }
   --  }




   procedure getInfo1NonVirtual (Self : in out Item;
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

   --  void impact.d3.Joint.any::getInfo1NonVirtual (btConstraintInfo1* info)
   --  {
   --          if (m_useSolveConstraintObsolete)
   --          {
   --                  info->m_numConstraintRows = 0;
   --                  info->nub = 0;
   --          } else
   --          {
   --                  //pre-allocate all 6
   --                  info->m_numConstraintRows = 6;
   --                  info->nub = 0;
   --          }
   --  }




   overriding procedure getInfo2 (Self : in out Item;
                       info :    out impact.d3.Joint.btConstraintInfo2)
   is
      use impact.d3.Object.rigid;

      unused  : Integer;
      pragma Unreferenced (unused);
      transA  : constant Transform_3d := getRigidBodyA (Self).getCenterOfMassTransform;
      transB  : constant Transform_3d := getRigidBodyB (Self).getCenterOfMassTransform;
      linVelA : constant Vector_3  := getRigidBodyA (Self).getLinearVelocity;
      linVelB : constant Vector_3  := getRigidBodyB (Self).getLinearVelocity;
      angVelA : constant Vector_3  := getRigidBodyA (Self).getAngularVelocity;
      angVelB : constant Vector_3  := getRigidBodyB (Self).getAngularVelocity;
   begin
      if Self.m_useOffsetForConstraintFrame then
         declare
            row : constant Integer := setAngularLimits (Self'Access, info, 0, transA, transB,
                                               linVelA, linVelB, angVelA, angVelB);
         begin
            unused := setLinearLimits (Self, info, row, transA, transB,
                                       linVelA, linVelB, angVelA, angVelB);
         end;
      else
         declare
            row : Integer := setLinearLimits (Self, info, 0, transA, transB,
                                              linVelA, linVelB, angVelA, angVelB);
         begin
            unused := setAngularLimits (Self'Access, info, 0, transA, transB,
                                        linVelA, linVelB, angVelA, angVelB);
         end;
      end if;
   end getInfo2;

   --  void impact.d3.Joint.any::getInfo2 (btConstraintInfo2* info)
   --  {
   --          btAssert(!m_useSolveConstraintObsolete);
   --
   --          const impact.d3.Transform& transA = m_rbA.getCenterOfMassTransform();
   --          const impact.d3.Transform& transB = m_rbB.getCenterOfMassTransform();
   --          const impact.d3.Vector& linVelA = m_rbA.getLinearVelocity();
   --          const impact.d3.Vector& linVelB = m_rbB.getLinearVelocity();
   --          const impact.d3.Vector& angVelA = m_rbA.getAngularVelocity();
   --          const impact.d3.Vector& angVelB = m_rbB.getAngularVelocity();
   --
   --          if(m_useOffsetForConstraintFrame)
   --          { // for stability better to solve angular limits first
   --                  int row = setAngularLimits(info, 0,transA,transB,linVelA,linVelB,angVelA,angVelB);
   --                  setLinearLimits(info, row, transA,transB,linVelA,linVelB,angVelA,angVelB);
   --          }
   --          else
   --          { // leave old version for compatibility
   --                  int row = setLinearLimits(info, 0, transA,transB,linVelA,linVelB,angVelA,angVelB);
   --                  setAngularLimits(info, row,transA,transB,linVelA,linVelB,angVelA,angVelB);
   --          }
   --
   --  }





   procedure getInfo2NonVirtual (Self    : in out Item;
                                 info    :    out impact.d3.Joint.btConstraintInfo2;
                                 transA  : in     Transform_3d;
                                 transB  : in     Transform_3d;
                                 linVelA : in     Vector_3;
                                 linVelB : in     Vector_3;
                                 angVelA : in     Vector_3;
                                 angVelB : in     Vector_3)
   is
      unused : Integer;
      pragma Unreferenced (unused);
      unused1 : Boolean;
      pragma Unreferenced (unused1);
   begin
      calculateTransforms (Self, transA, transB);

      for I in 1 .. 3 loop
         unused1 := testAngularLimitMotor (Self'Access, I);
      end loop;

      if Self.m_useOffsetForConstraintFrame then
         declare
            row : constant Integer := setAngularLimits (Self'Access, info, 0, transA, transB,
                                               linVelA, linVelB, angVelA, angVelB);
         begin
            unused := setLinearLimits (Self, info, row, transA, transB,
                                       linVelA, linVelB, angVelA, angVelB);
         end;
      else
         declare
            row : Integer := setLinearLimits (Self, info, 0, transA, transB,
                                              linVelA, linVelB, angVelA, angVelB);
         begin
            unused := setAngularLimits (Self'Access, info, 0, transA, transB,
                                        linVelA, linVelB, angVelA, angVelB);
         end;
      end if;
   end getInfo2NonVirtual;

   --  void impact.d3.Joint.any::getInfo2NonVirtual (btConstraintInfo2* info, const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& linVelA,const impact.d3.Vector& linVelB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB)
   --  {
   --
   --          btAssert(!m_useSolveConstraintObsolete);
   --          //prepare constraint
   --          calculateTransforms(transA,transB);
   --
   --          int i;
   --          for (i=0;i<3 ;i++ )
   --          {
   --                  testAngularLimitMotor(i);
   --          }
   --
   --          if(m_useOffsetForConstraintFrame)
   --          { // for stability better to solve angular limits first
   --                  int row = setAngularLimits(info, 0,transA,transB,linVelA,linVelB,angVelA,angVelB);
   --                  setLinearLimits(info, row, transA,transB,linVelA,linVelB,angVelA,angVelB);
   --          }
   --          else
   --          { // leave old version for compatibility
   --                  int row = setLinearLimits(info, 0, transA,transB,linVelA,linVelB,angVelA,angVelB);
   --                  setAngularLimits(info, row,transA,transB,linVelA,linVelB,angVelA,angVelB);
   --          }
   --  }





   procedure calculateTransforms (Self : in out Item)
   is
      use impact.d3.Object.rigid;
   begin
      calculateTransforms (Self,
                           getRigidBodyA (Self).getCenterOfMassTransform,
                           getRigidBodyB (Self).getCenterOfMassTransform);
   end calculateTransforms;

   --  void impact.d3.Joint.any::calculateTransforms()
   --  {
   --          calculateTransforms(m_rbA.getCenterOfMassTransform(),m_rbB.getCenterOfMassTransform());
   --  }





   procedure calculateTransforms (Self   : in out Item;
                                  transA : in     Transform_3d;
                                  transB : in     Transform_3d)
   is
      use linear_Algebra_3d;
      use impact.d3.Object.rigid;
      use impact.d3.Scalar;
   begin
      Self.m_calculatedTransformA := transA * Self.m_frameInA;
      Self.m_calculatedTransformB := transB * Self.m_frameInB;

      calculateLinearInfo (Self);
      calculateAngleInfo  (Self);

      if Self.m_useOffsetForConstraintFrame then
         declare
            miA : Real := getRigidBodyA (Self).getInvMass;
            miB : constant Real := getRigidBodyB (Self).getInvMass;
            miS : constant Real := miA + miB;
         begin
            Self.m_hasStaticBody := (miA < SIMD_EPSILON) or (mib < SIMD_EPSILON);

            if miS > 0.0 then
               Self.m_factA := miB / miS;
            else
               Self.m_factA := 0.5;
            end if;

            Self.m_factB := 1.0 - Self.m_factA;
         end;
      end if;

   end calculateTransforms;

   --  void impact.d3.Joint.any::calculateTransforms(const impact.d3.Transform& transA,const impact.d3.Transform& transB)
   --  {
   --          m_calculatedTransformA = transA * m_frameInA;
   --          m_calculatedTransformB = transB * m_frameInB;
   --          calculateLinearInfo();
   --          calculateAngleInfo();
   --          if(m_useOffsetForConstraintFrame)
   --          {        //  get weight factors depending on masses
   --                  impact.d3.Scalar miA = getRigidBodyA().getInvMass();
   --                  impact.d3.Scalar miB = getRigidBodyB().getInvMass();
   --                  m_hasStaticBody = (miA < SIMD_EPSILON) || (miB < SIMD_EPSILON);
   --                  impact.d3.Scalar miS = miA + miB;
   --                  if(miS > impact.d3.Scalar(0.f))
   --                  {
   --                          m_factA = miB / miS;
   --                  }
   --                  else
   --                  {
   --                          m_factA = impact.d3.Scalar(0.5f);
   --                  }
   --                  m_factB = impact.d3.Scalar(1.0f) - m_factA;
   --          }
   --  }





   function getCalculatedTransformA (Self : in Item) return Transform_3d
   is
   begin
      return Self.m_calculatedTransformA;
   end getCalculatedTransformA;

   --      {
   --              return m_calculatedTransformA;
   --      }





   function getCalculatedTransformB (Self : in Item) return Transform_3d
   is
   begin
      return Self.m_calculatedTransformB;
   end getCalculatedTransformB;

   --      const impact.d3.Transform & getCalculatedTransformB() const
   --      {
   --              return m_calculatedTransformB;
   --      }





   procedure setFrameOffsetA (Self : in out Item;   To : in Transform_3d)
   is
   begin
      Self.m_frameInA := To;
   end setFrameOffsetA;


   procedure setFrameOffsetB (Self : in out Item;   To : in Transform_3d)
   is
   begin
      Self.m_frameInB := To;
   end setFrameOffsetB;




   function getFrameOffsetA (Self : in Item) return Transform_3d
   is
   begin
      return Self.m_frameInA;
   end getFrameOffsetA;

   --      impact.d3.Transform & getFrameOffsetA()
   --      {
   --              return m_frameInA;
   --      }





   function getFrameOffsetB (Self : in Item) return Transform_3d
   is
   begin
      return Self.m_frameInB;
   end getFrameOffsetB;

   --      impact.d3.Transform & getFrameOffsetB()
   --      {
   --              return m_frameInB;
   --      }





   overriding procedure buildJacobian (Self : in out Item)
   is
   begin
      if Self.m_useSolveConstraintObsolete then
         --  Clear accumulated impulses for the next simulation step.
         --
         Self.m_linearLimits.m_accumulatedImpulse := math.Origin_3d;

         for i in 1 .. 3
         loop
            Self.m_angularLimits (i).m_accumulatedImpulse := 0.0;
         end loop;

         --  calculates transform
         --
         Self.calculateTransforms (Self.getRigidBodyA.getCenterOfMassTransform,
                                   Self.getRigidBodyB.getCenterOfMassTransform);

         --  //  const impact.d3.Vector& pivotAInW = m_calculatedTransformA.getOrigin();
         --  //  const impact.d3.Vector& pivotBInW = m_calculatedTransformB.getOrigin();
         Self.calcAnchorPos;

         declare
            use impact.d3.Matrix;

            pivotAInW : constant math.Vector_3 := Self.m_AnchorPos;
            pivotBInW : constant math.Vector_3 := Self.m_AnchorPos;

            --  not used here
            --    impact.d3.Vector rel_pos1 = pivotAInW - m_rbA.getCenterOfMassPosition();
            --    impact.d3.Vector rel_pos2 = pivotBInW - m_rbB.getCenterOfMassPosition();

            normalWorld : math.Vector_3;

         begin
            --  linear part
            --
            for i in 1 .. 3
            loop
               if Self.m_linearLimits.isLimited (i) then

                  if Self.m_useLinearReferenceFrameA then
                     normalWorld := getColumn (getBasis (Self.m_calculatedTransformA),  i);
                  else
                     normalWorld := getColumn (getBasis (Self.m_calculatedTransformB),  i);
                  end if;

                  Self.buildLinearJacobian (Self.m_jacLinear (i),
                                            normalWorld,
                                            pivotAInW,
                                            pivotBInW);
               end if;
            end loop;

            --  angular part
            --
            for i in 1 .. 3
            loop
               --  calculates error angle
               if Self.testAngularLimitMotor (i) then
                  normalWorld := Self.getAxis (i);

                  Self.buildAngularJacobian (Self.m_jacAng (i),    -- Create angular atom
                                             normalWorld);
               end if;
            end loop;
         end;

      end if;
   end buildJacobian;



--  void impact.d3.Joint.any::buildJacobian()
--  {
--  #ifndef __SPU__
--          if (m_useSolveConstraintObsolete)
--          {
--
--                  // Clear accumulated impulses for the next simulation step
--                  m_linearLimits.m_accumulatedImpulse.setValue(impact.d3.Scalar(0.), impact.d3.Scalar(0.), impact.d3.Scalar(0.));
--                  int i;
--                  for(i = 0; i < 3; i++)
--                  {
--                          m_angularLimits[i].m_accumulatedImpulse = impact.d3.Scalar(0.);
--                  }
--                  //calculates transform
--                  calculateTransforms(m_rbA.getCenterOfMassTransform(),m_rbB.getCenterOfMassTransform());
--
--                  //  const impact.d3.Vector& pivotAInW = m_calculatedTransformA.getOrigin();
--                  //  const impact.d3.Vector& pivotBInW = m_calculatedTransformB.getOrigin();
--                  calcAnchorPos();
--                  impact.d3.Vector pivotAInW = m_AnchorPos;
--                  impact.d3.Vector pivotBInW = m_AnchorPos;
--
--                  // not used here
--                  //    impact.d3.Vector rel_pos1 = pivotAInW - m_rbA.getCenterOfMassPosition();
--                  //    impact.d3.Vector rel_pos2 = pivotBInW - m_rbB.getCenterOfMassPosition();
--
--                  impact.d3.Vector normalWorld;
--                  //linear part
--                  for (i=0;i<3;i++)
--                  {
--                          if (m_linearLimits.isLimited(i))
--                          {
--                                  if (m_useLinearReferenceFrameA)
--                                          normalWorld = m_calculatedTransformA.getBasis().getColumn(i);
--                                  else
--                                          normalWorld = m_calculatedTransformB.getBasis().getColumn(i);
--
--                                  buildLinearJacobian(
--                                          m_jacLinear[i],normalWorld ,
--                                          pivotAInW,pivotBInW);
--
--                          }
--                  }
--
--                  // angular part
--                  for (i=0;i<3;i++)
--                  {
--                          //calculates error angle
--                          if (testAngularLimitMotor(i))
--                          {
--                                  normalWorld = this->getAxis(i);
--                                  // Create angular atom
--                                  buildAngularJacobian(m_jacAng[i],normalWorld);
--                          }
--                  }
--
--          }
--  #endif //__SPU__
--
--  }
--
--





   procedure updateRHS (Self     : in out Item;
                        timeStep : in     Real)
   is
   begin
      null;
   end updateRHS;


--
--  void        impact.d3.Joint.any::updateRHS(impact.d3.Scalar        timeStep)
--  {
--          (void)timeStep;
--
--  }
--




   function getAxis (Self       : in Item;
                     axis_index : in Integer) return Vector_3
   is
   begin
      return Self.m_calculatedAxis (axis_index);
   end getAxis;

   --  impact.d3.Vector impact.d3.Joint.any::getAxis(int axis_index) const
   --  {
   --          return m_calculatedAxis[axis_index];
   --  }





   function getRelativePivotPosition (Self       : in Item;
                                      axis_index : in Integer) return Real
   is
   begin
      return Self.m_calculatedLinearDiff (axis_index);
   end getRelativePivotPosition;

   --  impact.d3.Scalar        impact.d3.Joint.any::getRelativePivotPosition(int axisIndex) const
   --  {
   --          return m_calculatedLinearDiff[axisIndex];
   --  }





   procedure setFrames (Self   : in out Item;
                        frameA : in     Transform_3d;
                        frameB : in     Transform_3d)
   is
   begin
      Self.m_frameInA := frameA;
      Self.m_frameInB := frameB;

      buildJacobian       (Self);
      calculateTransforms (Self);
   end setFrames;

   --  void impact.d3.Joint.any::setFrames(const impact.d3.Transform& frameA, const impact.d3.Transform& frameB)
   --  {
   --          m_frameInA = frameA;
   --          m_frameInB = frameB;
   --          buildJacobian();
   --          calculateTransforms();
   --  }





   function getAngle (Self       : in Item;
                      axis_index : in Integer) return Real
   is
   begin
      return Self.m_calculatedAxisAngleDiff (axis_index);
   end getAngle;

   --  impact.d3.Scalar impact.d3.Joint.any::getAngle(int axisIndex) const
   --  {
   --          return m_calculatedAxisAngleDiff[axisIndex];
   --  }





   function testAngularLimitMotor (Self       : access Item;
                                   axis_index : in     Integer) return Boolean
   is
      use impact.d3.Joint;

      unused : Integer;
      pragma Unreferenced (unused);
      angle  : Real := Self.m_calculatedAxisAngleDiff (axis_index);
   begin
      angle := btAdjustAngleToLimits (angle,
                                     Self.m_angularLimits (axis_index).m_loLimit,
                                     Self.m_angularLimits (axis_index).m_hiLimit);
      Self.m_angularLimits (axis_index).m_currentPosition := angle;
      unused := testLimitValue (Self.m_angularLimits (axis_index)'Access, angle);

      return needApplyTorques (Self.m_angularLimits (axis_index));
   end testAngularLimitMotor;

   --  bool impact.d3.Joint.any::testAngularLimitMotor(int axis_index)
   --  {
   --          impact.d3.Scalar angle = m_calculatedAxisAngleDiff[axis_index];
   --          angle = btAdjustAngleToLimits(angle, m_angularLimits[axis_index].m_loLimit, m_angularLimits[axis_index].m_hiLimit);
   --          m_angularLimits[axis_index].m_currentPosition = angle;
   --          //test limits
   --          m_angularLimits[axis_index].testLimitValue(angle);
   --          return m_angularLimits[axis_index].needApplyTorques();
   --  }





   procedure setLinearLowerLimit (Self        : in out Item;
                                  linearLower : in     Vector_3)
   is
   begin
      Self.m_linearLimits.m_lowerLimit := linearLower;
   end setLinearLowerLimit;

   --      void        setLinearLowerLimit(const impact.d3.Vector& linearLower)
   --      {
   --              m_linearLimits.m_lowerLimit = linearLower;
   --      }





   procedure getLinearLowerLimit (Self        : in     Item;
                                  linearLower :    out Vector_3)
   is
   begin
      linearLower := Self.m_linearLimits.m_lowerLimit;
   end getLinearLowerLimit;

   --          void        getLinearLowerLimit(impact.d3.Vector& linearLower)
   --          {
   --                  linearLower = m_linearLimits.m_lowerLimit;
   --          }





   procedure setLinearUpperLimit (Self        : in out Item;
                                  linearUpper : in     Vector_3)
   is
   begin
      Self.m_linearLimits.m_upperLimit := linearUpper;
   end setLinearUpperLimit;

   --          void        setLinearUpperLimit(const impact.d3.Vector& linearUpper)
   --          {
   --                  m_linearLimits.m_upperLimit = linearUpper;
   --          }





   procedure getLinearUpperLimit (Self        : in     Item;
                                  linearUpper :    out Vector_3)
   is
   begin
      linearUpper := Self.m_linearLimits.m_upperLimit;
   end getLinearUpperLimit;

   --          void        getLinearUpperLimit(impact.d3.Vector& linearUpper)
   --          {
   --                  linearUpper = m_linearLimits.m_upperLimit;
   --          }





   procedure setAngularLowerLimit (Self         : in out Item;
                                   angularLower : in     Vector_3)
   is
      use impact.d3.Scalar;
   begin
      for I in 1 .. 3 loop
         Self.m_angularLimits (I).m_loLimit := btNormalizeAngle (angularLower (I));
      end loop;
   end setAngularLowerLimit;

   --      void        setAngularLowerLimit(const impact.d3.Vector& angularLower)
   --      {
   --                  for(int i = 0; i < 3; i++)
   --                          m_angularLimits[i].m_loLimit = btNormalizeAngle(angularLower[i]);
   --      }





   procedure getAngularLowerLimit (Self         : in     Item;
                                   angularLower :    out Vector_3)
   is
   begin
      for I in 1 .. 3 loop
         angularLower (I) := Self.m_angularLimits (I).m_loLimit;
      end loop;
   end getAngularLowerLimit;

   --          void        getAngularLowerLimit(impact.d3.Vector& angularLower)
   --          {
   --                  for(int i = 0; i < 3; i++)
   --                          angularLower[i] = m_angularLimits[i].m_loLimit;
   --          }





   procedure setAngularUpperLimit (Self         : in out Item;
                                   angularUpper : in     Vector_3)
   is
      use impact.d3.Scalar;
   begin
      for I in 1 .. 3 loop
         Self.m_angularLimits (I).m_hiLimit := btNormalizeAngle (angularUpper (I));
      end loop;
   end setAngularUpperLimit;

   --      void        setAngularUpperLimit(const impact.d3.Vector& angularUpper)
   --      {
   --                  for(int i = 0; i < 3; i++)
   --                          m_angularLimits[i].m_hiLimit = btNormalizeAngle(angularUpper[i]);
   --      }




   procedure getAngularUpperLimit (Self         : in     Item;
                                   angularUpper :    out Vector_3)
   is
   begin
      for I in 1 .. 3 loop
         angularUpper (I) := Self.m_angularLimits (I).m_hiLimit;
      end loop;
   end getAngularUpperLimit;

   --          void        getAngularUpperLimit(impact.d3.Vector& angularUpper)
   --          {
   --                  for(int i = 0; i < 3; i++)
   --                          angularUpper[i] = m_angularLimits[i].m_hiLimit;
   --          }





   procedure setLimit (Self : in out Item;
                       axis : in     Integer;
                       lo   : in out Real;
                       hi   : in out Real)
   is
      use impact.d3.Scalar;
   begin
      if axis < 3 then
         Self.m_linearLimits.m_lowerLimit (axis) := lo;
         Self.m_linearLimits.m_upperLimit (axis) := hi;
      else
         lo := btNormalizeAngle (lo);
         hi := btNormalizeAngle (hi);

         Self.m_angularLimits (axis - 3).m_loLimit := lo;
         Self.m_angularLimits (axis - 3).m_hiLimit := hi;
      end if;
   end setLimit;

   --      //first 3 are linear, next 3 are angular
   --      void setLimit(int axis, impact.d3.Scalar lo, impact.d3.Scalar hi)
   --      {
   --              if(axis<3)
   --              {
   --                      m_linearLimits.m_lowerLimit[axis] = lo;
   --                      m_linearLimits.m_upperLimit[axis] = hi;
   --              }
   --              else
   --              {
   --                          lo = btNormalizeAngle(lo);
   --                          hi = btNormalizeAngle(hi);
   --                      m_angularLimits[axis-3].m_loLimit = lo;
   --                      m_angularLimits[axis-3].m_hiLimit = hi;
   --              }
   --      }





   function isLimited (Self        : in Item;
                       limitIndex : in Integer) return Boolean
   is
   begin
      if limitIndex < 3 then
         return isLimited (Self.m_linearLimits, limitIndex);
      else
         return isLimited (Self.m_angularLimits (limitIndex - 3));
      end if;
   end isLimited;
   --
   --  Test limit
   --      - free    means upper < lower,
   --      - locked  means upper = lower
   --      - limited means upper > lower
   --
   --      - limitIndex: first 3 are linear, next 3 are angular



--      bool        isLimited(int limitIndex)
--      {
--              if(limitIndex<3)
--              {
--                          return m_linearLimits.isLimited(limitIndex);
--
--              }
--          return m_angularLimits[limitIndex-3].isLimited();
--      }






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
                                   rotAllowed : in Boolean := False) return Integer
   is
      use impact.d3.Containers;
      use impact.d3.Containers.real_Pointers;

      use math.Vectors;
      use type c.ptrdiff_t;

      srow    : constant C.ptrdiff_t := C.ptrdiff_t (row) * info.rowskip;
      powered : Boolean     := limot.m_enableMotor;
      limit   : constant Integer     := limot.m_currentLimit;

   begin
      if        powered        -- if the joint is powered, or has joint limits, add in the extra row
        or else limit /= 0
      then
         declare
            J1 : constant Real_Pointer := (if rotational then info.m_J1angularAxis else info.m_J1linearAxis);
            J2 : constant Real_Pointer := (if rotational then info.m_J2angularAxis else null);
         begin
            Real_Pointer (J1 + srow + 0).all := ax1 (1);
            Real_Pointer (J1 + srow + 1).all := ax1 (2);
            Real_Pointer (J1 + srow + 2).all := ax1 (3);

            if rotational then
               Real_Pointer (J2 + srow + 0).all := -ax1 (1);
               Real_Pointer (J2 + srow + 1).all := -ax1 (2);
               Real_Pointer (J2 + srow + 2).all := -ax1 (3);
            end if;

            if not rotational then

               if Self.m_useOffsetForConstraintFrame then
                  declare
                     use impact.d3.Vector;
                     tmpA,
                     tmpB   : math.Vector_3;

                     --  prepare body B
                     relB   : math.Vector_3 :=   getOrigin (Self.m_calculatedTransformB)    -- get vector from bodyB to frameB in WCS
                                               - getOrigin (transB);

                     projB  : constant math.Vector_3 := ax1  *  dot (relB, ax1);                    -- get its projection to constraint axis
                     orthoB : constant math.Vector_3 := relB - projB;                               -- get vector directed from bodyB to constraint axis (and orthogonal to it)

                     --  same for body A
                     relA   : math.Vector_3 :=   getOrigin (Self.m_calculatedTransformA)
                                               - getOrigin (transA);
                     projA  : constant math.Vector_3 := ax1  *  dot (relA, ax1);
                     orthoA : constant math.Vector_3 := relA - projA;


                     desiredOffs : constant math.Real :=   limot.m_currentPosition       -- get desired offset between frames A and B along constraint axis
                                                - limot.m_currentLimitError;

                     --  desired vector from projection of center of bodyA to projection of center of bodyB to constraint axis
                     --
                     totalDist   : constant math.Vector_3 :=   projA
                                                    + ax1 * desiredOffs
                                                    - projB;
                  begin
                     --  get offset vectors relA and relB
                     relA := orthoA + totalDist * Self.m_factA;
                     relB := orthoB - totalDist * Self.m_factB;

                     tmpA := cross (relA, ax1);
                     tmpB := cross (relB, ax1);

                     if          Self.m_hasStaticBody
                       and then not rotAllowed
                     then
                        tmpA := tmpA * Self.m_factA;
                        tmpB := tmpB * Self.m_factB;
                     end if;

                     for i in C.ptrdiff_t'(1) .. 3
                     loop
                        Real_Pointer (info.m_J1angularAxis + srow + i).all :=  tmpA (Integer (i));
                     end loop;

                     for i in C.ptrdiff_t'(1) .. 3
                     loop
                        Real_Pointer (info.m_J2angularAxis  + srow + i).all := -tmpB (Integer (i));
                     end loop;
                  end;

               else
                  declare
                     use impact.d3.Vector;
                     c   : math.Vector_3 := getOrigin (Self.m_calculatedTransformB) - getOrigin (transA);
                     ltd : math.Vector_3 := cross (c, ax1);                                                 -- Linear Torque Decoupling vector
                  begin
                     Real_Pointer (info.m_J1angularAxis + srow + 0).all := ltd (1);
                     Real_Pointer (info.m_J1angularAxis + srow + 1).all := ltd (2);
                     Real_Pointer (info.m_J1angularAxis + srow + 2).all := ltd (3);

                     c   := getOrigin (Self.m_calculatedTransformB) - getOrigin (transB);
                     ltd := -cross (c, ax1);

                     Real_Pointer (info.m_J2angularAxis + srow + 0).all := ltd (1);
                     Real_Pointer (info.m_J2angularAxis + srow + 1).all := ltd (2);
                     Real_Pointer (info.m_J2angularAxis + srow + 2).all := ltd (3);
                  end;
               end if;
            end if;
         end;


         --  if we're limited low and high simultaneously, the joint motor is ineffective
         --
         if         limit /= 0
           and then limot.m_loLimit = limot.m_hiLimit
         then
            powered := False;
         end if;

         Real_Pointer (info.m_constraintError + srow).all := 0.0;


         if powered then
            Real_Pointer (info.cfm + srow).all := limot.m_normalCFM;

            if limit = 0 then
               declare
                  tag_vel  : constant math.Real := (if rotational then limot.m_targetVelocity else -limot.m_targetVelocity);

                  mot_fact : constant math.Real := Self.getMotorFactor (limot.m_currentPosition,
                                                               limot.m_loLimit,
                                                               limot.m_hiLimit,
                                                               tag_vel,
                                                               info.fps * limot.m_stopERP);
               begin
                  Real_Pointer (info.m_constraintError + srow).all :=   Real_Pointer (info.m_constraintError + srow).all
                                                                      + mot_fact * limot.m_targetVelocity;
                  Real_Pointer (info.m_lowerLimit      + srow).all := -limot.m_maxMotorForce;
                  Real_Pointer (info.m_upperLimit      + srow).all :=  limot.m_maxMotorForce;
               end;
            end if;
         end if;


         if limit /= 0 then
            declare
               k : constant math.Real := info.fps * limot.m_stopERP;
            begin
               if not rotational then
                  Real_Pointer (info.m_constraintError + srow).all := Real_Pointer (info.m_constraintError + srow).all  +   k * limot.m_currentLimitError;
               else
                  Real_Pointer (info.m_constraintError + srow).all := Real_Pointer (info.m_constraintError + srow).all  + (-k * limot.m_currentLimitError);
               end if;
            end;

            Real_Pointer (info.cfm + srow).all := limot.m_stopCFM;

            if limot.m_loLimit = limot.m_hiLimit then
               Real_Pointer (info.m_lowerLimit + srow).all := -impact.d3.Scalar.SIMD_INFINITY;                  -- limited low and high simultaneously
               Real_Pointer (info.m_upperLimit + srow).all :=  impact.d3.Scalar.SIMD_INFINITY;

            else
               if limit = 1 then
                  Real_Pointer (info.m_lowerLimit + srow).all := 0.0;
                  Real_Pointer (info.m_upperLimit + srow).all :=  impact.d3.Scalar.SIMD_INFINITY;
               else
                  Real_Pointer (info.m_lowerLimit + srow).all := -impact.d3.Scalar.SIMD_INFINITY;
                  Real_Pointer (info.m_upperLimit + srow).all := 0.0;
               end if;



               if limot.m_bounce > 0.0 then   -- deal with bounce
                  declare
                     use impact.d3.Vector;
                     vel  : math.Real;   -- calculate joint velocity
                     newc : math.Real;
                  begin
                     if rotational then
                        vel := dot (angVelA, ax1);
                        --  make sure that if no body -> angVelB == zero vec
                        --  //                     if (body1)
                        vel := vel - dot (angVelB, ax1);

                     else
                        vel := dot (linVelA, ax1);
                        --  make sure that if no body -> angVelB == zero vec
                        --  //                       if (body1)
                        vel := vel - dot (linVelB, ax1);
                     end if;

                    -- only apply bounce if the velocity is incoming, and if the
                    -- resulting c[] exceeds what we already have.
                    --
                     if limit = 1 then

                        if vel < 0.0 then
                           newc := -limot.m_bounce * vel;

                           if newc > Real_Pointer (info.m_constraintError + srow).all then
                              Real_Pointer (info.m_constraintError + srow).all := newc;
                           end if;
                        end if;

                     else
                        if vel > 0.0 then
                           newc := -limot.m_bounce * vel;

                           if newc < Real_Pointer (info.m_constraintError + srow).all then
                              Real_Pointer (info.m_constraintError + srow).all := newc;
                           end if;
                        end if;
                     end if;
                  end;
               end if;

            end if;

         end if;

         return 1;

      else
         return 0;
      end if;

   end get_limit_motor_info2;



--  int impact.d3.Joint.any::get_limit_motor_info2(
--          btRotationalLimitMotor * limot,
--          const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& linVelA,const impact.d3.Vector& linVelB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB,
--          btConstraintInfo2 *info, int row, impact.d3.Vector& ax1, int rotational,int rotAllowed)
--  {
--      int srow = row * info->rowskip;
--      int powered = limot->m_enableMotor;
--      int limit = limot->m_currentLimit;
--      if (powered || limit)
--      {   // if the joint is powered, or has joint limits, add in the extra row
--          impact.d3.Scalar *J1 = rotational ? info->m_J1angularAxis : info->m_J1linearAxis;
--          impact.d3.Scalar *J2 = rotational ? info->m_J2angularAxis : 0;
--          J1[srow+0] = ax1[0];
--          J1[srow+1] = ax1[1];
--          J1[srow+2] = ax1[2];
--          if(rotational)
--          {
--              J2[srow+0] = -ax1[0];
--              J2[srow+1] = -ax1[1];
--              J2[srow+2] = -ax1[2];
--          }
--          if((!rotational))
--          {
--                          if (m_useOffsetForConstraintFrame)
--                          {
--                                  impact.d3.Vector tmpA, tmpB, relA, relB;
--                                  // get vector from bodyB to frameB in WCS
--                                  relB = m_calculatedTransformB.getOrigin() - transB.getOrigin();
--                                  // get its projection to constraint axis
--                                  impact.d3.Vector projB = ax1 * relB.dot(ax1);
--                                  // get vector directed from bodyB to constraint axis (and orthogonal to it)
--                                  impact.d3.Vector orthoB = relB - projB;
--                                  // same for bodyA
--                                  relA = m_calculatedTransformA.getOrigin() - transA.getOrigin();
--                                  impact.d3.Vector projA = ax1 * relA.dot(ax1);
--                                  impact.d3.Vector orthoA = relA - projA;
--                                  // get desired offset between frames A and B along constraint axis
--                                  impact.d3.Scalar desiredOffs = limot->m_currentPosition - limot->m_currentLimitError;
--                                  // desired vector from projection of center of bodyA to projection of center of bodyB to constraint axis
--                                  impact.d3.Vector totalDist = projA + ax1 * desiredOffs - projB;
--                                  // get offset vectors relA and relB
--                                  relA = orthoA + totalDist * m_factA;
--                                  relB = orthoB - totalDist * m_factB;
--                                  tmpA = relA.cross(ax1);
--                                  tmpB = relB.cross(ax1);
--                                  if(m_hasStaticBody && (!rotAllowed))
--                                  {
--                                          tmpA *= m_factA;
--                                          tmpB *= m_factB;
--                                  }
--                                  int i;
--                                  for (i=0; i<3; i++) info->m_J1angularAxis[srow+i] = tmpA[i];
--                                  for (i=0; i<3; i++) info->m_J2angularAxis[srow+i] = -tmpB[i];
--                          } else
--                          {
--                                  impact.d3.Vector ltd;        // Linear Torque Decoupling vector
--                                  impact.d3.Vector c = m_calculatedTransformB.getOrigin() - transA.getOrigin();
--                                  ltd = c.cross(ax1);
--                                  info->m_J1angularAxis[srow+0] = ltd[0];
--                                  info->m_J1angularAxis[srow+1] = ltd[1];
--                                  info->m_J1angularAxis[srow+2] = ltd[2];
--
--                                  c = m_calculatedTransformB.getOrigin() - transB.getOrigin();
--                                  ltd = -c.cross(ax1);
--                                  info->m_J2angularAxis[srow+0] = ltd[0];
--                                  info->m_J2angularAxis[srow+1] = ltd[1];
--                                  info->m_J2angularAxis[srow+2] = ltd[2];
--                          }
--          }
--          // if we're limited low and high simultaneously, the joint motor is
--          // ineffective
--          if (limit && (limot->m_loLimit == limot->m_hiLimit)) powered = 0;
--          info->m_constraintError[srow] = impact.d3.Scalar(0.f);
--          if (powered)
--          {
--                          info->cfm[srow] = limot->m_normalCFM;
--              if(!limit)
--              {
--                                  impact.d3.Scalar tag_vel = rotational ? limot->m_targetVelocity : -limot->m_targetVelocity;
--
--                                  impact.d3.Scalar mot_fact = getMotorFactor(        limot->m_currentPosition,
--                                                                                                          limot->m_loLimit,
--                                                                                                          limot->m_hiLimit,
--                                                                                                          tag_vel,
--                                                                                                          info->fps * limot->m_stopERP);
--                                  info->m_constraintError[srow] += mot_fact * limot->m_targetVelocity;
--                  info->m_lowerLimit[srow] = -limot->m_maxMotorForce;
--                  info->m_upperLimit[srow] = limot->m_maxMotorForce;
--              }
--          }
--          if(limit)
--          {
--              impact.d3.Scalar k = info->fps * limot->m_stopERP;
--                          if(!rotational)
--                          {
--                                  info->m_constraintError[srow] += k * limot->m_currentLimitError;
--                          }
--                          else
--                          {
--                                  info->m_constraintError[srow] += -k * limot->m_currentLimitError;
--                          }
--                          info->cfm[srow] = limot->m_stopCFM;
--              if (limot->m_loLimit == limot->m_hiLimit)
--              {   // limited low and high simultaneously
--                  info->m_lowerLimit[srow] = -SIMD_INFINITY;
--                  info->m_upperLimit[srow] = SIMD_INFINITY;
--              }
--              else
--              {
--                  if (limit == 1)
--                  {
--                      info->m_lowerLimit[srow] = 0;
--                      info->m_upperLimit[srow] = SIMD_INFINITY;
--                  }
--                  else
--                  {
--                      info->m_lowerLimit[srow] = -SIMD_INFINITY;
--                      info->m_upperLimit[srow] = 0;
--                  }
--                  // deal with bounce
--                  if (limot->m_bounce > 0)
--                  {
--                      // calculate joint velocity
--                      impact.d3.Scalar vel;
--                      if (rotational)
--                      {
--                          vel = angVelA.dot(ax1);
--  //make sure that if no body -> angVelB == zero vec
--  //                        if (body1)
--                              vel -= angVelB.dot(ax1);
--                      }
--                      else
--                      {
--                          vel = linVelA.dot(ax1);
--  //make sure that if no body -> angVelB == zero vec
--  //                        if (body1)
--                              vel -= linVelB.dot(ax1);
--                      }
--                      // only apply bounce if the velocity is incoming, and if the
--                      // resulting c[] exceeds what we already have.
--                      if (limit == 1)
--                      {
--                          if (vel < 0)
--                          {
--                              impact.d3.Scalar newc = -limot->m_bounce* vel;
--                              if (newc > info->m_constraintError[srow])
--                                                                  info->m_constraintError[srow] = newc;
--                          }
--                      }
--                      else
--                      {
--                          if (vel > 0)
--                          {
--                              impact.d3.Scalar newc = -limot->m_bounce * vel;
--                              if (newc < info->m_constraintError[srow])
--                                                                  info->m_constraintError[srow] = newc;
--                          }
--                      }
--                  }
--              }
--          }
--          return 1;
--      }
--      else return 0;
--  }
--
--










   function getUseFrameOffset (Self : in Item) return Boolean
   is
      pragma Unreferenced (Self);
      Stub : Boolean;
   begin
      return Stub;
   end getUseFrameOffset;





   procedure setUseFrameOffset (Self             : in out Item;
                                frameOffsetOnOff : in     Boolean)
   is
   begin
      null;
   end setUseFrameOffset;





   procedure setAxis (Self  : in out Item;
                      axis1 : in     Vector_3;
                      axis2 : in     Vector_3)
   is
      use linear_Algebra_3d, impact.d3.Vector;

      zAxis : constant Vector_3 := normalized (axis1);
      yAxis : constant Vector_3 := normalized (axis2);
      xAxis : constant Vector_3 := cross (yAxis, zAxis);   -- we want right coordinate system

      frameInW : Transform_3d;

   begin
      setIdentity (frameInW);
      setBasis    (frameInW,
                   ((xAxis (1), yAxis (1), zAxis (1)),
                    (xAxis (2), yAxis (2), zAxis (2)),
                    (xAxis (3), yAxis (3), zAxis (3))));

      --  now get constraint frame in local coordinate systems
      --
      Self.m_frameInA := inverse (getRigidBodyA (Self).getCenterOfMassTransform) * frameInW;
      Self.m_frameInB := inverse (getRigidBodyB (Self).getCenterOfMassTransform) * frameInW;

      Self.calculateTransforms;
   end setAxis;

   --  void impact.d3.Joint.any::setAxis(const impact.d3.Vector& axis1,const impact.d3.Vector& axis2)
   --  {
   --          impact.d3.Vector zAxis = axis1.normalized();
   --          impact.d3.Vector yAxis = axis2.normalized();
   --          impact.d3.Vector xAxis = yAxis.cross(zAxis); // we want right coordinate system
   --
   --          impact.d3.Transform frameInW;
   --          frameInW.setIdentity();
   --          frameInW.getBasis().setValue(        xAxis[0], yAxis[0], zAxis[0],
   --                                          xAxis[1], yAxis[1], zAxis[1],
   --                                         xAxis[2], yAxis[2], zAxis[2]);
   --
   --          // now get constraint frame in local coordinate systems
   --          m_frameInA = m_rbA.getCenterOfMassTransform().inverse() * frameInW;
   --          m_frameInB = m_rbB.getCenterOfMassTransform().inverse() * frameInW;
   --
   --          calculateTransforms();
   --  }









   function setAngularLimits (Self       : access Item;
                             info       : in impact.d3.Joint.btConstraintInfo2;
                             row_offset : in Integer;
                             transA     : in Transform_3d;
                             transB     : in Transform_3d;
                             linVelA    : in Vector_3;
                             linVelB    : in Vector_3;
                             angVelA    : in Vector_3;
                              angVelB    : in Vector_3) return Integer
   is
      row : Integer := row_offset;

   begin
      --  solve angular limits
      for i in 1 .. 3
      loop
         if Self.getRotationalLimitMotor (i).needApplyTorques then
            declare
               axis  : constant math.Vector_3 := Self.getAxis (i);
               flags : constant d3.Flags  := Self.m_flags / 2**(((i - 1) + 3) * BT_6DOF_FLAGS_AXIS_SHIFT);

            begin
               if (flags and BT_6DOF_FLAGS_CFM_NORM) = 0 then
                  Self.m_angularLimits (i).m_normalCFM := info.cfm.all;
               end if;

               if (flags and BT_6DOF_FLAGS_CFM_STOP) = 0 then
                  Self.m_angularLimits (i).m_stopCFM := info.cfm.all;
               end if;

               if (flags and BT_6DOF_FLAGS_ERP_STOP) = 0 then
                  Self.m_angularLimits (i).m_stopERP := info.erp;
               end if;

               row := row + Self.get_limit_motor_info2 (Self.getRotationalLimitMotor (i).all,
                                                        transA,  transB,
                                                        linVelA, linVelB,
                                                        angVelA, angVelB,
                                                        info,
                                                        row,
                                                        axis,
                                                        True);
            end;
         end if;
      end loop;


      return row;
   end setAngularLimits;





--  int impact.d3.Joint.any::setAngularLimits(btConstraintInfo2 *info, int row_offset, const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& linVelA,const impact.d3.Vector& linVelB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB)
--  {
--          impact.d3.Joint.any * d6constraint = this;
--          int row = row_offset;
--          //solve angular limits
--          for (int i=0;i<3 ;i++ )
--          {
--                  if(d6constraint->getRotationalLimitMotor(i)->needApplyTorques())
--                  {
--                          impact.d3.Vector axis = d6constraint->getAxis(i);
--                          int flags = m_flags >> ((i + 3) * BT_6DOF_FLAGS_AXIS_SHIFT);
--                          if(!(flags & BT_6DOF_FLAGS_CFM_NORM))
--                          {
--                                  m_angularLimits[i].m_normalCFM = info->cfm[0];
--                          }
--                          if(!(flags & BT_6DOF_FLAGS_CFM_STOP))
--                          {
--                                  m_angularLimits[i].m_stopCFM = info->cfm[0];
--                          }
--                          if(!(flags & BT_6DOF_FLAGS_ERP_STOP))
--                          {
--                                  m_angularLimits[i].m_stopERP = info->erp;
--                          }
--                          row += get_limit_motor_info2(d6constraint->getRotationalLimitMotor(i),
--                                                                                                  transA,transB,linVelA,linVelB,angVelA,angVelB, info,row,axis,1);
--                  }
--          }
--
--          return row;
--  }
--







   function setLinearLimits (Self    : in Item;
                            info    : in impact.d3.Joint.btConstraintInfo2;
                            row     : in Integer;
                            transA  : in Transform_3d;
                            transB  : in Transform_3d;
                            linVelA : in Vector_3;
                            linVelB : in Vector_3;
                            angVelA : in Vector_3;
                             angVelB : in Vector_3) return Integer
   is
      use impact.d3.Matrix;

      the_Row : Integer := Row;

      limot   : aliased btRotationLimitMotor;

      axis    : math.Vector_3;
      flags   : d3.Flags;

   begin
      --  solve linear limits
      --
      for i in 1 .. 3
      loop
         if Self.m_linearLimits.needApplyForce (i) then
            --  re-use rotational motor code
            limot.m_bounce            := 0.0;
            limot.m_currentLimit      := Self.m_linearLimits.m_currentLimit      (i);
            limot.m_currentPosition   := Self.m_linearLimits.m_currentLinearDiff (i);
            limot.m_currentLimitError := Self.m_linearLimits.m_currentLimitError (i);
            limot.m_damping           := Self.m_linearLimits.m_damping;
            limot.m_enableMotor       := Self.m_linearLimits.m_enableMotor (i);
            limot.m_hiLimit           := Self.m_linearLimits.m_upperLimit  (i);
            limot.m_limitSoftness     := Self.m_linearLimits.m_limitSoftness;
            limot.m_loLimit           := Self.m_linearLimits.m_lowerLimit  (i);
            limot.m_maxLimitForce     := 0.0;
            limot.m_maxMotorForce     := Self.m_linearLimits.m_maxMotorForce  (i);
            limot.m_targetVelocity    := Self.m_linearLimits.m_targetVelocity (i);

            axis  := getColumn (getBasis (Self.m_calculatedTransformA),  i);
            flags := Self.m_flags  /  2**((i - 1) * BT_6DOF_FLAGS_AXIS_SHIFT);

            limot.m_normalCFM := (if (flags and BT_6DOF_FLAGS_CFM_NORM) /= 0 then Self.m_linearLimits.m_normalCFM (i) else info.cfm.all);
            limot.m_stopCFM   := (if (flags and BT_6DOF_FLAGS_CFM_STOP) /= 0 then Self.m_linearLimits.m_stopCFM   (i) else info.cfm.all);
            limot.m_stopERP   := (if (flags and BT_6DOF_FLAGS_ERP_STOP) /= 0 then Self.m_linearLimits.m_stopERP   (i) else info.erp);

            if Self.m_useOffsetForConstraintFrame then
               declare
                  indx1      : constant Integer := (i + 0) mod 3 + 1;
                  indx2      : constant Integer := (i + 1) mod 3 + 1;
                  rotAllowed : Boolean := True;                  -- rotations around orthos to current axis
               begin
                  if         Self.m_angularLimits (indx1).m_currentLimit  /= 0
                    and then Self.m_angularLimits (indx2).m_currentLimit  /= 0
                  then
                     rotAllowed := False;
                  end if;

                  the_Row := the_Row + Self.get_limit_motor_info2 (limot,
                                                                   transA,  transB,
                                                                   linVelA, linVelB,
                                                                   angVelA, angVelB,
                                                                   info,
                                                                   the_Row,
                                                                   axis,
                                                                   False,
                                                                   rotAllowed);
               end;

            else
               the_Row := the_Row + Self.get_limit_motor_info2 (limot,
                                                                transA,  transB,
                                                                linVelA, linVelB,
                                                                angVelA, angVelB,
                                                                info,
                                                                the_Row,
                                                                axis,
                                                                False);
            end if;
         end if;
      end loop;


      return the_Row;
   end setLinearLimits;



--  int impact.d3.Joint.any::setLinearLimits(btConstraintInfo2* info, int row, const impact.d3.Transform& transA,const impact.d3.Transform& transB,const impact.d3.Vector& linVelA,const impact.d3.Vector& linVelB,const impact.d3.Vector& angVelA,const impact.d3.Vector& angVelB)
--  {
--  //        int row = 0;
--          //solve linear limits
--          btRotationalLimitMotor limot;
--          for (int i=0;i<3 ;i++ )
--          {
--                  if(m_linearLimits.needApplyForce(i))
--                  { // re-use rotational motor code
--                          limot.m_bounce = impact.d3.Scalar(0.f);
--                          limot.m_currentLimit = m_linearLimits.m_currentLimit[i];
--                          limot.m_currentPosition = m_linearLimits.m_currentLinearDiff[i];
--                          limot.m_currentLimitError  = m_linearLimits.m_currentLimitError[i];
--                          limot.m_damping  = m_linearLimits.m_damping;
--                          limot.m_enableMotor  = m_linearLimits.m_enableMotor[i];
--                          limot.m_hiLimit  = m_linearLimits.m_upperLimit[i];
--                          limot.m_limitSoftness  = m_linearLimits.m_limitSoftness;
--                          limot.m_loLimit  = m_linearLimits.m_lowerLimit[i];
--                          limot.m_maxLimitForce  = impact.d3.Scalar(0.f);
--                          limot.m_maxMotorForce  = m_linearLimits.m_maxMotorForce[i];
--                          limot.m_targetVelocity  = m_linearLimits.m_targetVelocity[i];
--                          impact.d3.Vector axis = m_calculatedTransformA.getBasis().getColumn(i);
--                          int flags = m_flags >> (i * BT_6DOF_FLAGS_AXIS_SHIFT);
--                          limot.m_normalCFM        = (flags & BT_6DOF_FLAGS_CFM_NORM) ? m_linearLimits.m_normalCFM[i] : info->cfm[0];
--                          limot.m_stopCFM                = (flags & BT_6DOF_FLAGS_CFM_STOP) ? m_linearLimits.m_stopCFM[i] : info->cfm[0];
--                          limot.m_stopERP                = (flags & BT_6DOF_FLAGS_ERP_STOP) ? m_linearLimits.m_stopERP[i] : info->erp;
--                          if(m_useOffsetForConstraintFrame)
--                          {
--                                  int indx1 = (i + 1) % 3;
--                                  int indx2 = (i + 2) % 3;
--                                  int rotAllowed = 1; // rotations around orthos to current axis
--                                  if(m_angularLimits[indx1].m_currentLimit && m_angularLimits[indx2].m_currentLimit)
--                                  {
--                                          rotAllowed = 0;
--                                  }
--                                  row += get_limit_motor_info2(&limot, transA,transB,linVelA,linVelB,angVelA,angVelB, info, row, axis, 0, rotAllowed);
--                          }
--                          else
--                          {
--                                  row += get_limit_motor_info2(&limot, transA,transB,linVelA,linVelB,angVelA,angVelB, info, row, axis, 0);
--                          }
--                  }
--          }
--          return row;
--  }
--









   procedure buildLinearJacobian (Self        : in out Item;
                                  jacLinear   :    out impact.d3.jacobian_Entry.item;
                                  normalWorld : in     Vector_3;
                                  pivotAInW   : in     Vector_3;
                                  pivotBInW   : in     Vector_3)
   is
      use impact.d3.jacobian_Entry, math.Vectors;
   begin
      jacLinear := to_jacobian_Entry (transpose (getBasis (Self.getRigidBodyA.getCenterOfMassTransform)),
                                       transpose (getBasis (Self.getRigidBodyB.getCenterOfMassTransform)),
                                       pivotAInW - Self.getRigidBodyA.getCenterOfMassPosition,
                                       pivotBInW - Self.getRigidBodyB.getCenterOfMassPosition,
                                       normalWorld,
                                       Self.getRigidBodyA.getInvInertiaDiagLocal,
                                       Self.getRigidBodyA.getInvMass,
                                       Self.getRigidBodyB.getInvInertiaDiagLocal,
                                       Self.getRigidBodyB.getInvMass);
   end buildLinearJacobian;



--  void impact.d3.Joint.any::buildLinearJacobian(
--          impact.d3.jacobian_Entry & jacLinear,const impact.d3.Vector & normalWorld,
--          const impact.d3.Vector & pivotAInW,const impact.d3.Vector & pivotBInW)
--  {
--          new (&jacLinear) impact.d3.jacobian_Entry(
--          m_rbA.getCenterOfMassTransform().getBasis().transpose(),
--          m_rbB.getCenterOfMassTransform().getBasis().transpose(),
--          pivotAInW - m_rbA.getCenterOfMassPosition(),
--          pivotBInW - m_rbB.getCenterOfMassPosition(),
--          normalWorld,
--          m_rbA.getInvInertiaDiagLocal(),
--          m_rbA.getInvMass(),
--          m_rbB.getInvInertiaDiagLocal(),
--          m_rbB.getInvMass());
--  }
--





   procedure buildAngularJacobian (Self       : in out Item;
                                   jacAngular :    out impact.d3.jacobian_Entry.Item;
                                   jointAxisW : in     Vector_3)
   is
      use impact.d3.jacobian_Entry, math.Vectors;
   begin
      jacAngular := to_jacobian_Entry (jointAxisW,
                                        transpose (getBasis (Self.getRigidBodyA.getCenterOfMassTransform)),
                                        transpose (getBasis (Self.getRigidBodyB.getCenterOfMassTransform)),
                                        Self.getRigidBodyA.getInvInertiaDiagLocal,
                                        Self.getRigidBodyB.getInvInertiaDiagLocal);
   end buildAngularJacobian;



--
--  void impact.d3.Joint.any::buildAngularJacobian(
--          impact.d3.jacobian_Entry & jacAngular,const impact.d3.Vector & jointAxisW)
--  {
--           new (&jacAngular)        impact.d3.jacobian_Entry(jointAxisW,
--                                        m_rbA.getCenterOfMassTransform().getBasis().transpose(),
--                                        m_rbB.getCenterOfMassTransform().getBasis().transpose(),
--                                        m_rbA.getInvInertiaDiagLocal(),
--                                        m_rbB.getInvInertiaDiagLocal());
--
--  }






   procedure calculateLinearInfo (Self : in out Item)
   is
      use math.Vectors;

      unused : Integer;
      pragma Unreferenced (unused);
   begin
      Self.m_calculatedLinearDiff := getOrigin (Self.m_calculatedTransformB) - getOrigin (Self.m_calculatedTransformA);
      Self.m_calculatedLinearDiff := inverse (getBasis (Self.m_calculatedTransformA)) * Self.m_calculatedLinearDiff;

      for I in 1 .. 3 loop
         Self.m_linearLimits.m_currentLinearDiff (I) := Self.m_calculatedLinearDiff (I);
         unused := testLimitValue (Self.m_linearLimits'Access, I, Self.m_calculatedLinearDiff (I));
      end loop;
   end calculateLinearInfo;

   --  void impact.d3.Joint.any::calculateLinearInfo()
   --  {
   --          m_calculatedLinearDiff = m_calculatedTransformB.getOrigin() - m_calculatedTransformA.getOrigin();
   --          m_calculatedLinearDiff = m_calculatedTransformA.getBasis().inverse() * m_calculatedLinearDiff;
   --          for(int i = 0; i < 3; i++)
   --          {
   --                  m_linearLimits.m_currentLinearDiff[i] = m_calculatedLinearDiff[i];
   --                  m_linearLimits.testLimitValue(i, m_calculatedLinearDiff[i]);
   --          }
   --  }





   procedure calculateAngleInfo (Self : in out Item)
   is
      use math.Vectors, impact.d3.Vector;

      unused         : Boolean;
      pragma Unreferenced (unused);
      relative_frame : constant Matrix_3x3 := inverse (getBasis (Self.m_calculatedTransformA)) * getBasis (Self.m_calculatedTransformB);
      axis0          : constant Vector_3 := math.Col (getBasis (Self.m_calculatedTransformB), 1);
      axis2          : constant Vector_3 := math.Col (getBasis (Self.m_calculatedTransformA), 3);
   begin
      unused := MatrixToEulerXYZ (relative_frame, Self.m_calculatedAxisAngleDiff'Access);
      Self.m_calculatedAxis (2) := cross (axis2, axis0);
      Self.m_calculatedAxis (1) := cross (Self.m_calculatedAxis (2), axis2);
      Self.m_calculatedAxis (3) := cross (axis0, Self.m_calculatedAxis (2));

      normalize (Self.m_calculatedAxis (1));
      normalize (Self.m_calculatedAxis (2));
      normalize (Self.m_calculatedAxis (3));
   end calculateAngleInfo;

   --  void impact.d3.Joint.any::calculateAngleInfo()
   --  {
   --          impact.d3.Matrix relative_frame = m_calculatedTransformA.getBasis().inverse()*m_calculatedTransformB.getBasis();
   --          matrixToEulerXYZ(relative_frame,m_calculatedAxisAngleDiff);
   --          // in euler angle mode we do not actually constrain the angular velocity
   --          // along the axes axis[0] and axis[2] (although we do use axis[1]) :
   --          //
   --          //    to get                        constrain w2-w1 along                ...not
   --          //    ------                        ---------------------                ------
   --          //    d(angle[0])/dt = 0        ax[1] x ax[2]                        ax[0]
   --          //    d(angle[1])/dt = 0        ax[1]
   --          //    d(angle[2])/dt = 0        ax[0] x ax[1]                        ax[2]
   --          //
   --          // constraining w2-w1 along an axis 'a' means that a'*(w2-w1)=0.
   --          // to prove the result for angle[0], write the expression for angle[0] from
   --          // GetInfo1 then take the derivative. to prove this for angle[2] it is
   --          // easier to take the euler rate expression for d(angle[2])/dt with respect
   --          // to the components of w and set that to 0.
   --          impact.d3.Vector axis0 = m_calculatedTransformB.getBasis().getColumn(0);
   --          impact.d3.Vector axis2 = m_calculatedTransformA.getBasis().getColumn(2);
   --
   --          m_calculatedAxis[1] = axis2.cross(axis0);
   --          m_calculatedAxis[0] = m_calculatedAxis[1].cross(axis2);
   --          m_calculatedAxis[2] = axis0.cross(m_calculatedAxis[1]);
   --
   --          m_calculatedAxis[0].normalize();
   --          m_calculatedAxis[1].normalize();
   --          m_calculatedAxis[2].normalize();
   --
   --  }

end impact.d3.Joint.any;
