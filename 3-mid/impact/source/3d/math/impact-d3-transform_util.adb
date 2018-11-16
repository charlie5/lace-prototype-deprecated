with impact.d3.Vector,
     impact.d3.Transform;
with impact.d3.Quaternions;





package body impact.d3.transform_Util
is


   function to_btConvexSeparatingDistanceUtil (boundingRadiusA,
                                               boundingRadiusB : in math.Real) return btConvexSeparatingDistanceUtil
   is
      Self : btConvexSeparatingDistanceUtil;
   begin
      Self.m_boundingRadiusA := boundingRadiusA;
      Self.m_boundingRadiusB := boundingRadiusB;

      Self.m_separatingDistance := 0.0;


      return Self;
   end to_btConvexSeparatingDistanceUtil;





   function  getConservativeSeparatingDistance (Self : in     btConvexSeparatingDistanceUtil) return math.Real
   is
   begin
      return Self.m_separatingDistance;
   end getConservativeSeparatingDistance;





   procedure updateSeparatingDistance (Self : in out btConvexSeparatingDistanceUtil;   transA,
                                       transB : in Transform_3d)
   is
      use impact.d3.Vector, impact.d3.Transform;

      toPosA : math.Vector_3 renames transA.Translation;
      toPosB : math.Vector_3 renames transB.Translation;

      toOrnA : constant Quaternion := getRotation (transA);
      toOrnB : constant Quaternion := getRotation (transB);

   begin
      if Self.m_separatingDistance > 0.0 then
         declare
            linVelA,
            angVelA,
            linVelB,
            angVelB : math.Vector_3;

            maxAngularProjectedVelocity : math.Real;
            relLinVel                   : math.Vector_3;
            relLinVelocLength           : math.Real;

            projectedMotion : math.Real;
         begin
            calculateVelocityQuaternion (Self.m_posA, toPosA,
                                         Self.m_ornA, toOrnA,
                                         1.0,
                                         linVelA, angVelA);
            calculateVelocityQuaternion (Self.m_posB, toPosB,
                                         Self.m_ornB, toOrnB,
                                         1.0,
                                         linVelB, angVelB);

            maxAngularProjectedVelocity := length (angVelA) * Self.m_boundingRadiusA + length (angVelB) * Self.m_boundingRadiusB;
            relLinVel                   := linVelB - linVelA;
            relLinVelocLength           := dot (relLinVel, Self.m_separatingNormal);

            if relLinVelocLength < 0.0 then
               relLinVelocLength := 0.0;
            end if;

            projectedMotion           := maxAngularProjectedVelocity + relLinVelocLength;
            Self.m_separatingDistance := Self.m_separatingDistance   - projectedMotion;
         end;
      end if;

      Self.m_posA := toPosA;
      Self.m_posB := toPosB;
      Self.m_ornA := toOrnA;
      Self.m_ornB := toOrnB;
   end updateSeparatingDistance;





   procedure initSeparatingDistance (Self : in out btConvexSeparatingDistanceUtil;   separatingVector   : in math.Vector_3;
                                                                                              separatingDistance : in math.Real;
                                     transA, transB     : in Transform_3d)
   is
      use impact.d3.Transform;
   begin
      Self.m_separatingDistance := separatingDistance;

      if Self.m_separatingDistance > 0.0 then
         declare
            toPosA : math.Vector_3     renames transA.Translation;
            toPosB : math.Vector_3     renames transB.Translation;
            toOrnA : constant Quaternion :=      getRotation (transA);
            toOrnB : constant Quaternion :=      getRotation (transB);
         begin
            Self.m_separatingNormal := separatingVector;

            Self.m_posA := toPosA;
            Self.m_posB := toPosB;
            Self.m_ornA := toOrnA;
            Self.m_ornB := toOrnB;
         end;
      end if;
   end initSeparatingDistance;










   function btAabbSupport (halfExtents, supportDir : in math.Vector_3) return math.Vector_3
   is
      Result : math.Vector_3;
   begin
      if supportDir (1) < 0.0 then   Result (1) := -halfExtents (1);
      else   Result (1) :=  halfExtents (1);
      end if;

      if supportDir (2) < 0.0 then   Result (2) := -halfExtents (2);
      else   Result (2) :=  halfExtents (2);
      end if;

      if supportDir (3) < 0.0 then   Result (3) := -halfExtents (3);
      else   Result (3) :=  halfExtents (3);
      end if;


      return Result;
   end btAabbSupport;





   --- Utils related to temporal transforms
   --


   procedure integrateTransform (curTrans           : in     Transform_3d;
                                 linvel, angvel     : in     math.Vector_3;
                                 timeStep           : in     math.Real;
                                 predictedTransform :    out Transform_3d)
   is
      use
          impact.d3.Transform,    impact.d3.Vector,        impact.d3.Quaternions,
          math.Functions,         math.Vectors;

      QUATERNION_DERIVATIVE : constant Boolean  := False;
      predictedOrn          : Quaternion := getRotation (curTrans);

   begin
      predictedTransform.Translation := curTrans.Translation  +  linvel * timeStep;


      if QUATERNION_DERIVATIVE then
         predictedOrn := getRotation (curTrans);

         predictedOrn := predictedOrn + (angvel * predictedOrn) * (timeStep * 0.5);
         normalize (predictedOrn);

      else -- Exponential map: google for "Practical Parameterization of Rotations Using the Exponential Map", F. Sebastian Grassia
         declare
            axis   : math.Vector_3;
            fAngle : math.Real    := length (angvel);

            dorn,
            orn0   : Quaternion;

         begin
            if fAngle * timeStep  >  ANGULAR_MOTION_THRESHOLD then   -- limit the angular motion
               fAngle := ANGULAR_MOTION_THRESHOLD / timeStep;
            end if;

            if fAngle < 0.001 then   -- use Taylor's expansions of sync function
               axis   := angvel * (0.5 * timeStep - (timeStep * timeStep * timeStep) * (0.020833333333) * fAngle * fAngle);
            else                     -- sync (fAngle) := sin (c * fAngle) / t
               axis   := angvel * (sin (0.5 * fAngle * timeStep) / fAngle);
            end if;

            dorn := (V => (axis (1), axis (2), axis (3)),
                     R => cos (fAngle * timeStep * 0.5));
            orn0 := getRotation (curTrans);

            predictedOrn := multiply (dorn, orn0);
            normalize (predictedOrn);
         end;
      end if;

      setRotation (predictedTransform, predictedOrn);
   end integrateTransform;






   procedure calculateVelocityQuaternion      (pos0, pos1     : in     math.Vector_3;
                                               orn0, orn1     : in     Quaternion;
                                               timeStep       : in     math.Real;
                                               linvel, angvel :    out math.Vector_3)
   is
      axis  : math.Vector_3;
      angle : math.Real;
   begin
      linVel := (pos1 - pos0) / timeStep;

      if orn0 /= orn1 then
         calculateDiffAxisAngleQuaternion (orn0, orn1, axis, angle);
         angVel := axis * angle / timeStep;
      else
         angVel := (0.0, 0.0, 0.0);
      end if;
   end calculateVelocityQuaternion;






   procedure calculateDiffAxisAngleQuaternion (orn0, orn1a    : in     Quaternion;
                                               axis           :    out math.Vector_3;
                                               angle          :    out math.Real)
   is
      use impact.d3.Quaternions,   impact.d3.Scalar,
          math.Functions, math.Vectors;

      orn1 : constant Quaternion := nearest (orn0, orn1a);
      dorn : constant Quaternion := multiply (orn1,  inverse (orn0));

      len  : math.Real;

      the_Axis : Quaternion;
   begin
      angle    := getAngle (dorn);
      the_axis := (V => dorn.V,
                   R => 0.0);

      --  check for axis length
      --
      len := length2 (the_axis);

      if len  <  SIMD_EPSILON * SIMD_EPSILON then
         axis := (1.0, 0.0, 0.0);
      else
         the_axis := the_axis  /  sqRt (len);
         axis     := the_Axis.V;
      end if;
   end calculateDiffAxisAngleQuaternion;







   procedure calculateVelocity (transform0, transform1 : in     Transform_3d;
                                     timeStep               : in     math.Real;
                                linvel, angvel         :    out math.Vector_3)
   is
      use impact.d3.transform_Util;

      axis  : math.Vector_3;
      angle : math.Real;
   begin
      linVel := (transform1.Translation - transform0.Translation) / timeStep;

      calculateDiffAxisAngle (transform0, transform1,
                              axis,       angle);

      angVel := axis * angle / timeStep;
   end calculateVelocity;






   procedure calculateDiffAxisAngle (transform0, transform1 : in     Transform_3d;
                                     axis                   :    out math.Vector_3;
                                     angle                  :    out math.Real)
   is
      use impact.d3.Transform,       impact.d3.Quaternions,            impact.d3.Scalar,
          math.Vectors,  math.Algebra.linear.d3,  math.Functions;

      dmat     : constant math.Matrix_3x3  := transform1.Rotation * inverse (transform0.Rotation);
      dorn     : Quaternion;

      len      : math.Real;
      the_Axis : Quaternion;

   begin
      dorn     := to_Quaternion (dmat);
      normalize (dorn);               -- floating point inaccuracy can lead to w component > 1..., which breaks

      angle    := getAngle (dorn);
      the_Axis := (V => dorn.V,
                   R => 0.0);

      --  check for axis length
      --
      len := length2 (the_axis);

      if len  <  SIMD_EPSILON * SIMD_EPSILON then
         axis := (1.0, 0.0, 0.0);
      else
         the_Axis := the_Axis / sqRt (len);
         axis     := the_Axis.V;
      end if;
   end calculateDiffAxisAngle;




end impact.d3.transform_Util;
