with impact.d3.Scalar;



package impact.d3.transform_Util
--
--
--
is
   use Math;



   ANGULAR_MOTION_THRESHOLD : constant math.Real := 0.5 * impact.d3.Scalar.SIMD_HALF_PI;



   --  The btConvexSeparatingDistanceUtil can help speed up convex collision detection
   --  by conservatively updating a cached separating distance/vector instead of re-calculating the closest distance
   --
   type btConvexSeparatingDistanceUtil is tagged private;




   function to_btConvexSeparatingDistanceUtil (boundingRadiusA,
                                               boundingRadiusB : in math.Real) return btConvexSeparatingDistanceUtil;



   function  getConservativeSeparatingDistance (Self : in     btConvexSeparatingDistanceUtil) return math.Real;

   procedure updateSeparatingDistance (Self : in out btConvexSeparatingDistanceUtil;   transA,
                                       transB : in Transform_3d);


   procedure initSeparatingDistance          (Self : in out btConvexSeparatingDistanceUtil;   separatingVector   : in math.Vector_3;
                                                                                              separatingDistance : in math.Real;
                                                                                              transA, transB     : in Transform_3d);







   function btAabbSupport (halfExtents, supportDir : in math.Vector_3) return math.Vector_3;




   --- Utils related to temporal transforms
   --


   procedure integrateTransform     (curTrans           : in     Transform_3d;
                                     linvel, angvel     : in     math.Vector_3;
                                     timeStep           : in     math.Real;
                                     predictedTransform :    out Transform_3d);



   procedure calculateVelocityQuaternion      (pos0, pos1     : in     math.Vector_3;
                                               orn0, orn1     : in     Quaternion;
                                               timeStep       : in     math.Real;
                                               linvel, angvel :    out math.Vector_3);


   procedure calculateDiffAxisAngleQuaternion (orn0, orn1a    : in     Quaternion;
                                               axis           :    out math.Vector_3;
                                               angle          :    out math.Real);



   procedure calculateVelocity      (transform0, transform1 : in     Transform_3d;
                                     timeStep               : in     math.Real;
                                     linvel, angvel         :    out math.Vector_3);


   procedure calculateDiffAxisAngle (transform0, transform1 : in     Transform_3d;
                                     axis                   :    out math.Vector_3;
                                     angle                  :    out math.Real);






private


   type btConvexSeparatingDistanceUtil is tagged
      record
         m_ornA,
         m_ornB               : Quaternion;

         m_posA,
         m_posB               : Vector_3;

         m_separatingNormal   : Vector_3;

         m_boundingRadiusA,
         m_boundingRadiusB,
         m_separatingDistance : Real;
      end record;


end impact.d3.transform_Util;
