with impact.d3.Transform,
     impact.d3.Matrix,
     impact.d3.Vector;
with impact.d3.min_max;




package body impact.d3.aabb_Util
is


   procedure Transform_Aabb (halfExtents : in     math.Vector_3;
                              margin      : in     math.Real;
                              t           : in     Transform_3d;
                              aabbMinOut,
                              aabbMaxOut  :    out math.Vector_3)
   is
      use impact.d3.Transform, impact.d3.Matrix, impact.d3.Vector;

      halfExtentsWithMargin : constant math.Vector_3   := halfExtents + (margin, margin, margin);
      abs_b                 : constant math.Matrix_3x3 := absolute (getBasis (t));

      center : constant math.Vector_3 := getOrigin (t);
      extent : constant math.Vector_3 := (dot (getRow (abs_b, 1), halfExtentsWithMargin),
                                 dot (getRow (abs_b, 2), halfExtentsWithMargin),
                                 dot (getRow (abs_b, 3), halfExtentsWithMargin));
   begin
      aabbMinOut := center - extent;
      aabbMaxOut := center + extent;
   end Transform_Aabb;





   procedure Transform_Aabb (localAabbMin,
                              localAabbMax : in     math.Vector_3;
                              margin       : in     math.Real;
                              t            : in     Transform_3d;
                              aabbMinOut,
                              aabbMaxOut   :    out math.Vector_3)
   is
      use linear_Algebra_3d, impact.d3.Transform, impact.d3.Matrix, impact.d3.Vector;

      pragma Assert (localAabbMin (1) <= localAabbMax (1));
      pragma Assert (localAabbMin (2) <= localAabbMax (2));
      pragma Assert (localAabbMin (3) <= localAabbMax (3));

      localHalfExtents : constant math.Vector_3 := 0.5 * (localAabbMax - localAabbMin)  +  (margin, margin, margin);
      localCenter      : constant math.Vector_3 := 0.5 * (localAabbMax + localAabbMin);

      abs_b : constant math.Matrix_3x3 := absolute (getBasis (t));

      center : constant math.Vector_3 := t * localCenter;
      extent : constant math.Vector_3 := (dot (getRow (abs_b, 1), localHalfExtents),
                                 dot (getRow (abs_b, 2), localHalfExtents),
                                 dot (getRow (abs_b, 3), localHalfExtents));
   begin
      aabbMinOut := center - extent;
      aabbMaxOut := center + extent;
   end Transform_Aabb;







   function btRayAabb2 (rayFrom, rayInvDirection : in     math.Vector_3;
                        raySign                  : in     Signs;
                        bounds                   : in     impact.d3.aabb_Util.Bounds;
                        tmin                     : access math.Real;
                        lambda_min, lambda_max   : in     math.Real) return Boolean
   is
      use type interfaces.unsigned_32;
      tmax,
      tymin, tymax,
      tzmin, tzmax : math.Real;

   begin
      tmin.all := (bounds (  Integer (raySign (1))) (1)  -  rayFrom (1)) * rayInvDirection (1);
      tmax     := (bounds (1 - Integer (raySign (1))) (1)  -  rayFrom (1)) * rayInvDirection (1);
      tymin    := (bounds (  Integer (raySign (2))) (2)  -  rayFrom (2)) * rayInvDirection (2);
      tymax    := (bounds (1 - Integer (raySign (2))) (2)  -  rayFrom (2)) * rayInvDirection (2);


      if (tmin.all > tymax)  or else  (tymin > tmax) then
         return False;
      end if;

      if tymin > tmin.all then
         tmin.all := tymin;
      end if;

      if tymax < tmax then
         tmax := tymax;
      end if;


      tzmin := (bounds (  Integer (raySign (3))) (3) - rayFrom (3))  *  rayInvDirection (3);
      tzmax := (bounds (1 - Integer (raySign (3))) (3) - rayFrom (3))  *  rayInvDirection (3);


      if (tmin.all > tzmax)  or else  (tzmin > tmax) then
         return False;
      end if;

      if tzmin > tmin.all then
         tmin.all := tzmin;
      end if;

      if tzmax < tmax then
         tmax := tzmax;
      end if;


      return     tmin.all < lambda_max
        and then tmax     > lambda_min;
   end btRayAabb2;








   function btRayAabb  (rayFrom, rayTo           : in     math.Vector_3;
                        aabbMin, aabbMax         : in     math.Vector_3;
                        param                    : access math.Real;
                        normal                   : access math.Vector_3) return Boolean
   is
      use Interfaces;

      aabbHalfExtent : math.Vector_3 := (aabbMax - aabbMin) * 0.5;
      aabbCenter     : constant math.Vector_3 := (aabbMax + aabbMin) * 0.5;

      source         : math.Vector_3 := rayFrom - aabbCenter;
      target         : constant math.Vector_3 := rayTo   - aabbCenter;

      sourceOutcode  : constant Unsigned_32    := btOutcode (source, aabbHalfExtent);
      targetOutcode  : constant Unsigned_32    := btOutcode (target, aabbHalfExtent);

   begin
      if (sourceOutcode and targetOutcode) = 0 then
         declare
            use impact.d3.min_max;

            lambda_enter : math.Real     := 0.0;
            lambda_exit  : math.Real     := param.all;

            r            : math.Vector_3 := target - source;

            normSign     : math.Real     := 1.0;
            hitNormal    : math.Vector_3 := (0.0, 0.0, 0.0);
            bit          : Unsigned_32   := 1;

            lambda       : math.Real;

         begin
            for j in 1 .. 3
            loop

               for i in 1 .. 3
               loop
                  if (sourceOutcode and bit) /= 0 then
                     lambda := (-source (i)  -  aabbHalfExtent (i) * normSign) / r (i);

                     if lambda_enter <= lambda then
                        lambda_enter  := lambda;
                        hitNormal     := (0.0, 0.0, 0.0);
                        hitNormal (i) := normSign;
                     end if;

                  elsif (targetOutcode and bit) /= 0 then
                     lambda := (-source (i)  -  aabbHalfExtent (i) * normSign) / r (i);
                     btSetMin (lambda_exit, lambda);
                  end if;


                  bit := bit * 2;
               end loop;

               normSign := -1.0;
            end loop;


            if lambda_enter <= lambda_exit then
               param.all  := lambda_enter;
               normal.all := hitNormal;

               return True;
            end if;

         end;
      end if;


      return False;
   end btRayAabb;





--  SIMD_FORCE_INLINE bool btRayAabb(const impact.d3.Vector& rayFrom,
--                                                                   const impact.d3.Vector& rayTo,
--                                                                   const impact.d3.Vector& aabbMin,
--                                                                   const impact.d3.Vector& aabbMax,
--                                            impact.d3.Scalar& param, impact.d3.Vector& normal)
--  {
--          impact.d3.Vector aabbHalfExtent = (aabbMax-aabbMin)* impact.d3.Scalar(0.5);
--          impact.d3.Vector aabbCenter = (aabbMax+aabbMin)* impact.d3.Scalar(0.5);
--          impact.d3.Vector        source = rayFrom - aabbCenter;
--          impact.d3.Vector        target = rayTo - aabbCenter;
--          int        sourceOutcode = btOutcode(source,aabbHalfExtent);
--          int targetOutcode = btOutcode(target,aabbHalfExtent);
--          if ((sourceOutcode & targetOutcode) == 0x0)
--          {
--                  impact.d3.Scalar lambda_enter = impact.d3.Scalar(0.0);
--                  impact.d3.Scalar lambda_exit  = param;
--                  impact.d3.Vector r = target - source;
--                  int i;
--                  impact.d3.Scalar        normSign = 1;
--                  impact.d3.Vector        hitNormal(0,0,0);
--                  int bit=1;
--
--                  for (int j=0;j<2;j++)
--                  {
--                          for (i = 0; i != 3; ++i)
--                          {
--                                  if (sourceOutcode & bit)
--                                  {
--                                          impact.d3.Scalar lambda = (-source[i] - aabbHalfExtent[i]*normSign) / r[i];
--                                          if (lambda_enter <= lambda)
--                                          {
--                                                  lambda_enter = lambda;
--                                                  hitNormal.setValue(0,0,0);
--                                                  hitNormal[i] = normSign;
--                                          }
--                                  }
--                                  else if (targetOutcode & bit)
--                                  {
--                                          impact.d3.Scalar lambda = (-source[i] - aabbHalfExtent[i]*normSign) / r[i];
--                                          btSetMin(lambda_exit, lambda);
--                                  }
--                                  bit<<=1;
--                          }
--                          normSign = impact.d3.Scalar(-1.);
--                  }
--                  if (lambda_enter <= lambda_exit)
--                  {
--                          param = lambda_enter;
--                          normal = hitNormal;
--                          return true;
--                  }
--          }
--          return false;
--  }
--
--
--







   function btOutcode (p, halfExtent : in math.Vector_3) return Interfaces.Unsigned_32
   is
      use type interfaces.Unsigned_32;

      X_lt, X_gt,
      Y_lt, Y_gt,
      Z_lt, Z_gt : Interfaces.Unsigned_32;

   begin
      if p (1) < -halfExtent (1) then   X_lt := 16#01#;
      else   X_lt := 16#00#;
      end if;

      if p (1) >  halfExtent (1) then   X_gt := 16#08#;
      else   X_gt := 16#00#;
      end if;


      if p (2) < -halfExtent (2) then   Y_lt := 16#02#;
      else   Y_lt := 16#00#;
      end if;

      if p (2) >  halfExtent (2) then   Y_gt := 16#10#;
      else   Y_gt := 16#00#;
      end if;


      if p (3) < -halfExtent (3) then   Z_lt := 16#04#;
      else   Z_lt := 16#00#;
      end if;

      if p (3) >  halfExtent (3) then   Z_gt := 16#20#;
      else   Z_gt := 16#00#;
      end if;


      return    X_lt or X_gt
             or Y_lt or Y_gt
             or Z_lt or Z_gt;
   end btOutcode;






   function TestTriangleAgainstAabb2 (vertices         : in math.Matrix_3x3;
                                      aabbMin, aabbMax : in math.Vector_3) return Boolean
   is
      use impact.d3.Matrix;

      p1 : math.Vector_3 renames getRow (vertices, 1);
      p2 : math.Vector_3 renames getRow (vertices, 2);
      p3 : math.Vector_3 renames getRow (vertices, 3);

   begin
      if Real'Min (Real'Min (p1 (1), p2 (1)),  p3 (1))  >  aabbMax (1) then   return False;   end if;
      if Real'Max (Real'Max (p1 (1), p2 (1)),  p3 (1))  <  aabbMin (1) then   return False;   end if;

      if Real'Min (Real'Min (p1 (3), p2 (3)),  p3 (3))  >  aabbMax (3) then   return False;   end if;
      if Real'Max (Real'Max (p1 (3), p2 (3)),  p3 (3))  <  aabbMin (3) then   return False;   end if;

      if Real'Min (Real'Min (p1 (2), p2 (2)),  p3 (2))  >  aabbMax (2) then   return False;   end if;
      if Real'Max (Real'Max (p1 (2), p2 (2)),  p3 (2))  <  aabbMin (2) then   return False;   end if;

      return True;
   end TestTriangleAgainstAabb2;




end impact.d3.aabb_Util;
