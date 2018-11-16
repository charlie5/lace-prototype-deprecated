with Interfaces;


package impact.d3.aabb_Util
--
--
--
is
   use Math;

   type    Signs  is array (1 .. 3) of interfaces.unsigned_32;
   subtype Bounds is Vector_3_array (1 .. 2);



   procedure Transform_Aabb (halfExtents : in     math.Vector_3;
                              margin      : in     math.Real;
                              t           : in     Transform_3d;
                              aabbMinOut,
                              aabbMaxOut  :    out math.Vector_3);


   procedure Transform_Aabb (localAabbMin,
                              localAabbMax : in     math.Vector_3;
                              margin       : in     math.Real;
                              t            : in     Transform_3d;
                              aabbMinOut,
                              aabbMaxOut   :    out math.Vector_3);







   function btRayAabb2 (rayFrom, rayInvDirection : in     math.Vector_3;
                        raySign                  : in     Signs;
                        bounds                   : in     impact.d3.aabb_Util.Bounds;
                        tmin                     : access math.Real;
                        lambda_min, lambda_max   : in     math.Real) return Boolean;


   function btRayAabb (rayFrom, rayTo            : in     math.Vector_3;
                       aabbMin, aabbMax          : in     math.Vector_3;
                       param                     : access math.Real;
                       normal                    : access math.Vector_3) return Boolean;




   function btOutcode (p, halfExtent             : in     math.Vector_3) return Interfaces.Unsigned_32;





   function TestTriangleAgainstAabb2 (vertices         : in math.Matrix_3x3;
                                      aabbMin, aabbMax : in math.Vector_3) return Boolean;
   --
   --  Conservative test for overlap between triangle and aabb.






end impact.d3.aabb_Util;




--  SIMD_FORCE_INLINE void AabbExpand (impact.d3.Vector& aabbMin,
--                                                                     impact.d3.Vector& aabbMax,
--                                                                     const impact.d3.Vector& expansionMin,
--                                                                     const impact.d3.Vector& expansionMax)
--  {
--          aabbMin = aabbMin + expansionMin;
--          aabbMax = aabbMax + expansionMax;
--  }




--  /// conservative test for overlap between two aabbs
--  SIMD_FORCE_INLINE bool TestPointAgainstAabb2(const impact.d3.Vector &aabbMin1, const impact.d3.Vector &aabbMax1,
--                                                                  const impact.d3.Vector &point)
--  {
--          bool overlap = true;
--          overlap = (aabbMin1.getX() > point.getX() || aabbMax1.getX() < point.getX()) ? false : overlap;
--          overlap = (aabbMin1.getZ() > point.getZ() || aabbMax1.getZ() < point.getZ()) ? false : overlap;
--          overlap = (aabbMin1.getY() > point.getY() || aabbMax1.getY() < point.getY()) ? false : overlap;
--          return overlap;
--  }




--
--  /// conservative test for overlap between two aabbs
--  SIMD_FORCE_INLINE bool TestAabbAgainstAabb2(const impact.d3.Vector &aabbMin1, const impact.d3.Vector &aabbMax1,
--                                                                  const impact.d3.Vector &aabbMin2, const impact.d3.Vector &aabbMax2)
--  {
--          bool overlap = true;
--          overlap = (aabbMin1.getX() > aabbMax2.getX() || aabbMax1.getX() < aabbMin2.getX()) ? false : overlap;
--          overlap = (aabbMin1.getZ() > aabbMax2.getZ() || aabbMax1.getZ() < aabbMin2.getZ()) ? false : overlap;
--          overlap = (aabbMin1.getY() > aabbMax2.getY() || aabbMax1.getY() < aabbMin2.getY()) ? false : overlap;
--          return overlap;
--  }





--  #define USE_BANCHLESS 1
--  #ifdef USE_BANCHLESS
--          //This block replaces the block below and uses no branches, and replaces the 8 bit return with a 32 bit return for improved performance (~3x on XBox 360)
--          SIMD_FORCE_INLINE unsigned testQuantizedAabbAgainstQuantizedAabb(const unsigned short int* aabbMin1,const unsigned short int* aabbMax1,const unsigned short int* aabbMin2,const unsigned short int* aabbMax2)
--          {
--                  return static_cast<unsigned int>(btSelect((unsigned)((aabbMin1[0] <= aabbMax2[0]) & (aabbMax1[0] >= aabbMin2[0])
--                          & (aabbMin1[2] <= aabbMax2[2]) & (aabbMax1[2] >= aabbMin2[2])
--                          & (aabbMin1[1] <= aabbMax2[1]) & (aabbMax1[1] >= aabbMin2[1])),
--                          1, 0));
--          }
--  #else
--          SIMD_FORCE_INLINE bool testQuantizedAabbAgainstQuantizedAabb(const unsigned short int* aabbMin1,const unsigned short int* aabbMax1,const unsigned short int* aabbMin2,const unsigned short int* aabbMax2)
--          {
--                  bool overlap = true;
--                  overlap = (aabbMin1[0] > aabbMax2[0] || aabbMax1[0] < aabbMin2[0]) ? false : overlap;
--                  overlap = (aabbMin1[2] > aabbMax2[2] || aabbMax1[2] < aabbMin2[2]) ? false : overlap;
--                  overlap = (aabbMin1[1] > aabbMax2[1] || aabbMax1[1] < aabbMin2[1]) ? false : overlap;
--                  return overlap;
--          }
--  #endif //USE_BANCHLESS


