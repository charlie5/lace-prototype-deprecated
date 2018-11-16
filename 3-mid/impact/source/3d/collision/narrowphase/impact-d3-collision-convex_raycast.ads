
package impact.d3.collision.convex_Raycast
--
--  impact.d3.collision.convex_Raycast is an interface for Casting
--
is
   use Math;



   type Item is abstract tagged private;

   procedure destuct (Self : in out Item)   is null;




   --  CastResult stores the closest result.
   --  Alternatively, add a callback method to decide about closest/all results.
   --
   type CastResult is tagged
      record
         m_hitTransformA,
         m_hitTransformB : Transform_3d;

         m_normal,
         m_hitPoint : math.Vector_3;

         m_fraction           : math.Real := BT_LARGE_FLOAT;                         -- input and output
         m_allowedPenetration : math.Real := 0.0;
      end record;

   --                  //virtual bool        <CastResult::>addRayResult(const impact.d3.Vector& normal,impact.d3.Scalar        fraction) = 0;

   procedure destuct (Self : in out CastResult)   is null;


   procedure reportFailure (Self : in CastResult;   errNo, numIterations : in Integer) is null;



   function calcTimeOfImpact (Self : access Item;   fromA, toA : in     Transform_3d;
                                                    fromB, toB : in     Transform_3d;
                                                    result     : access CastResult'Class) return Boolean
                              is abstract;







private


   type Item is abstract tagged
      record
         null;
      end record;


end impact.d3.collision.convex_Raycast;
