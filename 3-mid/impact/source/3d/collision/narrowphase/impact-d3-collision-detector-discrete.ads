


package impact.d3.collision.Detector.discrete
--
--  This interface is made to be used by an iterative approach to do TimeOfImpact calculations.
--
--  This interface allows to query for closest points and penetration depth between two (convex) objects
--  the closest point is on the second object (B), and the normal points from the surface on B towards A.
--  distance is between closest points on B and closest point on A. So you can calculate closest point on A
--  by taking closestPointInA = closestPointInB + m_distance * m_normalOnSurfaceB
--
is
   use Math;


   --- Result
   --

   type Result is abstract tagged null record;

   procedure destruct (Self : in out Result) is null;



   --   setShapeIdentifiersA/B provides experimental support for per-triangle material / custom material combiner
   --
   procedure setShapeIdentifiersA (Self : in out Result;   partId0 : in Integer;
                                                           index0  : in Integer) is null;  -- abstract;

   procedure setShapeIdentifiersB (Self : in out Result;   partId1 : in Integer;
                                                           index1  : in Integer) is null;  -- abstract;


   procedure addContactPoint      (Self : in out Result;   normalOnBInWorld : in math.Vector_3;
                                                           pointInWorld     : in math.Vector_3;
                                                           depth            : in math.Real  ) is abstract;




   --- ClosestPointInput
   --

   type ClosestPointInput is
      record
         m_transformA             : aliased Transform_3d;
         m_transformB             : aliased Transform_3d;

         m_maximumDistanceSquared : math.Real := math.Infinity;
      end record;





   --- impact.d3.collision.Detector.discrete
   --

   type Item is abstract tagged null record;

   procedure destruct (Self : in out Item) is null;


   --  Give either closest points (distance > 0) or penetration (distance)
   --  the normal always points from B towards A
   --
   procedure getClosestPoints (Self : in out Item;   input       : in     ClosestPointInput;
                                                     output      : in out Result'Class;
                                                     swapResults : in     Boolean := False)   is abstract;




   --- btStorageResult
   --

   type btStorageResult is new Result with
      record
         m_normalOnSurfaceB : math.Vector_3;
         m_closestPointInB  : math.Vector_3;
         m_distance         : math.Real    := math.Infinity;        -- negative means penetration !
      end record;


   overriding procedure addContactPoint (Self : in out btStorageResult;   normalOnBInWorld : in math.Vector_3;
                                                               pointInWorld     : in math.Vector_3;
                                                               depth            : in math.Real  );




end impact.d3.collision.Detector.discrete;
