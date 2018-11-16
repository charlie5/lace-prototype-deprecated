with
     impact.d3.Dispatcher,
     impact.d3.collision.manifold_Result,
     impact.d3.collision.create_Func;
with impact.d3.Manifold;
with impact.d3.collision.Algorithm;
with impact.d3.Object;



package impact.d3.collision.Algorithm.convex_plane
--
--  Provides Convex-Plane collision detection.
--  Other features are frame-coherency (persistent data) and collision response.
--
is


   type Item is new impact.d3.collision.Algorithm.item with private;




   --- Forge
   --

   function  to_convex_plane_Algorithm (mf                                 : access impact.d3.Manifold.Item;
                                        ci                                 : in     AlgorithmConstructionInfo;
                                        col0, col1                         : access impact.d3.Object.item'Class;
                                        is_Swapped                         : in     Boolean;
                                        numPerturbationIterations,
                                        minimumPointsPerturbationThreshold : in Integer) return Item'Class;

--     function  to_impact.d3.collision.Algorithm.convex_plane (ci         : in     impact.d3.collision.Algorithm.impact.d3.collision.AlgorithmConstructionInfo) return Item'Class;


   overriding procedure destruct (Self : in out Item);






   --- Attributes
   --

   overriding
   procedure processCollision (Self : in out Item;   col0, col1   : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                     resultOut    :    out impact.d3.collision.manifold_Result.item);



   overriding function  calculateTimeOfImpact        (Self : in     Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                                 dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                                 resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real;


   overriding
   procedure getAllContactManifolds       (Self : in out Item;   manifoldArray : out impact.d3.collision.Algorithm.btManifoldArray);


   procedure collideSingleContact (Self : in out Item;   perturbeRot  : in     math.Quaternion;
                                                         body0, body1 : access impact.d3.Object.item'Class;
                                                         dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                         resultOut    : access impact.d3.collision.manifold_Result.item);





   --- Create Functions
   --

   type CreateFunc is new create_Func.item with
      record
         m_numPerturbationIterations          : Integer := 1;
         m_minimumPointsPerturbationThreshold : Integer := 0;
      end record;


   overriding
   function CreateCollisionAlgorithm (Self : in CreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                                              body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view;









private


   type Item is new impact.d3.collision.Algorithm.item with
      record
         m_ownManifold                        : Boolean;
         m_manifoldPtr                        : access impact.d3.Manifold.item;

         m_isSwapped                          : Boolean;

         m_numPerturbationIterations,
         m_minimumPointsPerturbationThreshold : Integer;
      end record;


end impact.d3.collision.Algorithm.convex_plane;
