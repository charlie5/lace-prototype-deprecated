with impact.d3.collision.Algorithm.activating,
     impact.d3.Dispatcher,
     impact.d3.collision.manifold_Result,
     impact.d3.collision.create_Func;
with impact.d3.Manifold;
with impact.d3.collision.Algorithm;
with impact.d3.Object;



package impact.d3.collision.Algorithm.activating.sphere_sphere
--
--  impact.d3.collision.Algorithm.activating.sphere_sphere  provides sphere-sphere collision detection.
--
--  Other features are frame-coherency (persistent data) and collision response.
--  Also provides the most basic sample for custom/user impact.d3.collision.Algorithm
--
is



   type Item is new impact.d3.collision.Algorithm.activating.item with private;
   type View is access all Item'Class;



   --- Forge
   --

   function  to_sphere_sphere_Algorithm (mf         : access impact.d3.Manifold.Item;
                                         ci         : in     AlgorithmConstructionInfo;
                                         col0, col1 : access impact.d3.Object.item'Class) return Item;

   function  to_sphere_sphere_Algorithm (ci         : in     AlgorithmConstructionInfo) return Item;


--          impact.d3.collision.Algorithm.activating.sphere_sphere(const impact.d3.collision.AlgorithmConstructionInfo& ci)
--                  : impact.d3.collision.Algorithm.activating(ci) {}


   overriding procedure destruct (Self : in out Item);






   --- Attributes
   --

   overriding
   procedure processCollision (Self : in out Item;   col0, col1   : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                     resultOut    :    out impact.d3.collision.manifold_Result.item);



   overriding
   function  calculateTimeOfImpact        (Self : in     Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                                 dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                                 resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real;


   overriding
   procedure getAllContactManifolds       (Self : in out Item;   manifoldArray : out impact.d3.collision.Algorithm.btManifoldArray);





   --- Create Functions
   --

   type CreateFunc is new create_Func.item with null record;

   overriding
   function CreateCollisionAlgorithm (Self : in CreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                                              body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view;








private


   type Item is new impact.d3.collision.Algorithm.activating.item with
      record
        m_ownManifold :        Boolean;
         m_manifoldPtr : access impact.d3.Manifold.item;
      end record;


end impact.d3.collision.Algorithm.activating.sphere_sphere;
