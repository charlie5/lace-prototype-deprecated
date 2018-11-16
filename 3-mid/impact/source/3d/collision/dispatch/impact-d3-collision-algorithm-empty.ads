with impact.d3.collision.Algorithm;
with impact.d3.Object;
with impact.d3.Dispatcher,
     impact.d3.collision.manifold_Result,
     impact.d3.collision.create_Func;



package impact.d3.collision.Algorithm.empty
--
--  EmptyAlgorithm is a stub for unsupported collision pairs.
--
--  The dispatcher can dispatch a persistent btEmptyAlgorithm to avoid a search every frame.
--
is



   type Item is new impact.d3.collision.Algorithm.item with private;



   function  to_empty_Algorithm (ci   : in     AlgorithmConstructionInfo) return Item'Class;
   overriding procedure destruct           (Self : in out Item);



   overriding
   procedure processCollision             (Self : in out Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                                 dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                                 resultOut    :    out impact.d3.collision.manifold_Result.item);



   overriding function  calculateTimeOfImpact        (Self : in     Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                                 dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                                 resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real;


   overriding procedure getAllContactManifolds       (Self : in out Item;   manifoldArray :   out impact.d3.collision.Algorithm.btManifoldArray);





   --- Create Functions
   --

   type CreateFunc is new create_Func.item with null record;


   overriding
   function CreateCollisionAlgorithm (Self : in CreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                                              body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view;







private

   type Item is new impact.d3.collision.Algorithm.item with null record;

end impact.d3.collision.Algorithm.empty;
