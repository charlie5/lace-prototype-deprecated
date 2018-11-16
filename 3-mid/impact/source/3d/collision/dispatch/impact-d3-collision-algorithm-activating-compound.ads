with impact.d3.collision.Algorithm.activating,
     impact.d3.collision.Algorithm,
     impact.d3.Object,
     impact.d3.collision.create_Func,
     impact.d3.Dispatcher,
     impact.d3.collision.manifold_Result,
     impact.d3.Manifold;



private
with ada.containers.Vectors;



package impact.d3.collision.Algorithm.activating.compound
--
--  impact.d3.collision.Algorithm.activating.compound  supports collision between CompoundCollisionShapes and other collision shapes
--
is


   type Item is new impact.d3.collision.Algorithm.activating.item with private;



   function to_compound_Algorithm (ci           : in     AlgorithmConstructionInfo;
                                   body0, body1 : access impact.d3.Object.item'Class;
                                   isSwapped    : in     Boolean                                                ) return Item'Class;

   overriding procedure destruct (Self : in out Item);




   overriding procedure processCollision (Self : in out Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                     resultOut    :    out impact.d3.collision.manifold_Result.item);



   overriding function calculateTimeOfImpact (Self : in Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                     resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real;

   overriding
   procedure getAllContactManifolds (Self : in out Item;   manifoldArray : out impact.d3.collision.Algorithm.btManifoldArray);


--     procedure clearCache (Self : in out Item);





   --- Create Functions
   --

   type CreateFunc        is new create_Func.item with null record;


   overriding
   function CreateCollisionAlgorithm (Self : in CreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                                              body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view;



   type SwappedCreateFunc is new create_Func.item with null record;

   overriding
   function CreateCollisionAlgorithm (Self : in SwappedCreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                                                     body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view;








private


   type collision_algorithm_View is access all impact.d3.collision.Algorithm.item'Class;

   package collision_algorithm_Vectors is new ada.containers.Vectors (Positive, collision_algorithm_View);
   subtype collision_algorithm_Vector  is collision_algorithm_Vectors.Vector;



   type Item is new impact.d3.collision.Algorithm.activating.item with
      record
         m_isSwapped                : Boolean;
         m_childCollisionAlgorithms : collision_algorithm_Vector;

         m_sharedManifold           : access impact.d3.Manifold.item;
         m_ownsManifold             : Boolean;

         m_compoundShapeRevision    : Integer;   -- To keep track of changes, so that childAlgorithm array can be updated.
      end record;



   procedure removeChildAlgorithms      (Self : in out Item);
   procedure preallocateChildAlgorithms (Self : in out Item;   body0, body1 : access  impact.d3.Object.item'Class);




end impact.d3.collision.Algorithm.activating.compound;



