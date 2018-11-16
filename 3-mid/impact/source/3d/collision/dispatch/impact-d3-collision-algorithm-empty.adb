

package body impact.d3.collision.Algorithm.empty
is


   function  to_empty_Algorithm (ci   : in     AlgorithmConstructionInfo) return Item'Class
   is
      Self : Item;
   begin
      define (self, ci);

      return Self;
   end to_empty_Algorithm;


   overriding procedure destruct                     (Self : in out Item)
   is
      pragma Unreferenced (Self);
   begin
      return;
   end destruct;




   overriding procedure processCollision (Self : in out Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                     resultOut    :    out impact.d3.collision.manifold_Result.item)
   is
      pragma Unreferenced (resultOut, Self, body0, body1, dispatchInfo);
   begin
      return;
   end processCollision;



   overriding function calculateTimeOfImpact (Self : in     Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                                 dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                   resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real
   is
      pragma Unreferenced (Self, body0, body1, dispatchInfo, resultOut);
   begin
      return 1.0;
   end calculateTimeOfImpact;




   overriding procedure getAllContactManifolds (Self : in out Item;   manifoldArray : out impact.d3.collision.Algorithm.btManifoldArray)
   is
   begin
      raise Program_Error with "TBD";
      null;
   end getAllContactManifolds;








   --- Create Functions
   --

   overriding function CreateCollisionAlgorithm (Self : in CreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                                              body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view
   is
      pragma Unreferenced (Self, body0, body1);
   begin
      return new Item'(Item (to_empty_Algorithm (ci)));
   end CreateCollisionAlgorithm;



end impact.d3.collision.Algorithm.empty;
