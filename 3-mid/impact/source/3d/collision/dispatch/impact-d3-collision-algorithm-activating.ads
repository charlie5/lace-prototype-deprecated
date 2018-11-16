with impact.d3.collision.Algorithm,
     impact.d3.Object,
     impact.d3.Dispatcher,
     impact.d3.collision.manifold_Result;





package impact.d3.collision.Algorithm.activating
--
--  This class is not enabled yet (work-in-progress) to more aggressively activate objects.
--
is

   type Item is abstract new impact.d3.collision.Algorithm.item with private;


   procedure define   (Self : in out Item;   ci               : in     AlgorithmConstructionInfo;
                                             colObj0, colObj1 : access impact.d3.Object.item'Class);

--     package Forge
--     is
--        function  to_activating_Algorithm (ci               : in     AlgorithmConstructionInfo) return Item;
--        function  to_activating_Algorithm (ci               : in     AlgorithmConstructionInfo;
--                                           colObj0, colObj1 : access impact.d3.Object.item'Class                ) return Item;
--     end Forge;


   overriding procedure destruct (Self : in out Item);



   overriding function  calculateTimeOfImpact (Self : in Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                      dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                      resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real;





private

   type Item is abstract new impact.d3.collision.Algorithm.item with
      record
         null;
      end record;


end impact.d3.collision.Algorithm.activating;
