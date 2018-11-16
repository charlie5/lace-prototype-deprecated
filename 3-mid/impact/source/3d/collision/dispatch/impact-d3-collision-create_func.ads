with impact.d3.collision.Algorithm,
     impact.d3.Object;
with impact.d3.Dispatcher;
with impact.d3.Collision.Algorithm;


package impact.d3.Collision.create_Func
is



   type Item is abstract tagged    -- Used by the impact.d3.Dispatcher.collision to register and create instances for impact.d3.collision.Algorithm
      record
         m_swapped : Boolean := False;
      end record;



   procedure destruct (Self : in out Item) is null;




   function CreateCollisionAlgorithm (Self : in Item;   info  : in     impact.d3.Collision.Algorithm.AlgorithmConstructionInfo;
                                                        body0,
                                                        body1 : access Object.item'Class) return impact.d3.Dispatcher.Algorithm_view;
--                                        is abstract;


--     procedure dummy;
end impact.d3.Collision.create_Func;


--  class impact.d3.collision.Algorithm;
--  class impact.d3.Object;



--  struct impact.d3.collision.AlgorithmConstructionInfo;





--  struct impact.d3.collision.AlgorithmCreateFunc
--  {
--          virtual        impact.d3.collision.Algorithm* CreateCollisionAlgorithm(impact.d3.collision.AlgorithmConstructionInfo& , impact.d3.Object* body0,impact.d3.Object* body1)
--          {
--
--                  (void)body0;
--                  (void)body1;
--                  return 0;
--          }
--  };

