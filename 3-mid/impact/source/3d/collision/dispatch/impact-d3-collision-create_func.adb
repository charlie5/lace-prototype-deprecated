

package body impact.d3.Collision.create_Func
is




   function CreateCollisionAlgorithm (Self : in Item;   info  : in     impact.d3.Collision.Algorithm.AlgorithmConstructionInfo;
                                                                                  body0,
                                      body1 : access Object.item'Class) return impact.d3.Dispatcher.Algorithm_view
   is
   begin
      raise Program_Error;
      return null;
   end CreateCollisionAlgorithm;

   procedure dummy is begin null; end dummy;

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

