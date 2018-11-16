
package body impact.d3.collision.Algorithm.activating
is




   procedure define   (Self : in out Item;   ci               : in     AlgorithmConstructionInfo;
                                             colObj0, colObj1 : access impact.d3.Object.item'Class)
   is
      pragma Unreferenced (colObj0, colObj1);
      use impact.d3.collision.Algorithm;
   begin
      define (Self, ci);
   end define;





   overriding procedure destruct (Self : in out Item)
   is
   begin
      null;
      --  m_colObj0->activate();
      --  m_colObj1->activate();
   end destruct;






   overriding function  calculateTimeOfImpact (Self : in Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                      dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                    resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real
   is
   begin
      raise Program_Error;   -- tbd:
      return 0.0;
   end calculateTimeOfImpact;





end impact.d3.collision.Algorithm.activating;
