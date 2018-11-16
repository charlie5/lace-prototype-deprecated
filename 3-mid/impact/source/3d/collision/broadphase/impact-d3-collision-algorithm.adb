
package body impact.d3.collision.Algorithm
is



   function to_AlgorithmConstructionInfo (dispatcher : access impact.d3.Dispatcher.item'Class;   temp : in Integer) return AlgorithmConstructionInfo
   is
      pragma Unreferenced (temp);
      Self : AlgorithmConstructionInfo;
   begin
      Self.m_dispatcher1 := dispatcher;

      return Self;
   end to_AlgorithmConstructionInfo;






   procedure define (Self : in out Item;   ci : in AlgorithmConstructionInfo)
   is
   begin
      Self.m_dispatcher := ci.m_dispatcher1;
   end define;




   procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;




   procedure set_m_dispatcher (Self : in out Item;   To : access impact.d3.Dispatcher.item'Class)
   is
   begin
      Self.m_dispatcher := To;
   end set_m_dispatcher;



   function  get_m_dispatcher (Self : in     Item) return access impact.d3.Dispatcher.item'Class
   is
   begin
      return Self.m_dispatcher;
   end get_m_dispatcher;






end impact.d3.collision.Algorithm;
