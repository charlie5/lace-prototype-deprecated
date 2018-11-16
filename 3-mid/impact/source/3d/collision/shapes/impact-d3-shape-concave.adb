


package body impact.d3.Shape.concave
is



   --- Forge
   --
   procedure define   (Self : in out Item)
   is
   begin
      Self.m_collisionMargin := 0.0;
   end define;



   overriding procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;






   --- Attributes
   --

   overriding procedure setMargin (Self : in out Item;   margin : in Real)
   is
   begin
      Self.m_collisionMargin := margin;
   end setMargin;




   overriding function  getMargin (Self : in     Item) return Real
   is
   begin
      return Self.m_collisionMargin;
   end getMargin;




end impact.d3.Shape.concave;
