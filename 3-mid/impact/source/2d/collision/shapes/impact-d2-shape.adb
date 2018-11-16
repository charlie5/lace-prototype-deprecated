
package body impact.d2.Shape
is




   function getKind (Self : in b2Shape'Class) return shape.Kind
   is
   begin
      return self.m_Kind;
   end getKind;




end impact.d2.Shape;
