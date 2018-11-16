
package body impact.d3.Material
is


   function to_Material (friction,
                         restitution : math.Real) return Item
   is
      Self : Item;
   begin
      Self.m_friction    := friction;
      Self.m_restitution := restitution;

      return Self;
   end to_Material;


end impact.d3.Material;


