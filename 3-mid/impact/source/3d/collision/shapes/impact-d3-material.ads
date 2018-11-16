
package impact.d3.Material
--
--  Material class to be used by btMultimaterialTriangleMeshShape to store triangle properties.
--
--  Has public members so that materials can change due to world events.
--
is

   type Item is
      record
         m_friction,
         m_restitution : math.Real;
      end record;


   function to_Material (friction,
                         restitution : math.Real) return Item;


end impact.d3.Material;


