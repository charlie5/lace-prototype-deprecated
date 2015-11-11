generic
package any_math.any_geometry.any_d3.any_Modeller.any_Forge
--
--  Provides constructors for several geometry primitives.
--
is

   function     to_Box_Model (half_Extents : in Vector_3 := (0.5, 0.5, 0.5)) return a_Model;

   function to_capsule_Model (Length       : in Real     := 1.0;
                              Radius       : in Real     := 0.5)             return a_Model;

end any_math.any_geometry.any_d3.any_Modeller.any_Forge;

