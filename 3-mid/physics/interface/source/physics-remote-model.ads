package physics.remote.Model
--
--  A model describing physical properties, usable by DSA.
--
is
   pragma Remote_Types;


   type Item is abstract tagged
      record
         Id    : physics.model_Id := null_physics_model_Id;
         Scale : math.Vector_3        := (1.0, 1.0, 1.0);
      end record;

end physics.remote.Model;


