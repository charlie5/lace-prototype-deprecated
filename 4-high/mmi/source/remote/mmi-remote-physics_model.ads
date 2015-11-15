package mmi.remote.physics_Model
--
--  Interface for a model describing physical properties.
--
is
   pragma Remote_Types;


   type Item is abstract tagged
      record
         Id    : mmi.physics_model_Id := null_physics_model_Id;
         Scale : math.Vector_3        := (1.0, 1.0, 1.0);
      end record;

end mmi.remote.physics_Model;


