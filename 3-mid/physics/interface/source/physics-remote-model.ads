package physics.remote.Model
--
--  A model describing physical properties, usable with DSA.
--
is
   pragma remote_Types;

   type Item is abstract tagged
      record
         Id    : model_Id := null_model_Id;
         Scale : Vector_3 := (1.0, 1.0, 1.0);
      end record;

end physics.remote.Model;


