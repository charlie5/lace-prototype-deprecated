package openGL.remote_Model
--
--  Provides a DSA friendly bsse for a model describing a 3D model.
--
is
   pragma Remote_Types;

   type Item is abstract tagged
      record
         Id    : Model_Id := null_model_Id;
         Scale : Vector_3 := (1.0, 1.0, 1.0);
      end record;

end openGL.remote_Model;


