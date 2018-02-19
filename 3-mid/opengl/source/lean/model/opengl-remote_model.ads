package openGL.remote_Model
--
--  Provides a DSA friendly bsse class for 3D models.
--
is
   pragma Remote_Types;

   type Item is abstract tagged
      record
         Id    : Model_Id     := null_model_Id;
         Scale : Vector_3     := (1.0, 1.0, 1.0);
         Shine : openGL.Shine := 200.0;
      end record;

end openGL.remote_Model;


