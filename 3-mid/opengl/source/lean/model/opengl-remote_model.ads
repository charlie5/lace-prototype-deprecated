package openGL.remote_Model
--
--  Provides a DSA friendly base class for 3D models.
--
is
   pragma remote_Types;

   type Item is abstract tagged
      record
         Id    : model_Id     := null_model_Id;
         Shine : openGL.Shine := 200.0;
      end record;

end openGL.remote_Model;


