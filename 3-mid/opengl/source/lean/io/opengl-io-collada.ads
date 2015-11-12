package openGL.IO.collada
--
--  Provides a function to convert a Collada model file to an openGL IO model.
--
is

   function to_Model (model_Path : in String;
                      Scale      : in Vector_3 := (1.0, 1.0, 1.0)) return IO.Model;

end openGL.IO.collada;
