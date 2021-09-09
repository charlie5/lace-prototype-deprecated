package openGL.IO.Lat_Long_Radius
--
--  Provides a function to convert a model file containing longitude, latitude
--  and radius triplets (one triplet per line) to an openGL IO model.
--
is

   function to_Model (model_Path : in     String;
                      Scale      : in     Vector_3 := (1.0, 1.0, 1.0)) return IO.Model;

   function to_Model (math_Model : access Geometry_3d.a_Model;
                      Scale      : in     Vector_3 := (1.0, 1.0, 1.0)) return IO.Model;

end openGL.IO.Lat_Long_Radius;
