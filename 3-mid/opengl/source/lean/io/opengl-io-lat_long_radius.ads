package openGL.IO.lat_long_Radius
--
--  Provides a function to convert a model file containing longitude, latitude
--  and radius triplets (one triplet per line) to an openGL IO model.
--
is

   function to_Model (model_File : in     String)              return IO.Model;
   function to_Model (math_Model : access Geometry_3d.a_Model) return IO.Model;

end openGL.IO.lat_long_Radius;
