generic
package any_math.any_geometry.any_d3.any_Modeller.any_Forge
--
--  Provides constructors for several geometry primitives.
--
is

   function     to_Box_Model (half_Extents : in Vector_3 := (0.5, 0.5, 0.5)) return a_Model;

   function to_capsule_Model (Length       : in Real     := 1.0;
                              Radius       : in Real     := 0.5)             return a_Model;




   type Latitude  is range -90 ..  90;
   type Longitude is range   0 .. 359;

   no_Id : constant := Positive'Last;

   type vertex is
      record
         Id   : Positive                := no_Id;
         Site : any_Geometry.any_d3.Site;
      end record;

   type longitude_line is array (latitude) of vertex;

   type polar_model is array (longitude) of longitude_line;



   type Vertices is array (Positive range <>) of Vertex;

   type Triangle is array (Positive range 1 .. 3) of Positive;

   type Triangles is array (Positive range <>) of Triangle;



--     type mesh_Model (num_Vertices  : Positive;
--                      num_Triangles : Positive) is
--        record
--           Vertices  : mesh.Vertices  (1 .. num_Vertices);
--           Triangles : mesh.Triangles (1 .. num_Triangles);
--        end record;



   function polar_Model_from (Model_Filename : in String) return polar_model;


   function mesh_Model_from (the_Model : in polar_Model) return a_Model; -- mesh_Model;




end any_math.any_geometry.any_d3.any_Modeller.any_Forge;

