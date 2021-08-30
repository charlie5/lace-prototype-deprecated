private
with
     ada.Containers.Vectors,
     ada.Containers.hashed_Maps,
     ada.Containers.ordered_Sets;


generic
package any_Math.any_Geometry.any_d3.any_Modeller
is

   type Item is tagged private;
   type View is access all Item;


   --------------
   --  Attributes
   --
   procedure add_Triangle   (Self : in out Item;   Vertex_1,
                                                   Vertex_2,
                                                   Vertex_3 : in Site);

   function  Triangle_Count (Self : in     Item) return Natural;
   function  Model          (Self : in     Item) return a_Model;

   function  bounding_Sphere_Radius (Self : in out Item) return Real;
   --
   -- Caches the radius on 1st call.


   --------------
   --  Operations
   --
   procedure clear (Self : in out Item);



private

   subtype    Vertex is Site;
   type    my_Vertex is new Vertex;


   --------------
   --  Containers
   --

   function Hash (Site : in my_Vertex) return ada.Containers.Hash_type;
   package  Vertex_Maps_of_Index is new ada.Containers.hashed_Maps (my_Vertex,
                                                                    Natural,
                                                                    Hash,
                                                                    "=");
   subtype  Vertex_Map_of_Index         is Vertex_Maps_of_Index.Map;


   package  Vertex_Vectors is new Ada.Containers.Vectors (Positive, Vertex);
   subtype  Vertex_Vector  is Vertex_Vectors.Vector;


   subtype  Index_Triangle is any_Geometry.Triangle;
   function "<" (Left, Right : in Index_Triangle) return Boolean;
   package  Index_Triangle_Sets is new ada.Containers.ordered_Sets (Element_Type => Index_Triangle,
                                                                   "<"          => "<",
                                                                   "="          => "=");
   subtype  Index_Triangle_Set  is Index_Triangle_Sets.Set;


   ------------
   --  Modeller
   --

   type Item is tagged
      record
         Triangles : Index_Triangle_Set;
         Vertices  : Vertex_Vector;
         Index_Map : Vertex_Map_of_Index;

         bounding_Sphere_Radius : Real := Real'First;
      end record;

end any_Math.any_Geometry.any_d3.any_Modeller;
