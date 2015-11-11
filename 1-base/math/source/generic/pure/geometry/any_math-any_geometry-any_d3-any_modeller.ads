private
with
     ada.Containers.Vectors,
     ada.Containers.hashed_Maps,
     ada.Containers.ordered_Sets;


generic
package any_math.any_Geometry.any_d3.any_Modeller
is

   type Item is tagged private;
   type View is access all Item;


   --------------
   --  Attributes
   --

   procedure add_Triangle   (Self : in out Item;   Vertex_1,
                                                   Vertex_2,
                                                   Vertex_3      : in Site);

   function  triangle_Count (Self : in     Item) return Natural;
   function  Model          (Self : in     Item) return a_Model;

   function bounding_sphere_Radius (Self : access item) return Real;


   --------------
   --  Operations
   --

   procedure clear          (Self : in out Item);




private

   subtype    Vertex is Site;
   type    my_Vertex is new Vertex;


   --------------
   --  Containers
   --

   function Hash (the_Site : in my_Vertex) return ada.Containers.Hash_Type;
   package vertex_index_Maps is new ada.containers.hashed_Maps (my_Vertex,
                                                                Natural,
                                                                Hash,
                                                                "=");
   subtype vertex_index_Map        is vertex_index_Maps.Map;
   subtype vertex_index_map_Cursor is vertex_index_Maps.Cursor;


   package vertex_Vectors is new Ada.Containers.Vectors (Positive, Vertex);


   subtype index_Triangle is any_geometry.Triangle;


   function "<" (L, R : in index_Triangle) return Boolean;

   package index_triangle_Sets is new ada.Containers.Ordered_Sets (Element_Type => index_Triangle,
                                                                   "<"          => "<",
                                                                   "="          => "=");

   ------------
   --  Modeller
   --

   type Item is tagged
      record
         Triangles              : index_triangle_Sets.Set;

         Vertices               : vertex_Vectors.Vector;
         vertex_index_Map       : any_modeller.vertex_index_Map;

         bounding_sphere_Radius : Real := Real'First;
      end record;

end any_math.any_Geometry.any_d3.any_Modeller;
