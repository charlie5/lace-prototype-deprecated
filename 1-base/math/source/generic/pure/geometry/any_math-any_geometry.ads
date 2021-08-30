generic
package any_Math.any_Geometry
--
-- Provides a namespace and core types for geometry.
--
is
   pragma Pure;


   subtype Vertex_Id  is Index;
   type    Vertex_Ids is array (Index range <>) of Vertex_Id;

   subtype Triangle   is Vertex_Ids (1 .. 3);
   type    Triangles  is array (Index range <>) of Triangle;

   function Image (Self : in Triangle)  return String;
   function Image (Self : in Triangles) return String;


   --------
   -- Model
   --

   type Model_Options is tagged null record;

   default_Model_Options : constant Model_Options;


   type Model_Triangles (Triangle_Count : Index) is tagged
      record
         Triangles : any_Geometry.Triangles (1 .. Triangle_Count);
      end record;

   function Image (Self : in Model_Triangles) return String;


   type Model is abstract tagged
      record
         Triangles : access Model_Triangles'Class;
      end record;

   function Image (Self : in Model) return String;



   ----------------
   -- Geometry Item
   --

   type Item is abstract tagged private;

   procedure destroy (Self : in out Item)                 is abstract;
   procedure expand  (Self : access Item;   By : in Real) is abstract;



private

   type Item  is abstract tagged
      record
         null;
      end record;

   default_Model_Options : constant Model_Options := (others => <>);

end any_Math.any_Geometry;
