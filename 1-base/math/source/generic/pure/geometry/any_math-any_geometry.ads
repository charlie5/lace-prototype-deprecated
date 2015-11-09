generic
package any_math.any_Geometry
--
-- Provides core types for geometry.
--
is
   pragma Pure;


   subtype vertex_Id  is Index;
   type    vertex_Ids is array (Index range <>) of vertex_Id;

   subtype Triangle   is vertex_Ids (1 .. 3);
   type    Triangles  is array (Index range <>) of Triangle;

   function Image (Self : in Triangle)  return String;
   function Image (Self : in Triangles) return String;



   --------
   -- Model
   --

   type model_Options is tagged null record;

   default_model_Options : constant model_Options;



   type model_Triangles (triangle_Count : any_math.Index) is tagged
      record
         Triangles : any_geometry.Triangles (1 .. triangle_Count);
      end record;

   function Image (Self : in model_Triangles) return String;



   type Model is abstract tagged
      record
         Triangles : access model_Triangles'Class;
      end record;

   function Image (Self : in Model) return String;



   ----------------
   -- Geometry Item
   --

   type Item is abstract tagged private;

   procedure destroy (Self : in out Item)                       is abstract;
   procedure expand  (Self : access Item;   By   : in     Real) is abstract;




private

   type Item  is abstract tagged
      record
         null;
      end record;


   default_model_Options : constant model_Options := (others => <>);

end any_math.any_Geometry;
