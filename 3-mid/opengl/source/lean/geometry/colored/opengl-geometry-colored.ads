package openGL.Geometry.colored
--
--  Supports per-vertex site and color.
--
is
   type Item is new openGL.Geometry.item with private;

   function new_Geometry return access Geometry.colored.item'class;


   ------------
   --  Vertices
   --

   type Vertex is
      record
         Site  : Vector_3;
         Color : lucid_Color;
      end record;

   type Vertex_array is array (Index_t range <>) of aliased Vertex;

   function is_Transparent (Self : in Vertex_array) return Boolean;


   --------------
   --  Attributes
   --

   overriding
   function  is_Transparent (Self : in     Item) return Boolean;

   procedure Vertices_are   (Self : in out Item;   Now : in Vertex_array);



private

   type Item is new Geometry.item with
      record
         null;
      end record;

end openGL.Geometry.colored;
