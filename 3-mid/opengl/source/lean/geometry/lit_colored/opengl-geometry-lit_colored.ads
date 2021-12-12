package openGL.Geometry.lit_colored
--
--  Supports per-vertex color and lighting.
--
is
   type Item is new openGL.Geometry.item with private;
   type View is access all Item'Class;


   function new_Geometry return View;


   ----------
   --  Vertex
   --

   type Vertex is
      record
         Site   : Vector_3;
         Normal : Vector_3;
         Color  : rgba_Color;
         Shine  : Real;
      end record;

   type Vertex_array is array (Index_t range <>) of aliased Vertex;


   --------------
   --  Attributes
   --

   procedure Vertices_are   (Self : in out Item;   Now : in Vertex_array);



private

   type Item is new Geometry.item with null record;

end openGL.Geometry.lit_colored;
