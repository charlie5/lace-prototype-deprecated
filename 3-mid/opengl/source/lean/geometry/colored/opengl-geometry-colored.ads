package openGL.Geometry.colored
--
--  Supports per-vertex site and color.
--
is
   type Item is new Geometry.item with private;
   type View is access Item'Class;



   function new_Geometry return Geometry.colored.view;


   ------------
   --  Vertices
   --

   type Vertex is
      record
         Site  : Vector_3;
         Color : rgba_Color;
      end record;

   type Vertex_array is array (Index_t range <>) of aliased Vertex;


   --------------
   --  Attributes
   --

   procedure Vertices_are (Self : in out Item;   Now : in Vertex_array);



private

   type Item is new Geometry.item with
      record
         null;
      end record;

end openGL.Geometry.colored;
