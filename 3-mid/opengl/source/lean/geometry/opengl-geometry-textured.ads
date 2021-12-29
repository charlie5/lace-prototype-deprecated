package openGL.Geometry.textured
--
--  Supports per-vertex site and texture.
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
         Coords : Coordinate_2D;
      end record;

   type Vertex_array is array (Index_t range <>) of aliased Vertex;


   --------------
   --  Attributes
   --

   overriding
   function  is_Transparent (Self : in     Item) return Boolean;

   procedure Vertices_are   (Self : in out Item;   Now       : in Vertex_array);

   overriding
   procedure Indices_are    (Self : in out Item;   Now       : in Indices;
                                                   for_Facia : in Positive);


private

   type Item is new Geometry.item with null record;

   overriding
   procedure enable_Texture (Self : in Item);

end openGL.Geometry.textured;
