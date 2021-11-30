package openGL.Geometry.lit
--
--  Supports per-vertex site color, texture and lighting.
--
is
   type Item is new openGL.Geometry.item with private;

   function new_Geometry (texture_is_Alpha : in Boolean) return access Geometry.lit.item'Class;


   ----------
   --  Vertex
   --

   type Vertex is
      record
         Site   : Vector_3;
         Normal : Vector_3;
         Color  : rgba_Color;
         Coords : Coordinate_2D;
         Shine  : Real;
      end record;

   type Vertex_array is array (Index_t range <>) of aliased Vertex;


   --------------
   --  Attributes
   --

   procedure Vertices_are   (Self : in out Item;   Now       : in Vertex_array);

   overriding
   procedure Indices_are    (Self : in out Item;   Now       : in Indices;
                                                   for_Facia : in Positive);



private

   type Item is new Geometry.item with null record;

   overriding
   procedure enable_Texture (Self : in Item);

end openGL.Geometry.lit;