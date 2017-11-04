package openGL.Geometry.lit_colored_textured
--
--  Supports per-vertex site, color, texture and lighting.
--
is

   type Item is new openGL.Geometry.item with private;

   function new_Geometry (texture_is_Alpha : in Boolean) return access Geometry.lit_colored_textured.item'class;


   ----------
   --  Vertex
   --

   type Vertex is
      record
         Site   : Vector_3;
         Normal : Vector_3;
         Color  : lucid_Color;
         Coords : Coordinate_2D;
      end record;

   type Vertex_array is array (Index_t range <>) of aliased Vertex;


   --------------
   --  Attributes
   --

   overriding
   function  is_Transparent (Self : in     Item) return Boolean;

   overriding
   procedure Indices_are    (Self : in out Item;   Now       : in Indices;
                                                   for_Facia : in Positive);

   procedure Vertices_are   (Self : in out Item;   Now       : in Vertex_array);



private

   type Item is new Geometry.item with
      record
         null;
      end record;

   overriding
   procedure enable_Texture (Self : in Item);

end openGL.Geometry.lit_colored_textured;
