with
     openGL.Program.lit_colored_textured_skinned;

package openGL.Geometry.lit_colored_textured_skinned
--
--  Supports per-vertex site, color, texture, lighting and skinning.
--
is

   type Item is new openGL.Geometry.item with private;

   function new_Geometry return access Geometry.lit_colored_textured_skinned.item'class;


   ----------
   --  Vertex
   --

   type Vertex is
      record
         Site   : Vector_3;
         Normal : Vector_3;
         Color  : lucid_Color;
         Coords : Coordinate_2D;

         bone_Ids     : Vector_4;
         bone_Weights : Vector_4;
      end record;
   pragma Convention (C, Vertex);

   type Vertex_array is array (long_Index_t range <>) of aliased Vertex;


   --------------
   --  Attributes
   --

   procedure Vertices_are (Self : in out Item;   Now : in Vertex_array);

   overriding
   procedure Indices_are  (Self : in out Item;   Now       : in Indices;
                                                 for_Facia : in Positive);

   function  Program return openGL.Program.lit_colored_textured_skinned.view;



private

   type Item is new Geometry.item with
      record
         null;
      end record;

   overriding
   procedure enable_Texture (Self : in Item);

end openGL.Geometry.lit_colored_textured_skinned;
