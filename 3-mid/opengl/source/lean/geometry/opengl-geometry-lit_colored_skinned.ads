with
     openGL.Program.lit.colored_skinned;

package openGL.Geometry.lit_colored_skinned
--
--  Supports per-vertex site color, texture, lighting and skinning.
--
is
   type Item is new openGL.Geometry.item with private;
   type View is access all Item'Class;

   function new_Geometry return View;

   procedure define_Program;


   ----------
   --  Vertex
   --

   type Vertex is
      record
         Site   : Vector_3;
         Normal : Vector_3;
         Color  : rgba_Color;
         Shine  : Real;

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

   function  Program return openGL.Program.lit.colored_skinned.view;



private

   type Item is new Geometry.item with null record;

   overriding
   procedure enable_Texture (Self : in Item);

end openGL.Geometry.lit_colored_skinned;
