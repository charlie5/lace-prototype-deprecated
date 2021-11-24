private
with
     openGL.Geometry.colored;


package openGL.Model.line.colored
--
--  Models a colored line.
--
is
   type Item is new Model.line.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_line_Model (Color : in openGL.Color;
                            End_1,
                            End_2 : in Vector_3 := Origin_3D) return View;


   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;

   subtype end_Id is Index_t range 1 .. 2;

   procedure Site_is (Self : in out Item;   Now     : in Vector_3;
                                            for_End : in end_Id);
   function  Site    (Self : in     Item;   for_End : in end_Id) return Vector_3;



private

   type Item is new Model.line.item with
      record
         Color    :        openGL.rgb_Color;
         Vertices :        Geometry.colored.Vertex_array (end_Id);
         Geometry : access Geometry.colored.item'Class;
      end record;

end openGL.Model.line.colored;
