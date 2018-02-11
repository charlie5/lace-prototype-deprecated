with
     openGL.Model;

private
with
     openGL.Geometry.colored;


package openGL.Model.line.colored
--
--  Models a colored line.
--
is

   type Item is new openGL.Model.line.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_line_Model (Color : in openGL.Color;
                            End_1,
                            End_2 : in math.Vector_3 := Origin_3d) return Model.line.colored.view;


   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;

   subtype end_Id is Index_t range 1 .. 2;

   procedure Site_is (Self : in out Item;   Now     : in math.Vector_3;
                                            for_End : in end_Id);
   function  Site    (Self : in     Item;   for_End : in end_Id) return math.Vector_3;



private

   type Item is new openGL.Model.line.item with
      record
         Color    :        openGL.Color;

         Vertices :        openGL.geometry.colored.Vertex_array (end_Id);
         Geometry : access openGL.Geometry.colored.item'Class;
      end record;

end openGL.Model.line.colored;
