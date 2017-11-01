with
     openGL.Model;


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

   function  to_line_Model (Color : in openGL.Color;
                            End_1,
                            End_2 : in math.Vector_3 := Origin_3d) return Model.line.colored.item;

   function new_line_Model (Color : in openGL.Color;
                            End_1,
                            End_2 : in math.Vector_3 := Origin_3d) return Model.line.colored.view;


   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;

   procedure Site_is (Self : in out Item;   Now     : in math.Vector_3;
                                            for_End : in Integer);
   function  Site    (Self : in     Item;   for_End : in Integer) return math.Vector_3;


--     overriding
--     function  Bounds (Self : in Item) return openGL.Bounds;




private

   type State;     -- An opaque Taft type.


   type Item is new openGL.Model.line.item with
      record
         Color  :        openGL.Color;
--           Bounds :        openGL.Bounds;
         State  : access line.colored.State;
      end record;


   procedure set_Bounds (Self : in out Item);


end openGL.Model.line.colored;
