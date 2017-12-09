with
     openGL.Geometry.colored;


package openGL.Model.Grid
--
--  Models a grid.
--
is

   type Item is new openGL.Model.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function  to_grid_Model (Color  : openGL.Color;
                            Width  : Integer;
                            Height : Integer) return Model.Grid.item;

   function new_grid_Model (Color  : openGL.Color;
                            Width  : Integer;
                            Height : Integer) return Model.Grid.view;

   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;


private

   type Item is new openGL.Model.item with
      record
         Color    :        openGL.Color;
         Vertices : access openGL.geometry.colored.Vertex_array;
         Geometry : access openGL.Geometry.colored.item'Class;
         Width,
         Height   :        Positive;
      end record;

end openGL.Model.Grid;
