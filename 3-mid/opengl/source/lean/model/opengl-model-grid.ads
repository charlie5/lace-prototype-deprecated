with
     openGL.Geometry.colored;


package openGL.Model.grid
--
--  Models a grid.
--
--  TODO: Rename to 'line_Grid'.
is
   type Item is new Model.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_grid_Model (Color  : openGL.Color;
                            Width  : Integer;
                            Height : Integer) return View;

   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views;


private

   type Item is new Model.item with
      record
         Color    :        openGL.rgb_Color;
         Vertices : access openGL.Geometry.colored.Vertex_array;
         Geometry :        openGL.Geometry.colored.view;
         Width,
         Height   :        Positive;
      end record;

end openGL.Model.grid;
