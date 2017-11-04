with
     openGL.Model;


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

   procedure Site_is (Self : in out Item;   Now     : in math.Vector_3;
                                            for_End : in Integer);
   function  Site    (Self : in     Item;   for_End : in Integer) return math.Vector_3;

--     overriding
--     function  Bounds (Self : in Item) return openGL.Bounds;





private

   type State;   -- An opaque Taft type.


   type Item is new openGL.Model.item with
      record
         Color  :        openGL.Color;
--           Bounds :        openGL.Bounds;
         State  : access grid.State;
         Width,
         Height :        Positive;
      end record;


   overriding
   procedure set_Bounds (Self : in out Item);


end openGL.Model.Grid;
