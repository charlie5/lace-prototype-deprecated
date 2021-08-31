with
     openGL.geometry.colored,
     openGL.Font,
     openGL.Palette;


package openGL.Model.arrow.colored
--
--  Models a colored arrow.
--
is

   type Item is new openGL.Model.arrow.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_Arrow (Color      : in openGL.Color  := Palette.White;
                       line_Width : in openGL.Real   := 1.0;
                       End_1,
                       End_2      : in math.Vector_3 := Origin_3d) return View;


   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                     Fonts    : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;

   procedure End_Site_is (Self : in out Item;   Now     : in math.Vector_3;
                                                for_End : in Integer);
   function  End_Site    (Self : in     Item;   for_End : in Integer) return math.Vector_3;

--     overriding
--     function  Bounds      (Self : in     Item) return openGL.Bounds;

   overriding
   procedure modify      (Self : in out Item);

   overriding
   function  is_Modified (Self : in     Item) return Boolean;



private

   type Item is new openGL.Model.arrow.item with
      record
         Color       :         openGL.Color;
         line_Width  :         openGL.Real;
--           Bounds      :         openGL.Bounds;

         Vertices    : aliased openGL.Geometry.colored.Vertex_array (1 .. 4);
         Geometry    : access  openGL.Geometry.colored.item'Class;

         is_Modified :         Boolean := False;
      end record;

--     procedure set_Bounds    (Self : in out Item);
   procedure set_side_Bits (Self : in out Item);


end openGL.Model.arrow.colored;
