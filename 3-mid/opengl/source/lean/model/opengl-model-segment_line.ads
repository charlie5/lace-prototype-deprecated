with
     openGL.Model,
     openGL.Font;

private
with
     ada.Containers.Vectors,
     ada.Streams;


package openGL.Model.segment_line
--
--  Models a segmented line.
--
is

   type Item is new openGL.Model.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function  to_segment_line_Model (Scale : math.Vector_3;
                                    Color : openGL.Color) return Model.segment_line.item;

   function new_segment_line_Model (Scale : math.Vector_3;
                                    Color : openGL.Color) return Model.segment_line.view;


   -----------
   --- Segment
   --

   type Segment is
      record
         First : math.Vector_3;
         Last  : math.Vector_3;
      end record;

   type Segments_t is array (Positive range <>) of aliased Segment;


   function Angle_in_xz_plane (Self : in Segment) return math.Radians;



   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures   : access Texture.name_Map_of_texture'Class;
                                                     Fonts      : in     Font.font_id_Maps_of_font.Map) return openGL.Geometry.views;


   procedure add_1st_Segment  (Self : in out Item;   start_Site : in math.Vector_3;
                                                     end_Site   : in math.Vector_3);

   procedure add_Segment      (Self : in out Item;   end_Site   : in math.Vector_3);


   function Site              (Self : in     Item;   for_End    : in Integer) return math.Vector_3;

   procedure Site_is          (Self : in out Item;   Now        : in math.Vector_3;
                                                     for_End    : in Integer);


   procedure Color_is         (Self : in out Item;   Now        : in openGL.Color;
                                                     for_End    : in Integer);

   function segment_Count     (Self : in     Item) return Natural;
   function Segments          (Self : in     Item) return Model.segment_line.Segments_t;

--     overriding
--     function  Bounds           (Self : in     Item) return openGL.Bounds;

--     overriding
--     procedure set_Bounds (Self : in out Item);



private

   type State;   -- An opaque Taft type.


   package site_Vectors is new ada.Containers.Vectors (Positive, math.Vector_3);
   subtype site_Vector  is site_Vectors.Vector;


   type Item is new openGL.Model.item with
      record
         Color  :        openGL.Color;
         Points :        site_Vector;

--           Bounds :        openGL.Bounds;

         State  : access Model.segment_line.State;
      end record;



   procedure Item_write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                         Self   : in     segment_Line.Item);
   for Item'Write use Item_write;



   procedure Item_read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                        Self   : out    segment_Line.Item);
   for Item'Read use Item_read;


end openGL.Model.segment_line;
