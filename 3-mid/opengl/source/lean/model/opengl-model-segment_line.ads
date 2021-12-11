with
     openGL.Font;

private
with
     openGL.Geometry.colored,
     ada.Containers.Vectors;


package openGL.Model.segment_line
--
--  Models a segmented line.
--
is
   type Item is new Model.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   function new_segment_line_Model (Color : in openGL.Color) return View;


   -----------
   --- Segment
   --

   type Segment is
      record
         First : Vector_3;
         Last  : Vector_3;
      end record;

   type Segments_t is array (Positive range <>) of aliased Segment;

   function Angle_in_xz_plane (the_Segment : in Segment) return Radians;


   --------------
   --- Attributes
   --

   overriding
   function  to_GL_Geometries (Self : access Item;   Textures   : access Texture.name_Map_of_texture'Class;
                                                     Fonts      : in     Font.font_id_Map_of_font) return Geometry.views;

   procedure add_1st_Segment  (Self : in out Item;   start_Site : in Vector_3;
                                                     end_Site   : in Vector_3);

   procedure add_Segment      (Self : in out Item;   end_Site   : in Vector_3);

   function Site              (Self : in     Item;   for_End    : in Integer) return Vector_3;

   procedure Site_is          (Self : in out Item;   Now        : in Vector_3;
                                                     for_End    : in Integer);

   procedure Color_is         (Self : in out Item;   Now        : in Color;
                                                     for_End    : in Integer);

   function segment_Count     (Self : in     Item) return Natural;
   function Segments          (Self : in     Item) return Segments_t;



private

   type vertex_Array_view is access all Geometry.colored.Vertex_array;

   package site_Vectors is new ada.Containers.Vectors (Positive, Vector_3);
   subtype site_Vector  is site_Vectors.Vector;


   type Item is new Model.item with
      record
         Color        : openGL.rgb_Color;
         Points       : site_Vector;

         Vertices     : Vertex_array_view := new Geometry.colored.Vertex_array (1 .. 2);
         vertex_Count : Natural           := 0;

         Geometry     : openGL.Geometry.colored.view;
      end record;

end openGL.Model.segment_line;
