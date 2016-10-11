with
     openGL.Texture,

     ada.Strings.unbounded,
     ada.Streams.Stream_IO,
     ada.Unchecked_Deallocation;


package openGL.IO
--
--  Provides I/O functions for openGL.
--
is

   subtype Text is ada.Strings.unbounded.unbounded_String;


   ------------------
   --  General Vertex
   --

   null_Id : constant long_Index_t;

   type Vertex is
      record
         site_Id,
         coord_Id,
         normal_Id,
         weights_Id : long_Index_t;
   end record;

   type Vertices      is array (long_Index_t range <>) of aliased Vertex;
   type Vertices_view is access all Vertices;



   --------
   --  Face
   --

   type facet_Kind is (Triangle, Quad, Polygon);

   type Face (Kind : facet_Kind := Triangle) is
      record
         case Kind
         is
         when Triangle => Tri  : Vertices (1 .. 3);
         when Quad     => Quad : Vertices (1 .. 4);
         when Polygon  => Poly : Vertices_view;
         end case;
      end record;

   type Faces is array (long_Index_t range <>) of Face;


   procedure destroy       (Self : in out Face);
   function  Vertices_of   (Self : in     Face) return Vertices;

   procedure set_Vertex_in (Self : in out Face;   Which : in long_Index_t;
                                                  To    : in Vertex);


   --------------------
   --  Rigging/Skinning
   --

   type bone_Id is range 0 .. 200;

   type bone_weight is
      record
         Bone   : bone_Id;
         Weight : Real;
      end record;

   type bone_Weights       is array (long_Index_t range <>) of bone_weight;
   type bone_Weights_view  is access bone_Weights;
   type bone_Weights_array is array (long_Index_t range <>) of bone_Weights_view;



   ---------
   --  Views
   --

   type              Sites_view is access all openGL.many_Sites;
   type             Coords_view is access all openGL.many_Coordinates_2D;
   type            Normals_view is access all openGL.many_Normals;
   type bone_weights_Array_view is access all bone_weights_Array;
   type              Faces_view is access all IO.Faces;

   procedure free is new Ada.Unchecked_Deallocation (many_Sites,          IO.Sites_view);
   procedure free is new Ada.Unchecked_Deallocation (many_Coordinates_2D, IO.Coords_view);
   procedure free is new Ada.Unchecked_Deallocation (many_Normals,        IO.Normals_view);
   procedure free is new Ada.Unchecked_Deallocation (IO.Faces,            IO.Faces_view);



   -----------------
   --- General Model
   --

   type Model is
      record
         Sites   : Sites_view;
         Coords  : Coords_view;
         Normals : Normals_view;
         Weights : bone_weights_Array_view;
         Faces   : Faces_view;
      end record;

   procedure destroy (Self : in out Model);



   --------------
   --  Heightmaps
   --

   type height_Map_view is access all openGL.height_Map;

   function to_height_Map (image_Filename : in String;
                           Scale          : in Real  := 1.0) return height_Map_view;


   ----------
   --  Images
   --

   function fetch_Image (Stream  : in Ada.Streams.Stream_IO.Stream_Access;
                         try_TGA : in Boolean) return openGL.Image;
   pragma Obsolescent   (fetch_Image, "use 'openGL.Images.fetch_Image' instead");


   function       to_Image (image_Filename : in     String)  return openGL.Image;
   function to_lucid_Image (image_Filename : in     String)  return openGL.lucid_Image;
   function to_lucid_Image (image_Filename : in     String;
                            is_Lucid       : access Boolean) return openGL.lucid_Image;


   ------------
   --  Textures
   --

   function to_Texture (image_Filename : in     String)  return openGL.Texture.Object;



   ---------------
   --  Screenshots
   --

   function  current_Frame return  openGL.Image;

   procedure Screenshot (Filename : in String;   with_Alpha : in Boolean := False);
   --
   --  Stores the image of the current, active viewport (in RGB or RGBA Bitmap format).



   -----------------
   --  Video Capture
   --

   procedure start_capture (AVI_name   : in String;
                            frame_rate : in Positive);
   --
   --  Prepare for video capture (RGB uncompressed, AVI format).

   procedure capture_Frame;
   --
   --  Captures the current, active viewport.

   procedure stop_Capture;




private
   null_Id : constant long_Index_t := 0;
end openGL.IO;
