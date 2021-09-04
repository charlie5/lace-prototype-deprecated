with
     ada.Streams.Stream_IO;


package openGL.Images
--
--  Provides ability to create and manipulate images.
--
is

   function fetch_Image (Stream  : in Ada.Streams.Stream_IO.Stream_Access;
                         try_TGA : in Boolean) return openGL.Image;

end openGL.Images;
