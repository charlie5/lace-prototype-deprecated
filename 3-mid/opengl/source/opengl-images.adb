with
     GID,
     ada.Calendar;

package body openGL.Images
is

   function fetch_Image (Stream  : in Ada.Streams.Stream_IO.Stream_Access;
                         try_TGA : in Boolean) return openGL.Image
   is
      the_GID_Image : GID.Image_descriptor;
      next_Frame    : Ada.Calendar.Day_Duration := 0.0;

   begin
      GID.Load_image_header (the_GID_Image,
                             Stream.all,
                             try_TGA);
      declare
         Image_Width  : constant Positive := GID.Pixel_Width  (the_GID_Image);
         Image_Height : constant Positive := GID.Pixel_height (the_GID_Image);

         the_Image    :          openGL.Image (1 .. Index_t (Image_Height),
                                               1 .. Index_t (Image_Width));
         procedure Load_raw_image
         is
            subtype Primary_color_range is GL.glUByte;

            Row,
            Col : Index_t;

            procedure set_X_Y (x, y : Natural)
            is
            begin
               Col := Index_t (X + 1);
               Row := Index_t (Y + 1);
            end set_X_Y;

            procedure put_Pixel (Red, Green, Blue : primary_Color_Range;
                                 Alpha            : primary_Color_Range)
            is
               use type GL.glUByte, Real;
               pragma Warnings (Off, Alpha);     -- Alpha is just ignored.
            begin
               the_Image (Row, Col) := (Red, Green, Blue);

               if Col = Index_t (Image_Width)
               then                 -- GID requires us to look to next pixel on the right for next time.
                  Row := Row + 1;
                  Col := 1;
               else
                  Col := Col + 1;
               end if;
            end put_Pixel;

            procedure Feedback (Percents : Natural) is null;

            procedure Load_image is
              new GID.load_Image_contents (primary_Color_Range,
                                           set_X_Y,  put_Pixel,
                                           Feedback, GID.fast);
         begin
            load_Image (the_GID_Image, next_Frame);
         end load_raw_Image;

      begin
         load_raw_Image;
         return the_Image;
      end;
   end fetch_Image;


end openGL.Images;
