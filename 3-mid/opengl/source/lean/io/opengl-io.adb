with
     openGL.Images,
     openGL.Viewport,
     openGL.Tasks,
     openGL.Errors,

     GID,

     GL.Binding,
     GL.safe,
     GL.Pointers,

     ada.unchecked_Conversion,
     ada.Calendar,
     ada.Characters.handling,

     System;

package body openGL.IO
is
   use ada.Characters.handling,
       ada.Streams.Stream_IO;

   use type Index_t;


   --------
   --  Face
   --

   function Vertices_of (Self : in Face) return Vertices
   is
   begin
      case Self.Kind
      is
         when Triangle =>   return Self.Tri;
         when Quad     =>   return Self.Quad;
         when Polygon  =>   return Self.Poly.all;
      end case;
   end Vertices_of;



   procedure set_Vertex_in (Self : in out Face;   Which : in long_Index_t;
                                                  To    : in Vertex)
   is
   begin
      case Self.Kind
      is
         when Triangle =>   Self.Tri  (Which) := To;
         when Quad     =>   Self.Quad (Which) := To;
         when Polygon  =>   Self.Poly (Which) := To;
      end case;
   end set_Vertex_in;



   procedure destroy (Self : in out Face)
   is
      procedure free is new ada.unchecked_Deallocation (Vertices, Vertices_view);
   begin
      if Self.Kind = Polygon
      then
         free (Self.Poly);
      end if;
   end destroy;


   -------------
   -- Operations
   --

   function current_Frame return Image
   is
      use GL,
          GL.Binding,
          GL.Pointers,
          Texture;

      Extent  : constant Extent_2d := openGL.Viewport.Extent;
      Frame   :          Image (1 .. Index_t (Extent.Width),
                                1 .. Index_t (Extent.Height));
   begin
      glReadPixels (0, 0,
                    GLsizei (Extent.Width),
                    GLsizei (Extent.Height),
                    to_GL (Format' (Texture.RGB)),
                    GL_UNSIGNED_BYTE,
                    to_GLvoid_access (Frame (1, 1).Red'Access));
      return Frame;
   end current_Frame;


   ---------
   --  Forge
   --

   function to_height_Map (image_Filename : in asset_Name;
                           Scale          : in Real  := 1.0) return height_Map_view
   is
      File    :          Ada.Streams.Stream_IO.File_Type;
      Image   :          GID.Image_Descriptor;
      up_Name : constant String              := To_Upper (to_String (image_Filename));

      next_Frame : ada.Calendar.Day_Duration := 0.0;

   begin
      open (File, in_File, to_String (image_Filename));

      GID.load_Image_Header (Image,
                             Stream (File).all,
                             try_tga =>          image_Filename'Length >= 4
                                        and then up_Name (up_Name'Last - 3 .. up_Name'Last) = ".TGA");
      declare
         image_Width  : constant Positive := GID.Pixel_Width  (Image);
         image_Height : constant Positive := GID.Pixel_Height (Image);

         the_Heights  : constant access height_Map := new height_Map' (1 .. Index_t (image_height) =>
                                                                         (1 .. Index_t (image_width) => <>));
         procedure load_raw_Image
         is
            subtype primary_Color_range is GL.GLubyte;

            Row, Col : Index_t;


            procedure set_X_Y (x, y : Natural)
            is
            begin
               Col := Index_t (X + 1);
               Row := Index_t (Y + 1);
            end Set_X_Y;


            procedure put_Pixel (Red, Green, Blue : primary_Color_range;
                                 Alpha            : primary_Color_range)
            is
               pragma Warnings (Off, alpha); -- Alpha is just ignored.
               use type GL.GLubyte, Real;
            begin
               the_Heights (Row, Col) :=   (Real (Red) + Real (Green) + Real (Blue))
                                         / (3.0 * 255.0)
                                         * Scale;

               if Col = Index_t (image_Width)
               then
                  Row := Row + 1;
                  Col := 1;
               else
                  Col := Col + 1;
               end if;

               --  ^ GID requires us to look to next pixel on the right for next time.
            end put_Pixel;


            procedure Feedback (Percents : Natural) is null;

            procedure load_Image is new GID.load_Image_contents (primary_Color_range,
                                                                 set_X_Y,
                                                                 put_Pixel,
                                                                 Feedback,
                                                                 GID.fast);
         begin
            load_Image (Image, next_Frame);
         end load_Raw_image;

      begin
         load_raw_Image;
         close (File);

         return the_Heights.all'unchecked_Access;
      end;
   end to_height_Map;



   function fetch_Image (Stream  : in ada.Streams.Stream_IO.Stream_access;
                         try_TGA : in Boolean) return Image
   is
   begin
      return Images.fetch_Image (Stream, try_TGA);
   end fetch_Image;



   function to_Image (image_Filename : in asset_Name)  return Image
   is
      File    :          ada.Streams.Stream_IO.File_type;
      up_Name : constant String := to_Upper (to_String (image_Filename));
   begin
      open (File, In_File, to_String (image_Filename));

      declare
         the_Image : constant Image
           := fetch_Image (Stream (File),
                           try_TGA =>          image_Filename'Length >= 4
                                      and then up_Name (up_Name'Last - 3 .. up_Name'Last) = ".TGA");
      begin
         close (File);
         return the_Image;
      end;
   end to_Image;



   function to_lucid_Image (image_Filename : in asset_Name) return lucid_Image
   is
      Unused : aliased Boolean;
   begin
      return to_lucid_Image (image_Filename, Unused'Access);
   end to_lucid_Image;



   function to_lucid_Image (image_Filename : in     asset_Name;
                            is_Lucid       : access Boolean) return lucid_Image
   is
      File      :          ada.Streams.Stream_IO.File_type;
      the_Image :          GID.Image_Descriptor;
      up_Name   : constant String := to_Upper (to_String (image_Filename));

      next_Frame :          ada.Calendar.Day_Duration := 0.0;

   begin
      open (File, in_File, to_String (image_Filename));

      GID.load_Image_Header (the_Image,
                             Stream (File).all,
                             try_TGA =>          image_Filename'Length >= 4
                                        and then up_Name (up_Name'Last - 3 .. up_Name'Last) = ".TGA");
      declare
         image_Width  : constant Positive := GID.Pixel_Width  (the_Image);
         image_Height : constant Positive := GID.Pixel_Height (the_Image);

         Frame : lucid_Image (1 .. Index_t (image_Height),
                              1 .. Index_t (image_Width));

         procedure load_raw_Image
         is
            subtype primary_Color_range is GL.GLubyte;

            Row, Col : Index_t;


            procedure set_X_Y (X, Y : Natural)
            is
            begin
               Col := Index_t (X + 1);
               Row := Index_t (Y + 1);
            end set_X_Y;


            procedure put_Pixel (Red, Green, Blue : primary_Color_range;
                                 Alpha            : primary_Color_range)
            is
               use type GL.GLubyte, Real;
            begin
               Frame (Row, Col) := ((Red, Green, Blue), Alpha);

               if Col = Index_t (image_Width)
               then     -- GID requires us to look to next pixel on the right for next time.
                  Row := Row + 1;
                  Col := 1;
               else
                  Col := Col + 1;
               end if;

               if Alpha /= opaque_Value
               then
                  is_Lucid.all := True;
               end if;
            end put_Pixel;


            procedure Feedback (Percents : Natural) is null;

            procedure load_Image is new GID.load_Image_contents (primary_Color_range,
                                                                 set_X_Y,
                                                                 put_Pixel,
                                                                 Feedback,
                                                                 GID.fast);
         begin
            load_Image (the_Image, next_Frame);
         end Load_raw_image;

      begin
         is_Lucid.all := False;

         load_raw_Image;
         close (File);

         return Frame;
      end;
   end to_lucid_Image;



   function to_Texture (image_Filename : in asset_Name) return Texture.Object
   is
      use Texture;

      is_Lucid        : aliased  Boolean;
      the_lucid_Image : constant lucid_Image    := to_lucid_Image (image_Filename, is_Lucid'Access);
      the_Texture     :          Texture.Object := Forge.to_Texture (Texture.Dimensions' (the_lucid_Image'Length (2),
                                                                                          the_lucid_Image'Length (1)));
   begin
      if is_Lucid
      then
         set_Image (the_Texture, the_lucid_Image);
      else
         declare
            the_opaque_Image : constant Image := to_Image (the_lucid_Image);
         begin
            set_Image (the_Texture, the_opaque_Image);
         end;
      end if;

      return the_Texture;
   end to_Texture;



   procedure destroy (Self : in out Model)
   is
      procedure free is new ada.unchecked_Deallocation (bone_Weights,       bone_Weights_view);
      procedure free is new ada.unchecked_Deallocation (bone_Weights_array, bone_Weights_array_view);
   begin
      free (Self.Sites);
      free (Self.Coords);
      free (Self.Normals);

      if Self.Weights /= null
      then
         for Each in Self.Weights'Range
         loop
            free (Self.Weights (Each));
         end loop;

         free (Self.Weights);
      end if;

      for Each in Self.Faces'Range
      loop
         destroy (Self.Faces (Each));
      end loop;

      free (Self.Faces);
   end destroy;


   --------------------------------
   --- Screenshot and Video Capture
   --

   type U8  is mod 2 **  8;   for U8 'Size use  8;
   type U16 is mod 2 ** 16;   for U16'Size use 16;
   type U32 is mod 2 ** 32;   for U32'Size use 32;

   type I32 is range -2 ** 31 .. 2 ** 31 - 1;
   for  I32'Size use 32;


   generic
      type Number is mod <>;
      S : Stream_Access;
   procedure write_Intel_x86_Number (N : in Number);


   procedure write_Intel_x86_Number (N : in Number)
   is
      M     :          Number  := N;
      Bytes : constant Integer := Number'Size / 8;
   begin
      for i in 1 .. bytes
      loop
         U8'write (S, U8 (M mod 256));
         M := M / 256;
      end loop;
   end write_Intel_x86_Number;



   procedure write_raw_BGR_Frame (Stream        : Stream_Access;
                                  Width, Height : Natural)
   is
      use GL,
          GL.Binding,
          openGL.Texture;

      -- 4-byte padding for .bmp/.avi formats is the same as GL's default
      -- padding: see glPixelStore, GL_[UN]PACK_ALIGNMENT = 4 as initial value.
      -- http://www.openGL.org/sdk/docs/man/xhtml/glPixelStore.xml
      --
      padded_row_Size : constant Positive := 4 * Integer (Float'Ceiling (Float (Width) * 3.0 / 4.0));
      -- (in bytes)


      type temp_Bitmap_type is array (Natural range <>) of aliased gl.GLUbyte;

      PicData: temp_Bitmap_type (0 .. (padded_row_Size + 4) * (Height + 4) - 1);
      -- No dynamic allocation needed!
      -- The "+4" are there to avoid parity address problems when GL writes to the buffer.

      type Loc_pointer is new gl.safe.GLvoid_Pointer;

      function convert is new ada.unchecked_Conversion (System.Address, Loc_pointer);
      -- This method is functionally identical as GNAT's Unrestricted_Access
      -- but has no type safety (cf GNAT Docs).

      pragma no_strict_Aliasing (Loc_pointer);     -- Recommended by GNAT 2005+.

      pPicData :          Loc_pointer;
      data_Max : constant Integer    := padded_row_Size * Height - 1;

      -- Workaround for the severe xxx'Read xxx'Write performance
      -- problems in the GNAT and ObjectAda compilers (as in 2009)
      -- This is possible if and only if Byte = Stream_Element and
      -- arrays types are both packed the same way.
      --
      type Byte_array is array (Integer range <>) of aliased GLUByte;

      subtype Size_Test_a is Byte_array (1 .. 19);
      subtype Size_Test_b is ada.Streams.Stream_Element_array (1 .. 19);

      workaround_possible: constant Boolean :=          Size_Test_a'Size      = Size_Test_b'Size
                                               and then Size_Test_a'Alignment = Size_Test_b'Alignment;
   begin
      Tasks.check;

      pPicData:= Convert (PicData (0)'Address);

      GLReadPixels (0, 0,
                    GLSizei (Width),
                    GLSizei (Height),
                    to_GL (Texture.BGR),
                    GL.GL_UNSIGNED_BYTE,
                    pPicData);
      Errors.log;

      if workaround_possible
      then
         declare
            use ada.Streams;

            SE_Buffer : Stream_Element_array (0 .. Stream_Element_Offset (PicData'Last));

            for SE_Buffer'Address use PicData'Address;
            pragma import (Ada, SE_Buffer);
         begin
            ada.Streams.write (Stream.all, SE_Buffer (0 .. Stream_Element_Offset (data_Max)));
         end;

      else
         temp_Bitmap_type'write (Stream, PicData (0 .. data_Max));
      end if;

   end write_raw_BGR_Frame;



   procedure write_raw_BGRA_Frame (Stream        : Stream_access;
                                   Width, Height : Natural)
   is
      use GL,
          GL.Binding,
          Texture;

      -- 4-byte padding for .bmp/.avi formats is the same as GL's default
      -- padding: see glPixelStore, GL_[UN]PACK_ALIGNMENT = 4 as initial value.
      -- http://www.openGL.org/sdk/docs/man/xhtml/glPixelStore.xml
      --
      padded_row_Size : constant Positive:= 4 * Integer (Float'Ceiling (Float (Width)));
      -- (in bytes)

      type temp_Bitmap_type is array (Natural range <>) of aliased gl.GLUbyte;

      PicData: temp_Bitmap_type (0.. (padded_row_size + 4) * (height + 4) - 1);
      -- No dynamic allocation needed!
      -- The "+4" are there to avoid parity address problems when GL writes
      -- to the buffer.

      type Loc_pointer is new gl.safe.GLvoid_Pointer;

      function convert is new ada.unchecked_Conversion (System.Address, Loc_pointer);
      -- This method is functionally identical as GNAT's Unrestricted_Access
      -- but has no type safety (cf GNAT Docs).

      pragma no_strict_Aliasing (loc_pointer); -- Recommended by GNAT 2005+.

      pPicData :          Loc_pointer;
      data_Max : constant Integer    := padded_row_Size * Height - 1;

      -- Workaround for the severe xxx'Read xxx'Write performance
      -- problems in the GNAT and ObjectAda compilers (as in 2009)
      -- This is possible if and only if Byte = Stream_Element and
      -- arrays types are both packed the same way.
      --
      type Byte_array is array (Integer range <>) of aliased GLUByte;

      subtype Size_Test_a is Byte_Array (1..19);
      subtype Size_Test_b is ada.Streams.Stream_Element_array (1 .. 19);

      workaround_possible: constant Boolean :=          Size_Test_a'Size      = Size_Test_b'Size
                                               and then Size_Test_a'Alignment = Size_Test_b'Alignment;
   begin
      Tasks.check;

      pPicData:= convert (PicData (0)'Address);

      GLReadPixels (0, 0,
                    GLSizei (width),
                    GLSizei (height),
                    to_GL (openGL.Texture.BGRA),
                    GL.GL_UNSIGNED_BYTE,
                    pPicData);
      Errors.log;

      if workaround_possible
      then
         declare
            use ada.Streams;

            SE_Buffer : Stream_Element_array (0 .. Stream_Element_Offset (PicData'Last));

            for SE_Buffer'Address use PicData'Address;
            pragma Import (Ada, SE_Buffer);
         begin
            ada.Streams.write (Stream.all, SE_Buffer (0 .. Stream_Element_Offset (data_Max)));
         end;

      else
         temp_Bitmap_type'write (Stream, PicData (0 .. data_Max));
      end if;

   end write_raw_BGRA_Frame;


   -------------
   -- Screenshot
   --

   subtype FXPT2DOT30 is U32;

   type CIEXYZ is
      record
         ciexyzX : FXPT2DOT30;
         ciexyzY : FXPT2DOT30;
         ciexyzZ : FXPT2DOT30;
      end record;

   type CIEXYZTRIPLE is
      record
         ciexyzRed   : CIEXYZ;
         ciexyzGreen : CIEXYZ;
         ciexyzBlue  : CIEXYZ;
      end record;

   type BITMAPFILEHEADER is
      record
         bfType      : U16;
         bfSize      : U32;
         bfReserved1 : U16 := 0;
         bfReserved2 : U16 := 0;
         bfOffBits   : U32;
      end record;
   pragma pack (BITMAPFILEHEADER);
   for          BITMAPFILEHEADER'Size use 8 * 14;

   type BITMAPINFOHEADER is
      record
         biSize          : U32;
         biWidth         : I32;
         biHeight        : I32;
         biPlanes        : U16;
         biBitCount      : U16;
         biCompression   : U32;
         biSizeImage     : U32;
         biXPelsPerMeter : I32 := 0;
         biYPelsPerMeter : I32 := 0;
         biClrUsed       : U32 := 0;
         biClrImportant  : U32 := 0;
      end record;
   pragma pack (BITMAPINFOHEADER);
   for          BITMAPINFOHEADER'Size use 8 * 40;

   type BITMAPV4HEADER is
      record
         Core          : BITMAPINFOHEADER;
         bV4RedMask    : U32;
         bV4GreenMask  : U32;
         bV4BlueMask   : U32;
         bV4AlphaMask  : U32;
         bV4CSType     : U32;
         bV4Endpoints  : CIEXYZTRIPLE;
         bV4GammaRed   : U32;
         bV4GammaGreen : U32;
         bV4GammaBlue  : U32;
      end record;
   pragma pack (BITMAPV4HEADER);
   for          BITMAPV4HEADER'Size use 8 * 108;


   procedure opaque_Screenshot (Filename : in String)
   is
      use GL,
          GL.Binding;

      File       : ada.Streams.Stream_IO.File_Type;
      FileInfo   : BITMAPINFOHEADER;
      FileHeader : BITMAPFILEHEADER;

      Viewport   : array (0 .. 3) of aliased GLint;
   begin
      Tasks.check;

      glGetIntegerv (GL_VIEWPORT,
                     Viewport (0)'Unchecked_Access);
      Errors.log;

      FileHeader.bfType    := 16#4D42#;     -- 'BM'
      FileHeader.bfOffBits :=   BITMAPINFOHEADER'Size / 8
                              + BITMAPFILEHEADER'Size / 8;

      FileInfo.biSize        := BITMAPINFOHEADER'Size / 8;
      FileInfo.biWidth       := I32 (Viewport (2));
      FileInfo.biHeight      := I32 (Viewport (3));
      FileInfo.biPlanes      := 1;
      FileInfo.biBitCount    := 24;
      FileInfo.biCompression := 0;
      FileInfo.biSizeImage   := U32 (  4
                                     * Integer (Float'Ceiling (Float (FileInfo.biWidth) * 3.0 / 4.0))
                                     * Integer (FileInfo.biHeight));
      FileHeader.bfSize      := FileHeader.bfOffBits + FileInfo.biSizeImage;

      create (File, out_File, Filename);
      declare
         procedure write_Intel is new write_Intel_x86_Number (U16, Stream (File));
         procedure write_Intel is new write_Intel_x86_Number (U32, Stream (File));
         function  convert     is new ada.unchecked_Conversion (I32, U32);
      begin
         -- ** Endian-safe: ** --
         write_Intel (FileHeader.bfType);
         write_Intel (FileHeader.bfSize);
         write_Intel (FileHeader.bfReserved1);
         write_Intel (FileHeader.bfReserved2);
         write_Intel (FileHeader.bfOffBits);
         --
         write_Intel (         FileInfo.biSize);
         write_Intel (convert (FileInfo.biWidth));
         write_Intel (convert (FileInfo.biHeight));
         write_Intel (         FileInfo.biPlanes);
         write_Intel (         FileInfo.biBitCount);
         write_Intel (         FileInfo.biCompression);
         write_Intel (         FileInfo.biSizeImage);
         write_Intel (convert (FileInfo.biXPelsPerMeter));
         write_Intel (convert (FileInfo.biYPelsPerMeter));
         write_Intel (         FileInfo.biClrUsed);
         write_Intel (         FileInfo.biClrImportant);
         --
         write_raw_BGR_Frame (Stream (File),
                              Integer (Viewport (2)),
                              Integer (Viewport (3)));
         Close (File);

      exception
         when others =>
            Close (File);
            raise;
      end;
   end opaque_Screenshot;



   procedure lucid_Screenshot (Filename : in String)
   is
      use GL,
          GL.Binding;

      File        : ada.Streams.Stream_IO.File_type;

      FileHeader  : BITMAPFILEHEADER;
      FileInfo    : BITMAPV4HEADER;

      Viewport    : array (0 .. 3) of aliased GLint;

   begin
      Tasks.check;

      glGetIntegerv (GL_VIEWPORT,
                     Viewport (0)'Unchecked_Access);
      Errors.log;

      FileHeader.bfType    := 16#4D42#;     -- 'BM'
      FileHeader.bfOffBits :=   BITMAPV4HEADER  'Size / 8
                              + BITMAPFILEHEADER'Size / 8;

      FileInfo.Core.biSize        := BITMAPV4HEADER'Size / 8;
      FileInfo.Core.biWidth       := I32 (Viewport (2));
      FileInfo.Core.biHeight      := I32 (Viewport (3));
      FileInfo.Core.biPlanes      :=  1;
      FileInfo.Core.biBitCount    := 32;
      FileInfo.Core.biCompression :=  3;
      FileInfo.Core.biSizeImage   := U32 (  4     -- 4-byte padding for '.bmp/.avi' formats.
                                          * Integer (Float'Ceiling (Float (FileInfo.Core.biWidth)))
                                          * Integer (FileInfo.Core.biHeight));

      FileInfo.bV4RedMask    := 16#00FF0000#;
      FileInfo.bV4GreenMask  := 16#0000FF00#;
      FileInfo.bV4BlueMask   := 16#000000FF#;
      FileInfo.bV4AlphaMask  := 16#FF000000#;
      FileInfo.bV4CSType     := 0;
      FileInfo.bV4Endpoints  := (others => (others => 0));
      FileInfo.bV4GammaRed   := 0;
      FileInfo.bV4GammaGreen := 0;
      FileInfo.bV4GammaBlue  := 0;

      FileHeader.bfSize := FileHeader.bfOffBits + FileInfo.Core.biSizeImage;

      Create (File, out_File, Filename);
      declare
         procedure write_Intel is new write_Intel_x86_Number (U16, Stream (File));
         procedure write_Intel is new write_Intel_x86_Number (U32, Stream (File));
         function  convert     is new ada.unchecked_Conversion (I32, U32);
      begin
         -- ** Endian-safe: ** --
         write_Intel (FileHeader.bfType);
         write_Intel (FileHeader.bfSize);
         write_Intel (FileHeader.bfReserved1);
         write_Intel (FileHeader.bfReserved2);
         write_Intel (FileHeader.bfOffBits);
         --
         write_Intel (         FileInfo.Core.biSize);
         write_Intel (convert (FileInfo.Core.biWidth));
         write_Intel (convert (FileInfo.Core.biHeight));
         write_Intel (         FileInfo.Core.biPlanes);
         write_Intel (         FileInfo.Core.biBitCount);
         write_Intel (         FileInfo.Core.biCompression);
         write_Intel (         FileInfo.Core.biSizeImage);
         write_Intel (convert (FileInfo.Core.biXPelsPerMeter));
         write_Intel (convert (FileInfo.Core.biYPelsPerMeter));
         write_Intel (         FileInfo.Core.biClrUsed);
         write_Intel (         FileInfo.Core.biClrImportant);

         write_Intel (FileInfo.bV4RedMask);
         write_Intel (FileInfo.bV4GreenMask);
         write_Intel (FileInfo.bV4BlueMask);
         write_Intel (FileInfo.bV4AlphaMask);
         write_Intel (FileInfo.bV4CSType);

         write_Intel (FileInfo.bV4Endpoints.ciexyzRed.ciexyzX);
         write_Intel (FileInfo.bV4Endpoints.ciexyzRed.ciexyzY);
         write_Intel (FileInfo.bV4Endpoints.ciexyzRed.ciexyzZ);

         write_Intel (FileInfo.bV4Endpoints.ciexyzGreen.ciexyzX);
         write_Intel (FileInfo.bV4Endpoints.ciexyzGreen.ciexyzY);
         write_Intel (FileInfo.bV4Endpoints.ciexyzGreen.ciexyzZ);

         write_Intel (FileInfo.bV4Endpoints.ciexyzBlue.ciexyzX);
         write_Intel (FileInfo.bV4Endpoints.ciexyzBlue.ciexyzY);
         write_Intel (FileInfo.bV4Endpoints.ciexyzBlue.ciexyzZ);

         write_Intel (FileInfo.bV4GammaRed);
         write_Intel (FileInfo.bV4GammaGreen);
         write_Intel (FileInfo.bV4GammaBlue);

         write_raw_BGRA_Frame (Stream (File),
                               Integer (Viewport (2)),
                               Integer (Viewport (3)));
         close (File);

      exception
         when others =>
            Close (File);
            raise;
      end;
   end lucid_Screenshot;



   procedure Screenshot (Filename : in String;   with_Alpha : in Boolean := False)
   is
   begin
      if with_Alpha
      then  lucid_Screenshot (Filename);
      else opaque_Screenshot (Filename);
      end if;
   end Screenshot;


   ----------------
   -- Video Capture
   --

   --  We define global variables since it is not expected
   --  that more that one capture is taken at the same time.
   --
   avi           : ada.Streams.Stream_IO.File_type;
   frames        : Natural;
   rate          : Positive;
   width, height : Positive;
   bmp_size      : U32;

   procedure write_RIFF_Headers
   is
      --  Written 1st time to take place (but # of frames unknown)
      --  Written 2nd time for setting # of frames, sizes, etc.
      --
      calc_bmp_size    : constant U32           := U32 (((width)) * height * 3);
      --  !! stuff to multiple of 4 !!
      index_size       : constant U32           := U32 (frames) * 16;
      movie_size       : constant U32           := 4 + U32 (frames) * (calc_bmp_size + 8);
      second_list_size : constant U32           := 4 + 64 + 48;
      first_list_size  : constant U32           := (4 + 64) + (8 + second_list_size);
      file_size        : constant U32           := 8 + (8 + first_list_size) + (4 + movie_size) + (8 + index_size);
      Stream           : constant Stream_access := ada.Streams.Stream_IO.Stream (avi);

      procedure write_Intel is new write_Intel_x86_Number (U16, Stream);
      procedure write_Intel is new write_Intel_x86_Number (U32, Stream);

      microseconds_per_frame : constant U32 := U32 (1_000_000.0 / long_Float (rate));
   begin
      bmp_size := calc_bmp_size;

      String'write (Stream, "RIFF");
      U32   'write (Stream, file_size);
      String'write (Stream, "AVI ");
      String'write (Stream, "LIST");
      write_Intel  (first_list_size);
      String'write (Stream, "hdrl");
      String'write (Stream, "avih");
      write_Intel  (U32' (56));

      --  Begin of AVI Header
      write_Intel (microseconds_per_frame);
      write_Intel (U32'(0));      -- MaxBytesPerSec
      write_Intel (U32'(0));      -- Reserved1
      write_Intel (U32'(16));     -- Flags (16 = has an index)
      write_Intel (U32 (frames));
      write_Intel (U32'(0));      -- InitialFrames
      write_Intel (U32'(1));      -- Streams
      write_Intel (bmp_size);
      write_Intel (U32 (width));
      write_Intel (U32 (height));
      write_Intel (U32'(0));      -- Scale
      write_Intel (U32'(0));      -- Rate
      write_Intel (U32'(0));      -- Start
      write_Intel (U32'(0));      -- Length
      --  End of AVI Header

      String'write (Stream, "LIST");
      write_Intel  (second_list_size);
      String'write (Stream, "strl");

      --  Begin of Str
      String'write (Stream, "strh");
      write_Intel  (U32'(56));
      String'write (Stream, "vids");
      String'write (Stream, "DIB ");
      write_Intel  (U32'(0));                -- flags
      write_Intel  (U32'(0));                -- priority
      write_Intel  (U32'(0));                -- initial frames
      write_Intel  (microseconds_per_frame); -- Scale
      write_Intel  (U32'(1_000_000));        -- Rate
      write_Intel  (U32'(0));                -- Start
      write_Intel  (U32 (frames));           -- Length
      write_Intel  (bmp_size);               -- SuggestedBufferSize
      write_Intel  (U32'(0));                -- Quality
      write_Intel  (U32'(0));                -- SampleSize
      write_Intel  (U32'(0));
      write_Intel  (U16 (width));
      write_Intel  (U16 (height));
      --  End of Str

      String'write (Stream, "strf");
      write_Intel (U32'(40));

      --  Begin of BMI
      write_Intel (U32'(40));    -- BM header size (like BMP)
      write_Intel (U32 (width));
      write_Intel (U32 (height));
      write_Intel (U16'(1));     -- Planes
      write_Intel (U16'(24));    -- BitCount
      write_Intel (U32'(0));     -- Compression
      write_Intel (bmp_size);    -- SizeImage
      write_Intel (U32'(3780));  -- XPelsPerMeter
      write_Intel (U32'(3780));  -- YPelsPerMeter
      write_Intel (U32'(0));     -- ClrUsed
      write_Intel (U32'(0));     -- ClrImportant
      --  End of BMI

      String'write (Stream, "LIST");
      write_Intel  (movie_size);
      String'write (Stream, "movi");
   end Write_RIFF_headers;



   procedure start_Capture (AVI_Name   : String;
                            frame_Rate : Positive)
   is
      use GL,
          GL.Binding;
      Viewport : array (0 .. 3) of aliased GLint;
   begin
      Tasks.check;

      create (Avi, out_File, AVI_Name);

      Frames := 0;
      Rate   := frame_Rate;

      glGetIntegerv (GL_VIEWPORT,
                     Viewport (0)'unchecked_Access);
      Errors.log;

      Width  := Positive (Viewport (2));
      Height := Positive (Viewport (3));
      --  NB: GL viewport resizing should be blocked during the video capture !
      write_RIFF_Headers;
   end start_Capture;



   procedure capture_Frame
   is
      S : constant Stream_Access := Stream (Avi);
      procedure Write_Intel is new Write_Intel_x86_number (U32, s);
   begin
      String'write        (S, "00db");
      write_Intel         (bmp_Size);
      write_raw_BGR_frame (S, Width, Height);

      Frames := Frames + 1;
   end capture_Frame;



   procedure stop_Capture
   is
      index_Size  : constant U32           := U32 (Frames) * 16;
      S           : constant Stream_Access := Stream (Avi);
      ChunkOffset :          U32           := 4;

      procedure write_Intel is new write_Intel_x86_Number (U32, S);
   begin
      --  Write the index section
      --
      String'write (S, "idx1");
      write_Intel (index_Size);

      for f in 1 .. Frames
      loop
         String'write (S, "00db");
         write_Intel  (U32'(16));     -- Keyframe.
         write_Intel  (ChunkOffset);
         ChunkOffset := ChunkOffset + bmp_Size + 8;
         write_Intel  (bmp_Size);
      end loop;

      Set_Index (avi, 1);     -- Go back to file beginning.
      write_RIFF_Headers;     -- Rewrite headers with correct data.
      close (Avi);
   end stop_Capture;


end openGL.IO;
