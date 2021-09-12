with
     freetype.face_Size,
     freetype.charMap,

     freeType_C.FT_Face,
     freeType_C.FT_GlyphSlot,

     interfaces.C;

package freetype.Face
--
--  The Face class provides an abstraction layer for the Freetype Face.
--
is
   type Item is tagged private;
   type View is access all Item'Class;


   ---------
   --  Types
   --
   type FT_Encodings      is array (Positive range <>) of freeType_C.FT_Encoding;
   type FT_Encodings_view is access all FT_Encodings;


   ---------
   --  Forge
   --
   use Interfaces;

   package Forge
   is
      function to_Face (fontFilePath      : in String;
                        precomputeKerning : in Boolean) return Face.item;
      --
      --  Opens and reads a face file. Error is set.

      function to_Face (pBufferBytes      : access C.unsigned_char;              -- The in-memory buffer.
                        bufferSizeInBytes : in     Positive;                     -- The length of the buffer in bytes.
                        precomputeKerning : in     Boolean) return Face.item;
      --
      --  Read face data from an in-memory buffer. Error is set.

      procedure destruct (Self : in out Item);   -- Disposes of the current Freetype face.
   end Forge;


   --------------
   --  Attributes
   --

   function attach (Self : access Item;   fontFilePath : in String) return Boolean;
   --
   --  Attach auxilliary file to font (e.g., font metrics).
   --
   --  fontFilePath: Auxilliary font file path.
   --
   --  Returns true if file has opened successfully.


   function attach (Self : access Item;   pBufferBytes      : access C.unsigned_char;
                                          bufferSizeInBytes : in     Positive) return Boolean;
   --
   --  Attach auxilliary data to font (e.g., font metrics) from memory.
   --
   --  pBufferBytes:      The in-memory buffer.
   --  bufferSizeInBytes: The length of the buffer in bytes.
   --
   --  Returns true if file has opened successfully.


   function freetype_Face (Self : in     Item) return freeType_C.FT_Face.item;
   --
   --  Get the freetype face object.
   --
   --  Returns a pointer to an FT_Face.


   function Size (Self : access Item;   Size         : in Natural;
                                        x_Res, y_Res : in Natural) return freetype.face_Size.item;
   --
   --  Sets the char size for the current face.
   --  This doesn't guarantee that the size was set correctly. Clients should check errors.
   --
   --  Size:         The face size in points (1/72 inch).
   --  x_Res, y_Res: The resolution of the target device.
   --
   --  Returns FTSize object.


   function CharMapCount (Self : in     Item) return Natural;
   --
   --  Get the number of character maps in this face.
   --
   -- Return character map count.


   function CharMapList  (Self : access Item) return FT_Encodings_view;
   --
   --  Get a list of character maps in this face.
   --
   --  Returns a pointer to the first encoding.


   function KernAdvance  (Self : access Item;   Index1 : in Natural;
                                                Index2 : in Natural) return Vector_3;
   --
   --  Gets the kerning vector between two glyphs.


   function GlyphCount   (Self : in     Item) return Natural;
   --
   --  Gets the number of glyphs in the current face.


   function Glyph (Self : access Item;   Index      : in freetype.charMap.glyphIndex;
                                         load_Flags : in freeType_C.FT_Int) return freeType_C.FT_GlyphSlot.item;

   function Error (Self : in     Item) return freeType_C.FT_Error;
   --
   --  Return the current error code.



private

   use freeType_C;

   type Float_array      is array (C.size_t range <>) of aliased C.c_float;
   type Float_array_view is access all Float_array;


   type Item is tagged
      record
        ftFace           :         FT_Face  .item;      -- The Freetype face.
        charSize         : aliased face_Size.item;      -- The size object associated with this face.
        numGlyphs        :         Natural;             -- The number of glyphs in this face.
        fontEncodingList :         FT_Encodings_view;
        hasKerningTable  :         Boolean;             -- This face has kerning tables.
        kerningCache     :         Float_array_view;    -- If this face has kerning tables, we can cache them.
        Err              :         FT_Error;            -- Current error code. Zero means no error.
      end record;


      max_Precomputed : constant := 128;

      procedure BuildKerningCache (Self : in out Item);


end freetype.Face;
