with
     openGL.Glyph.Container,
     openGL.FontImpl,

     freetype.Face,
     freetype_c.FT_GlyphSlot,

     ada.Containers.hashed_Maps;

package openGL.Font
--
--  Specific font classes are derived from this class. It uses the helper
--  classes 'freetype_c.Face' and 'freetype_c.FTSize' to access the Freetype library.
--
--  This class is abstract and derived classes must implement the protected
--  'MakeGlyph' function to create glyphs of the appropriate type.
--
is
   type Item is abstract tagged limited private;
   type View is access all Item'Class;


   ----------
   -- Font_Id
   --

   type font_Id is
      record
         Name : asset_Name;
         Size : Integer;
      end record;

   function Hash (the_Id : in font_Id) return ada.Containers.Hash_type;



   ------------
   -- Font Maps
   --
   package font_id_Maps_of_font is new ada.Containers.hashed_Maps (Key_Type        => font_Id,
                                                                   Element_Type    => Font.view,
                                                                   Hash            => Hash,
                                                                   Equivalent_Keys => "=");
   subtype font_id_Map_of_font  is font_id_Maps_of_font.Map;


   ---------
   --  Forge
   --

   procedure destruct (Self : in out Item);
   procedure free     (Self : in out View);


   --------------
   --  Attributes
   --

   function CharMap (Self : in Item;   Encoding : in freetype_c.FT_Encoding) return Boolean;
   --
   --  Set the character map for the face.
   --
   --  Encoding: Freetype enumerate for char map code.
   --
   --  Returns True if charmap was valid and set correctly.


   function CharMapCount (Self : in Item) return Natural;
   --
   --  Get the number of character maps in this face.
   --
   --  Returns the character map count.


   function CharMapList (Self : access Item) return freetype.face.FT_Encodings_view;
   --
   --  Get a list of character maps in this face.
   --
   --  Returns aceess to the array of encodings.


   function Ascender (Self : in Item) return Real;
   --
   --  Get the global ascender height for the face.
   --
   --  Returns the Ascender height.


   function Descender (Self : in Item) return Real;
   --
   --  Gets the global descender height for the face.
   --
   --  Returns the Descender height.


   function  LineHeight  (Self : in Item) return Real;
   --
   --  Gets the line spacing for the font.
   --
   --  Returns the line height.


   function FaceSize (Self : access Item;   size         : in Natural;
                                            x_Res, y_Res : in Natural) return Boolean;
   --
   --  Set the character size for the current face.
   --
   --  Returns True if size was set correctly.


   function FaceSize (Self : in Item) return Natural;
   --
   --  Get the current face size in points (1/72 inch).
   --
   --  Returns the face size.


   procedure Depth (Self : in out Item;   Depth : in Real);
   --
   --  Set the extrusion distance for the font. Only implemented by FTExtrudeFont.
   --
   --  Depth: The extrusion distance.


   procedure Outset (Self : in out Item;   Outset : in Real);
   --
   --  Set the outset distance for the font. Only implemented by FTOutlineFont, FTPolygonFont and FTExtrudeFont.
   --
   --  Outset: The outset distance.


   procedure Outset (Self : in out Item;   Front : in Real;
                                           Back  : in Real);
   --
   --  Set the front and back outset distances for the font. Only implemented by FTExtrudeFont.
   --
   --  Front: The front outset distance.
   --  Back:  The  back outset distance.


   function BBox (Self : access Item;   Text     : in String;
                                        Length   : in Integer  := -1;
                                        Position : in Vector_3 := Origin_3D;
                                        Spacing  : in Vector_3 := Origin_3D) return Bounds;
   --
   --  Get the bounding box for a string.
   --
   --  Text:     A character buffer.
   --  Length:   The length of the string. If < 0 then all characters will be checked until a null character is encountered.
   --  Position: The pen position of the first character.
   --  Spacing:  A displacement vector to add after each character has been checked.
   --
   --  Returns the corresponding bounding box.


   function Error (Self : in Item) return freetype_c.FT_Error;
   --
   --  Queries the font for errors.
   --
   --  Returns the current error code.


   --------------
   --  Operations
   --

   function attach (Self : in Item;   Font_File_Path : in String) return Boolean;
   --
   --  Attach auxilliary file to font e.g font metrics.
   --  Note: Not all font formats implement this function.
   --
   --  fontFilePath: The auxilliary font file path.
   --
   --  Returns True if file has been attached successfully.


   function attach (Self : in Item;   pBufferBytes      : in FontImpl.unsigned_char_Pointer;
                                      bufferSizeInBytes : in Natural) return Boolean;
   --
   --  Attach auxilliary data to font e.g font metrics, from memory.
   --  Note: Not all font formats implement this function.
   --
   --  'pBufferBytes'          The in-memory buffer.
   --  'bufferSizeInBytes'     The length of the buffer in bytes.
   --
   --  Returns True if file has been attached successfully.


   procedure glyph_load_Flags (Self : in out Item;   Flags : in freetype_c.FT_Int);
   --
   --  Set the glyph loading flags. By default, fonts use the most
   --  sensible flags when loading a font's glyph using FT_Load_Glyph().
   --  This function allows to override the default flags.
   --
   --  Flags: The glyph loading flags.


   function Advance (Self : access Item;   Text    : in String;
                                           Length  : in Integer  := -1;
                                           Spacing : in Vector_3 := Origin_3D) return Real;
   --
   --  Get the advance for a string.
   --
   --  Text:    String to be checked.
   --  Length:  The length of the string. If < 0 then all characters will be checked until
   --           a null character is encountered.
   --  Spacing: A displacement vector to add after each character has been checked.
   --
   --  Returns the string's advance width.


   function kern_Advance (Self : in Item;    From, To : in Character) return Real;

   function x_PPEM  (Self : in Item) return Real;
   function x_Scale (Self : in Item) return Real;
   function y_Scale (Self : in Item) return Real;

   function check_Glyphs (Self : access Item;   Text     : in String;
                                                Length   : in Integer             := -1;
                                                Position : in Vector_3            := math.Origin_3D;
                                                Spacing  : in Vector_3            := math.Origin_3D;
                                                Mode     : in fontImpl.RenderMode := fontImpl.RENDER_ALL) return Vector_3;
   --
   --  Render a string of characters.
   --
   --  Text:     String to be output.
   --  Length:   The length of the string. If < 0 then all characters will be displayed until a null character is encountered.
   --  Position: The pen position of the first character.
   --  Spacing:  A displacement vector to add after each character has been displayed
   --  Mode:     Render mode to use for display.
   --
   --  Returns the new pen position after the last character was output.


   function MakeGlyph (Self : access Item;   Slot : in freetype_c.FT_GlyphSlot.item) return glyph.Container.Glyph_view
                       is abstract;
   --
   --  Construct a glyph of the correct type.
   --  Clients must override the function and return their specialised glyph.
   --
   --  Slot: A FreeType glyph slot.
   --
   --  Returns an FTGlyph or null on failure.



private

   type Item is abstract tagged limited
      record
         Impl : FontImpl.view;      -- Internal FTGL FTFont implementation object. For private use only.
      end record;


   procedure define (Self : in out Item;   fontFilePath : in String);
   --
   --  Open and read a font file. Sets Error flag.


   procedure define (Self : in out Item;   pBufferBytes      : in FontImpl.unsigned_char_Pointer;
                                           bufferSizeInBytes : in Natural);
   --
   --  Open and read a font from a buffer in memory. Sets Error flag.
   --  The buffer is owned by the client and is NOT copied by FTGL. The pointer must be valid while using FTGL.


   procedure define (Self : in out Item;   pImpl : in FontImpl.view);
   --
   --  Internal FTGL FTFont constructor. For private use only.
   --
   --  pImpl: An internal implementation object, which will be destroyed upon FTFont deletion.


end openGL.Font;
