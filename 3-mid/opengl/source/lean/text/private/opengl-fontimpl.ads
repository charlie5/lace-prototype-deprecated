with
     openGL.Glyph.Container,
     freetype.Face,
     freetype.charMap,
     Freetype_C,
     interfaces.C.Pointers;

limited
with
     openGL.Font;

private
with
     freetype.face_Size;

package openGL.FontImpl
--
--  Implements an openGL font.
--
is
   type Item is tagged limited private;
   type View is access all Item'Class;


   ---------
   --  Types
   --

   type RenderMode is (RENDER_FRONT, RENDER_BACK, RENDER_SIDE, RENDER_ALL);

   for RenderMode use (RENDER_FRONT => 16#0001#,
                       RENDER_BACK  => 16#0002#,
                       RENDER_SIDE  => 16#0004#,
                       RENDER_ALL   => 16#ffff#);

   type TextAlignment is (ALIGN_LEFT, ALIGN_CENTER, ALIGN_RIGHT, ALIGN_JUSTIFY);

   for TextAlignment use (ALIGN_LEFT    => 0,
                          ALIGN_CENTER  => 1,
                          ALIGN_RIGHT   => 2,
                          ALIGN_JUSTIFY => 3);


   --  unsigned_char_Pointer
   --

   use Interfaces;

   type    unsigned_char_array    is array (C.size_t range <>) of aliased C.unsigned_char;

   package unsigned_char_Pointers is new C.Pointers (Index              => C.size_t,
                                                     Element            => C.unsigned_char,
                                                     Element_array      => unsigned_char_array,
                                                     default_Terminator => 0);
   subtype unsigned_char_Pointer  is unsigned_char_Pointers.Pointer;


   ---------
   --  Forge
   --

   procedure define   (Self : access Item;   ftFont            : access Font.item'Class;
                                             fontFilePath      : in     String);

   procedure define   (Self : access Item;   ftFont            : access Font.item'Class;
                                             pBufferBytes      : access C.unsigned_char;
                                             bufferSizeInBytes : in     Integer);
   procedure destruct (Self : in out Item);


   ---------------
   --  'Protected' ~ For derived class use only.
   --

   function  Err            (Self : in     Item) return freetype_c.FT_Error;

   function  attach         (Self : access Item;   fontFilePath      : in     String)  return Boolean;
   function  attach         (Self : access Item;   pBufferBytes      : access C.unsigned_char;
                                                   bufferSizeInBytes : in     Integer) return Boolean;

   function  FaceSize       (Self : access Item;   Size     : in Natural;
                                                   x_Res,
                                                   y_Res    : in Natural) return Boolean;
   function  FaceSize       (Self : in     Item)                          return Natural;

   procedure Depth          (Self : in out Item;   Depth    : in Real);
   procedure Outset         (Self : in out Item;   Outset   : in Real);
   procedure Outset         (Self : in out Item;   Front    : in Real;
                                                   Back     : in Real);

   procedure GlyphLoadFlags (Self : in out Item;   Flags    : in freetype_c.FT_Int);

   function  CharMap        (Self : access Item;   Encoding : in freetype_c.FT_Encoding) return Boolean;
   function  CharMapCount   (Self : in     Item)                                         return Natural;
   function  CharMapList    (Self : access Item)                                         return freetype.face.FT_Encodings_view;

   function  Ascender       (Self : in     Item) return Real;
   function  Descender      (Self : in     Item) return Real;
   function  LineHeight     (Self : in     Item) return Real;

   function  BBox           (Self : access Item;   Text     : in String;
                                                   Length   : in Integer;
                                                   Position : in Vector_3;
                                                   Spacing  : in Vector_3)  return Bounds;

   function  Advance        (Self : access Item;   Text     : in String;
                                                   Length   : in Integer;
                                                   Spacing  : in Vector_3)  return Real;

   function  kern_Advance   (Self : in     Item;   From, To : in Character) return Real;


   function  x_PPEM         (Self : in     Item) return Real;
   function  x_Scale        (Self : in     Item) return Real;
   function  y_Scale        (Self : in     Item) return Real;

   function  render         (Self : access Item;   Text       : in String;
                                                   Length     : in Integer;
                                                   Position   : in Vector_3;
                                                   Spacing    : in Vector_3;
                                                   renderMode : in Integer) return Vector_3;


private

   type glyph_Container_view is access all openGL.Glyph.Container.item'Class;


   type Item is tagged limited
      record
         Face       : aliased freetype.Face.item;          -- Current face object.
         charSize   :         freetype.face_Size.item;     -- Current size object.

         load_Flags :         freetype_c.FT_Int;           -- The default glyph loading flags.
         Err        :         freetype_c.FT_Error;         -- Current error code. Zero means no error.

         Intf       : access  Font.item'Class;             -- A link back to the interface of which we implement.
         glyphList  :         Glyph_Container_view;        -- An object that holds a list of glyphs

         Pen        :         Vector_3;                    -- Current pen or cursor position;
      end record;


   function CheckGlyph (Self : access Item;   Character : in freetype.charmap.CharacterCode) return Boolean;
   --
   --  Check that the glyph at <code>chr</code> exist. If not load it.
   --
   --  Character: The character index.
   --
   --  Returns true if the glyph can be created.


end openGL.FontImpl;
