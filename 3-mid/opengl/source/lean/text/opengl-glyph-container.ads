with
     freetype.Face,
     freetype.charMap;

private
with
     ada.Containers.Vectors;

package openGL.Glyph.Container
--
--  Contains the post processed Glyph objects.
--
is
   type Item       is tagged private;
   type Glyph_view is access all Glyph.item'Class;


   ---------
   --  Forge
   --

   function  to_glyph_Container (parent_Face : in freetype.Face.view) return glyph.Container.item;
   --
   -- parent_Face: The Freetype face.


   procedure destruct (Self : in out Item);


   --------------
   --  Attributes
   --

   function CharMap (Self : access Item;   Encoding : in freeType_c.FT_Encoding) return Boolean;
   --
   --  Sets the character map for the face.
   --
   -- Encoding: The Freetype encoding symbol.
   --
   --  Returns True if charmap was valid and set correctly.


   function FontIndex (Self : in Item;   Character : in freetype.charMap.characterCode) return Natural;
   --
   --  Get the font index of the input character.
   --
   --  Character: The character code of the requested glyph in the current encoding (eg apple roman).
   --
   --  Returns the font index for the character.


   procedure add (Self : in out Item;   Glyph     : in Glyph_view;
                                        Character : in freetype.charMap.characterCode);
   --
   --  Adds a glyph to this glyph list.
   --
   --  Glyph:     The FTGlyph to be inserted into the container.
   --  Character: The char code of the glyph NOT the glyph index.


   function Glyph (Self : in Item;   Character : in freetype.charMap.characterCode) return Glyph_view;
   --
   --  Get a glyph from the glyph list.
   --
   --  Character: The char code of the glyph NOT the glyph index.
   --
   --  Returns a Glyph or null is it hasn't been loaded.


   function BBox (Self : in Item;   Character : in freetype.charMap.characterCode) return Bounds;
   --
   --  Get the bounding box for a character.
   --
   --  Character: The char code of the glyph NOT the glyph index.


   function Advance (Self : in Item;   Character         : in freetype.charMap.characterCode;
                                       nextCharacterCode : in freetype.charMap.characterCode) return Real;
   --
   --  Character:         Glyph index of the character.
   --  nextCharacterCode: The next glyph in a string.
   --
   --  Returns the kerned advance width for a glyph.


   function Error (Self : in Item) return freetype_c.FT_Error;
   --
   --  Queries the glyph container for errors.
   --
   --  Returns the current error code.


   --------------
   --  Operations
   --

   function render (Self : access Item;   Character         : in freetype.charMap.characterCode;
                                          nextCharacterCode : in freetype.charMap.characterCode;
                                          penPosition       : in Vector_3;
                                          renderMode        : in Integer) return Vector_3;
   --
   --  Renders a character.
   --
   --  Character:         The glyph to be Rendered.
   --  nextCharacterCode: The next glyph in the string. Used for kerning.
   --  penPosition:       The position to Render the glyph.
   --  renderMode:        Render mode to display.
   --
   --  Returns the distance to advance the pen position after rendering,



private

   type    charMap_view  is access all freetype.charMap.item'class;
   package glyph_Vectors is new ada.Containers.Vectors (Positive, Glyph_view);

   type Item is tagged
      record
         Face    : freetype.Face.view;       -- The FTGL face.
         charMap : charMap_view;             -- The character map object associated with the current face.

         Glyphs  : glyph_Vectors.Vector;     -- A structure to hold the glyphs.
         Err     : freeType_c.FT_Error;      -- Current error code. Zero means no error.
      end record;

end openGL.Glyph.Container;
