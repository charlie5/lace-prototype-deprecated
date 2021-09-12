with
     freeType_C.FT_Face,
     freeType_C.FT_Size;

package freetype.face_Size
--
--  The face_Size class provides an abstraction layer for the Freetype Size type.
--
is
   type Item is tagged private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   procedure destruct (Self : in out Item) is null;


   --------------
   --- Attributes
   --

   function CharSize  (Self : access Item;   Face         : in freeType_C.FT_Face.item;
                                             point_Size   : in Natural;
                                             x_Resolution,
                                             y_Resolution : in Natural) return Boolean;
   --
   --  Sets the char size for the current face.
   --
   --  This doesn't guarantee that the size was set correctly. Clients should call 'check Error' for
   --  more information if this function returns false. If an error does occur the size object isn't modified.
   --
   --  Face:         Parent face for this size object.
   --  point_Size:   The face size in points (1/72 inch).
   --  x_Resolution: The horizontal resolution of the target device.
   --  y_Resolution: The vertical resolution of the target device.
   --
   --  Returns true if the size has been set.


   function CharSize  (Self : in Item) return Natural;   -- Returns the char size in points.
   --
   --  Get the char size for the current face.


   function Ascender  (Self : in Item) return Float;     -- Returns the Ascender height.
   --
   --  Gets the global ascender height for the face in pixels.


   function Descender (Self : in Item) return Float;     -- Returns the Descender height.
   --
   --  Gets the global descender height for the face in pixels.


   function Height    (Self : in Item) return Float;     -- Returns the height in pixels.
   --
   --  Gets the global face height for the face.
   --
   --  If the face is scalable this returns the height of the global
   --  bounding box which ensures that any glyph will be less than or
   --  equal to this height. If the font isn't scalable there is no
   --  guarantee that glyphs will not be taller than this value.


   function Width     (Self : in Item) return Float;   -- Returns the width in pixels.
   --
   --  Gets the global face width for the face.
   --
   --  If the face is scalable this returns the width of the global
   --  bounding box which ensures that any glyph will be less than or
   --  equal to this width. If the font isn't scalable this value is
   --  the max_advance for the face.


   function Underline (Self : in Item) return Float;   -- Returns the underline position in pixels.
   --
   --  Gets the underline position for the face.


   function Error     (Self : in Item) return freeType_C.FT_Error;   -- Returns the current error code.
   --
   --  Queries for errors.



private

   type Item is tagged
      record
         ftFace      : freeType_C.FT_Face.item;    -- The current Freetype face that this FTSize object relates to.
         ftSize      : freeType_C.FT_Size.item;    -- The freetype Size.

         Size        : Natural := 0;               -- The size in points.
         xResolution,                              -- The horizontal resolution.
         yResolution : Natural := 0;               -- The vertical resolution.

         Err         : freeType_C.FT_Error := 0;   -- Current error code. Zero means no error.
      end record;

end freetype.face_Size;
