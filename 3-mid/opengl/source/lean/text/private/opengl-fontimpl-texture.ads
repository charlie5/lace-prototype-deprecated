with
     openGL.FontImpl,
     openGL.Texture,
     openGL.GlyphImpl.texture,

     freetype_c.FT_GlyphSlot,

     ada.Containers.Vectors;

private
with
     GL;

package openGL.FontImpl.texture
--
--  Implements a texture font.
--
is
   type Item is new FontImpl.item with private;
   type View is access all Item'Class;


   ---------
   --  Forge
   --

   function  to_FontImpl_texture (ftFont            : access openGL.Font.item'Class;
                                  fontFilePath      : in     String)  return fontImpl.texture.item;

   function new_FontImpl_texture (ftFont            : access openGL.Font.item'Class;
                                  fontFilePath      : in     String)  return access fontImpl.texture.item'Class;

   function  to_FontImpl_texture (ftFont            : access openGL.Font.item'Class;
                                  pBufferBytes      : in     unsigned_char_Pointer;
                                  bufferSizeInBytes : in     Natural) return fontImpl.texture.item;

   function new_FontImpl_texture (ftFont            : access openGL.Font.item'Class;
                                  pBufferBytes      : in     unsigned_char_Pointer;
                                  bufferSizeInBytes : in     Natural) return access fontImpl.texture.item'Class;
   overriding
   procedure destruct (Self : in out Item);


   --------------
   --  Attributes
   --

   overriding
   function FaceSize (Self : access Item;   Size     : in Natural;
                                            x_Res,
                                            y_Res    : in Natural := 72) return Boolean;
   --
   --  Set the char size for the current face.
   --
   --  Returns True if size was set correctly.


   function render   (Self : access Item;   Text     : in String;
                                            Length   : in Integer;
                                            Position : in Vector_3;
                                            Spacing  : in Vector_3;
                                            Mode     : in renderMode) return Vector_3;

   function Quad     (Self : access Item;   for_Character : in Character) return openGL.GlyphImpl.Texture.Quad_t;


   ---------------
   --- 'Protected'
   --

   function MakeGlyphImpl (Self : access Item;   ftGlyph : in freetype_c.FT_GlyphSlot.item) return access Glyph.item'Class;
   --
   --  Create an FTTextureGlyph object for the base class.


   function gl_Texture    (Self : in     Item) return openGL.Texture.texture_Name;



private

   use type openGL.Texture.texture_Name;
   package texture_name_Vectors is new ada.Containers.Vectors (Positive, openGL.Texture.texture_Name);


   type Item is new FontImpl.item with
      record
         maximumGLTextureSize : aliased gl.GLsizei := 0;   -- The max texture dimension on this openGL implemetation.

         textureWidth         :         gl.GLsizei := 0;   -- The min texture width  required to hold the glyphs.
         textureHeight        :         gl.GLsizei := 0;   -- The min texture height required to hold the glyphs.
         textureIDList        :         texture_name_Vectors.Vector;
                                                           -- An array of texture ids.

         glyphHeight          :         Integer    := 0;   -- The max height for glyphs in the current font.
         glyphWidth           :         Integer    := 0;   -- The max width  for glyphs in the current font.

         Padding              :         Natural    := 3;   -- A value to be added to the height and width to ensure that
         numGlyphs            :         Natural;           -- glyphs don't overlap in the texture.
         remGlyphs            :         Natural;

         xOffset, yOffset     :         Integer    := 0;
      end record;


   procedure CalculateTextureSize (Self : in out Item);
   --
   --  Get the size of a block of memory required to layout the glyphs
   --
   --  Calculates a width and height based on the glyph sizes and the
   --  number of glyphs. It over estimates.


   function CreateTexture (Self : access Item) return openGL.Texture.texture_Name;
   --
   --  Creates a 'blank' openGL texture object.
   --
   --  The format is GL_ALPHA and the params are
   --     * GL_TEXTURE_WRAP_S = GL_CLAMP
   --     * GL_TEXTURE_WRAP_T = GL_CLAMP
   --     * GL_TEXTURE_MAG_FILTER = GL_LINEAR
   --     * GL_TEXTURE_MIN_FILTER = GL_LINEAR
   --     * Note that mipmapping is NOT used


   procedure free_Textures (Self : in out Item);

end openGL.FontImpl.Texture;
