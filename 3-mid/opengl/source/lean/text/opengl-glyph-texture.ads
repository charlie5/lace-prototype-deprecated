with
     openGL.Texture,
     openGL.GlyphImpl.Texture,
     freetype_c.FT_GlyphSlot;

package openGL.Glyph.texture
--
--  A specialisation of Glyph for creating texture glyphs.
--
is
   type Item is new Glyph.item with private;


   -----------
   --   Forge
   --

   function to_Glyph (glyth_Slot       : in freetype_c.FT_GlyphSlot.item;
                      texture_Id       : in openGL.Texture.texture_Name;
                      xOffset, yOffset : in Integer;
                      Width,   Height  : in Integer) return Glyph.texture.item;
   --
   --  glyth_Slot:       The Freetype glyph to be processed.
   --  texture_id:       The id of the texture that this glyph will be drawn in.
   --  xOffset, yOffset: The x and y offset into the parent texture to draw this glyph.
   --  Width, Height:    The width and height (number of rows) of the parent texture.


   function new_Glyph (glyth_Slot       : in freetype_c.FT_GlyphSlot.item;
                       texture_Id       : in openGL.Texture.texture_Name;
                       xOffset, yOffset : in Integer;
                       Width,   Height    : in Integer) return access Glyph.texture.item'Class;
   --
   --  glyth_Slot:       The Freetype glyph to be processed.
   --  texture_Id:       The id of the texture that this glyph will be drawn in.
   --  xOffset, yOffset: The x,y offset into the parent texture to draw this glyph.
   --  Width, Height:    The width and height (number of rows) of the parent texture.


   --------------
   --  Attributes
   --

   function Quad (Self : in Item;   Pen : in Vector_3) return GlyphImpl.texture.Quad_t;


   ---------------
   --  Operations
   --

   overriding
   function render (Self : in Item;   Pen        : in Vector_3;
                                      renderMode : in Integer) return Vector_3;
   --
   --  Render this glyph at the current pen position.
   --
   --  Pen:        The current pen position.
   --  renderMode: Render mode to display.
   --
   --  Returns the advance distance for this glyph.



private

   type Item is new Glyph.item with null record;

end openGL.Glyph.texture;
