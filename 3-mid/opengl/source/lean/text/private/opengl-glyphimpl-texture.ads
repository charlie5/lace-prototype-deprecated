with
     openGL.Texture,
     freetype_c.FT_GlyphSlot;

private
with
     openGL.Geometry.lit_textured,
     openGL.Primitive.indexed,
     GL;

package openGL.GlyphImpl.texture
--
--  Implements a texture-based glyph.
--
is
   type Item is new GlyphImpl.item with private;
   type View is access all Item'Class;


   ---------
   --  Types
   --

   type Vertex is
      record
         Site   : Vector_3;
         Coords : Coordinate_2D;
      end record;

   type Quad_t is
      record
         NW, NE,
         SW, SE  : Vertex;
         Advance : Vector_3;
      end record;


   ---------
   --  Forge
   --
   function new_GlyphImpl (glyth_Slot       : in freetype_c.FT_GlyphSlot.item;
                           texture_Id       : in openGL.Texture.texture_Name;
                           xOffset, yOffset : in Integer;
                           Width,   Height  : in Integer) return GlyphImpl.texture.view;
   --
   --  glyth_Slot:       The Freetype glyph to be processed.
   --  texture_Id:       The Id of the texture that this glyph will be drawn in.
   --  xOffset, yOffset: The x any y offset into the parent texture to draw this glyph.
   --  Width,   Height:  The width and height (number of rows) of the parent texture.


   --------------
   --  Attributes
   --

   function Quad (Self : in Item;   Pen : in Vector_3) return Quad_t;


   --------------
   --  Operations
   --

   function renderImpl (Self : in Item;   Pen        : in Vector_3;
                                          renderMode : in Integer) return Vector_3;
   --
   --  Pen:         The current pen position.
   --  renderMode:  Render mode to display.
   --
   --  Returns the advance distance for this glyph.


   -------------
   --  Protected - for derived class use only.
   --

   procedure ResetActiveTexture;
   --
   --  Reset the currently active texture to zero to get into a known
   --  state before drawing a string. This is to get around possible threading issues.



private

   type Item is new GlyphImpl.item with
      record
         destWidth,                                             -- The width and height of the glyph 'image'.
         destHeight  :        GL.GLint;

         Corner      :        Vector_3;                         -- Vector from the pen site to the top left of the pixmap.

         UV          :        Coordinates_2D (1 .. 2);          -- The texture co-ords of this glyph within the texture.
         glTextureID :        openGL.texture.texture_Name;      -- The texture index that this glyph is contained in.

         Geometry    : access Geometry.lit_textured.item;
         Primitive   :        openGL.Primitive.indexed.view;
      end record;

end openGL.GlyphImpl.texture;
