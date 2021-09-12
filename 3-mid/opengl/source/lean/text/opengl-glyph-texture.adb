package body openGL.Glyph.texture
is

   ---------
   --  Forge
   --

   function to_Glyph (glyth_Slot       : in freetype_c.FT_GlyphSlot.item;
                      texture_Id       : in openGL.Texture.texture_Name;
                      xOffset, yOffset : in Integer;
                      Width,   Height  : in Integer) return Glyph.texture.item
   is
      Self :          Glyph    .texture.item;
      Impl : constant GlyphImpl.texture.view := GlyphImpl.texture.new_GlyphImpl (glyth_Slot,
                                                                                 texture_Id,
                                                                                 xOffset, yOffset,
                                                                                 Width,   Height);
   begin
      Self.define (Impl.all'Access);
      return Self;
   end to_Glyph;


   function new_Glyph (glyth_Slot       : in freetype_c.FT_GlyphSlot.item;
                       texture_Id       : in openGL.Texture.texture_Name;
                       xOffset, yOffset : in Integer;
                       Width,   Height  : in Integer) return access Glyph.texture.item'Class
   is
   begin
      return new Glyph.texture.item' (to_Glyph (glyth_Slot,
                                                texture_Id,
                                                xOffset, yOffset,
                                                Width,   Height));
   end new_Glyph;


   --------------
   --  Attributes
   --

   function Quad (Self : in Item;   Pen : in Vector_3) return GlyphImpl.texture.Quad_t
   is
   begin
      return GlyphImpl.texture.view (Self.Impl).Quad (Pen);
   end Quad;


   --------------
   --  Operations
   --

   overriding function render (Self : in Item;   Pen        : in Vector_3;
                                                 renderMode : in Integer) return Vector_3
   is
   begin
      return GlyphImpl.texture.view (Self.Impl).renderImpl (Pen, renderMode);
   end render;


end openGL.Glyph.texture;
