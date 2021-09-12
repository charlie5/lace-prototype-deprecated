with
     openGL.FontImpl.texture,
     ada.unchecked_Deallocation;

package body openGL.Font.texture
is
   ---------
   --  Forge
   --

   function to_Font_texture (fontFilePath : in String) return Font.texture.item
   is
   begin
      return Self : Font.texture.item
      do
         Self.define (fontImpl.texture.new_FontImpl_texture (Self'Access,
                                                             fontFilePath));
      end return;
   end to_Font_texture;


   function new_Font_texture (fontFilePath : in String) return Font.texture.view
   is
      Self : constant Font.texture.view := new Font.texture.item;
   begin
      Self.define (fontImpl.Texture.new_FontImpl_texture (Self,
                                                          fontFilePath));
      return Self;
   end new_Font_texture;


   function to_Font_texture (pBufferBytes      : in FontImpl.unsigned_char_Pointer;
                             bufferSizeInBytes : in Natural) return Font.texture.item
   is
   begin
      return Self : Font.texture.item
      do
         Self.define (fontImpl.Texture.new_FontImpl_texture (Self'Access,
                                                             pBufferBytes,
                                                             bufferSizeInBytes));
      end return;
   end to_Font_texture;


   overriding
   procedure destruct (Self : in out Item)
   is
   begin
      destruct (openGL.Font.item (Self));   -- Destroy base class.
   end destruct;


   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      Self.destruct;
      deallocate (Self);
   end free;


   --------------
   --  Attributes
   --

   function gl_Texture (Self : in Item) return openGL.Texture.texture_Name
   is
   begin
      return fontImpl.texture.view (Self.Impl).gl_Texture;
   end gl_Texture;


   function Quad (Self : in Item;   for_Character : in Character) return GlyphImpl.Texture.Quad_t
   is
   begin
      return fontImpl.texture.view (Self.Impl).Quad (for_Character);
   end Quad;


   --------------
   --  Operations
   --

   overriding
   function MakeGlyph (Self : access Item;   Slot : in freetype_c.FT_GlyphSlot.item) return glyph.Container.Glyph_view
   is
      type FontImpl_texture_view is access all FontImpl.texture.Item'Class;

      myimpl : constant FontImpl_texture_view := FontImpl_texture_view (Self.impl);
   begin
      if myimpl = null then
         return null;
      end if;

      return myimpl.MakeGlyphImpl (Slot);
   end MakeGlyph;


end openGL.Font.texture;
