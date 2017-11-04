with
     openGL.Glyph.texture,
     openGL.Glyph.Container,
     openGL.Palette,
     openGL.Tasks,

     GL.lean,
     GL.Pointers,

     freetype_c.Binding,
     ada.unchecked_Conversion;


package body openGL.FontImpl.Texture
is

   ---------
   --  Forge
   --

   function to_FontImpl_texture (ftFont       : access openGL.Font.Item'Class;
                                 fontFilePath : in     String) return fontImpl.texture.item
   is
      use freetype_c.Binding;
      Success : Boolean;
   begin
      return Self : fontImpl.texture.item
      do
         define (Self'Access, ftFont, fontFilePath);

         Self.load_Flags := freetype_c.FT_Int (FT_LOAD_NO_HINTING_flag or FT_LOAD_NO_BITMAP_flag);
         Self.numGlyphs  := Self.face.GlyphCount;
         Self.remGlyphs  := Self.numGlyphs;

         Success := Self.FaceSize (20);

         if not Success then
            raise openGL.Error with "unable to set font facesize";
         end if;
      end return;
   end to_FontImpl_texture;



   function new_FontImpl_texture (ftFont       : access openGL.Font.Item'Class;
                                  fontFilePath : in     String) return access fontImpl.texture.item'Class
   is
      use freetype_c.Binding;

      Self    : constant fontImpl.texture.view := new fontImpl.texture.item;
      Success :          Boolean;
   begin
      define (Self, ftFont, fontFilePath);

      Self.load_Flags := freetype_c.FT_Int (FT_LOAD_NO_HINTING_flag or FT_LOAD_NO_BITMAP_flag);
      Self.numGlyphs  := Self.face.GlyphCount;
      Self.remGlyphs  := Self.numGlyphs;

      Success := Self.FaceSize (20);

      if not Success then
         raise openGL.Error with "unable to set font facesize";
      end if;

      return Self;
   end new_FontImpl_texture;



   function to_FontImpl_texture (ftFont            : access openGL.Font.Item'Class;
                                 pBufferBytes      : in     unsigned_char_Pointer;
                                 bufferSizeInBytes : in     Natural) return fontImpl.texture.item
   is
      use freetype_c.Binding;
   begin
      return Self : fontImpl.texture.item
      do
         define (Self,  ftFont, pBufferBytes, bufferSizeInBytes);

         Self.load_Flags := freetype_c.FT_Int (   FT_LOAD_NO_HINTING_flag
                                               or FT_LOAD_NO_BITMAP_flag);
         Self.numGlyphs  := Self.face.GlyphCount;
         Self.remGlyphs  := Self.numGlyphs;
      end return;
   end to_FontImpl_texture;



   function new_FontImpl_texture (ftFont            : access openGL.Font.Item'Class;
                                  pBufferBytes      : in     unsigned_char_Pointer;
                                  bufferSizeInBytes : in     Natural) return access fontImpl.texture.item'Class
   is
   begin
      return new fontImpl.texture.item' (to_FontImpl_texture (ftFont, pBufferBytes, bufferSizeInBytes));
   end new_FontImpl_texture;



   procedure free_Textures (Self : in out Item)
   is
      use texture_name_Vectors,
          GL.lean;

      check_is_OK : constant Boolean                     := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      Cursor      :          texture_name_Vectors.Cursor := Self.textureIDList.First;
      the_Name    : aliased  openGL.Texture.texture_Name;
   begin
      while has_Element (Cursor)
      loop
         the_Name := Element (Cursor);
         glDeleteTextures (1, the_Name'Unchecked_Access);

         next (Cursor);
      end loop;
   end free_Textures;



   overriding
   procedure destruct (Self : in out Item)
   is
      use      GL.lean;
      use type ada.Containers.Count_Type;
   begin
      destruct (FontImpl.item (Self));   -- Destroy base class.

      if Self.textureIDList.Length > 0
      then
         Self.free_Textures;
      end if;
   end destruct;




   --------------
   --  Attributes
   --

   overriding
   function FaceSize (Self : access Item;   size         : in Natural;
                                            x_res, y_res : in Natural := 72) return Boolean
   is
      type access_FontImpl is access all FontImpl.item;
      Success : Boolean;
   begin
      if not Self.textureIDList.is_empty
      then
         Self.free_Textures;
         Self.textureIDList.clear;

         Self.numGlyphs := Self.face.GlyphCount;
         Self.remGlyphs := Self.numGlyphs;
      end if;

      Success := access_FontImpl (Self).FaceSize (size,  x_res, y_res);
      return Success;
   end FaceSize;



   function Render (Self : access Item;   s        : in String;
                                          len      : in Integer;
                                          position : in Vector_3;
                                          spacing  : in Vector_3;
                                          Mode     : in renderMode) return Vector_3
   is
      use GL, GL.lean;
      function to_Integer is new ada.Unchecked_Conversion (fontImpl.RenderMode, Integer);

      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      tmp         :          Vector_3;

   begin
      glEnable    (GL_BLEND);
      glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable    (GL_TEXTURE_2D);

      GlyphImpl.texture.ResetActiveTexture;

      tmp := FontImpl.item (Self.all).Render (s,        len,
                                              position, spacing,
                                              to_Integer (Mode));
      return tmp;
   end Render;



   function MakeGlyphImpl (Self : access Item;   ftGlyph : in freetype_c.FT_GlyphSlot.item) return access Glyph.Item'Class
   is
      use type openGL.Real;

      tempGlyph : glyph.Container.Glyph_view;

   begin
      Self.glyphHeight := Integer (Self.charSize.Height + 0.5);
      Self.glyphWidth  := Integer (Self.charSize.Width  + 0.5);

      if Self.glyphHeight < 1 then  Self.glyphHeight := 1;   end if;
      if Self.glyphWidth  < 1 then  Self.glyphWidth  := 1;   end if;

      if Self.textureIDList.is_empty
      then
         Self.textureIDList.append (Self.CreateTexture);
         Self.xOffset := Self.padding;
         Self.yOffset := Self.padding;
      end if;

      if Self.xOffset > (Integer (Self.textureWidth) - Self.glyphWidth)
      then
         Self.xOffset := Self.padding;
         Self.yOffset := Self.yOffset + Self.glyphHeight;

         if Self.yOffset > (Integer (Self.textureHeight) - Self.glyphHeight)
         then
            Self.textureIDList.append (Self.CreateTexture);
            Self.yOffset := Self.padding;
         end if;
      end if;

      tempGlyph      := openGL.Glyph.texture.new_Glyph (ftGlyph,
                                                        Self.textureIDList.Last_Element,
                                                        Self.xOffset,
                                                        Self.yOffset,
                                                        Integer (Self.textureWidth),
                                                        Integer (Self.textureHeight)).all'Access;

      Self.xOffset   := Self.xOffset + Integer (  tempGlyph.BBox.Box.Upper (1)
                                                - tempGlyph.BBox.Box.Lower (1)
                                                + Real (Self.padding)
                                                + 0.5);
      Self.remGlyphs := Self.remGlyphs - 1;

      return tempGlyph;
   end MakeGlyphImpl;



   function Quad (Self : access Item;   for_Character : in Character) return openGL.GlyphImpl.Texture.Quad_t
   is
      use freetype.charMap;

      Success :constant Boolean := Self.CheckGlyph (to_characterCode (for_Character));
      pragma Unreferenced (Success);

      the_Glyph : constant openGL.Glyph.Container.Glyph_view := Self.glyphList.Glyph (to_characterCode (for_Character));
   begin
      return openGL.Glyph.Texture.item (the_Glyph.all).Quad ((0.0, 0.0, 0.0));
   end Quad;



   procedure  CalculateTextureSize (Self : in out Item)
   is
      use      openGL.Texture,
               GL,   GL.lean;
      use type Real, GL.GLsizei;

      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      h           :          Integer;

   begin
      if Self.maximumGLTextureSize = 0
      then
         Self.maximumGLTextureSize := 1024;
         glGetIntegerv (GL_MAX_TEXTURE_SIZE, Self.maximumGLTextureSize'Unchecked_Access);

         pragma assert (Self.maximumGLTextureSize /= 0);   -- If you hit this then you have an invalid openGL context.
      end if;

      begin
         Self.textureWidth := power_of_2_Ceiling (  (Self.remGlyphs * Self.glyphWidth)
                                                  + (Self.padding * 2));
      exception
         when Constraint_Error =>
            Self.textureWidth := Self.maximumGLTextureSize;
      end;

      if     Self.textureWidth >  Self.maximumGLTextureSize
      then   Self.textureWidth := Self.maximumGLTextureSize;
      else   null;   -- Self.textureWidth := Self.textureWidth;
      end if;

      h                  := Integer (   Real (Integer (Self.textureWidth) - (Self.padding * 2))
                                     /  Real (Self.glyphWidth)
                                     +  0.5);
      Self.textureHeight := Power_of_2_Ceiling (  ((Self.numGlyphs / h) + 1)
                                                * Self.glyphHeight);

      if     Self.textureHeight >  Self.maximumGLTextureSize
      then   Self.textureHeight := Self.maximumGLTextureSize;
      else   null;   -- Self.textureHeight := Self.textureHeight;
      end if;
   end CalculateTextureSize;



   function CreateTexture (Self : access Item) return openGL.Texture.texture_Name
   is
      use openGL.Palette,
          GL, GL.lean;

      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);

   begin
      Self.CalculateTextureSize;

      declare
         use openGL.Texture, GL.Pointers;

         the_Image :         openGL.Image (1 .. Index_t (self.textureHeight),
                                           1 .. Index_t (Self.textureWidth)) := (others => (others => openGL.Palette.Black));

         textID    : aliased openGL.Texture.texture_Name;
      begin
         glGenTextures   (1, textID'Unchecked_Access);

         glBindTexture   (GL_TEXTURE_2D,  textID);
         glTexParameteri (GL_TEXTURE_2D,  GL_TEXTURE_WRAP_S,     GL_CLAMP_TO_EDGE);
         glTexParameteri (GL_TEXTURE_2D,  GL_TEXTURE_WRAP_T,     GL_CLAMP_TO_EDGE);
         glTexParameteri (GL_TEXTURE_2D,  GL_TEXTURE_MAG_FILTER, GL_LINEAR);
         glTexParameteri (GL_TEXTURE_2D,  GL_TEXTURE_MIN_FILTER, GL_LINEAR);

         glTexImage2D    (GL_TEXTURE_2D,
                          0,                 GL_ALPHA,
                          Self.textureWidth, Self.textureHeight,
                          0,                 GL_ALPHA,
                          GL_UNSIGNED_BYTE,
                          to_GLvoid_access (the_Image (1, 1).Red'Access));
         return textID;
      end;
   end CreateTexture;



   function gl_Texture (Self : in Item) return openGL.Texture.texture_Name
   is
   begin
      return Self.textureIDList.last_Element;
   end gl_Texture;


end openGL.FontImpl.Texture;
