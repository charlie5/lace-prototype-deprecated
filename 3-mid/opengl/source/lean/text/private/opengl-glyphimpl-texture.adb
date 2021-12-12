with
     openGL.Tasks,
     openGL.Errors,

     GL.Binding,
     GL.Pointers,

     freetype_c.Binding,
     freetype_c.FT_Bitmap,

     interfaces.C;


package body openGL.GlyphImpl.texture
is
   -----------
   --  Globals
   --

   activeTextureID : openGL.texture.texture_Name;     -- TODO: Check C source for how this is used.
   pragma Unreferenced (activeTextureID);
   --
   --  The texture index of the currently active texture
   --
   --  We keep track of the currently active texture to try to reduce the
   --  number of texture bind operations.

   procedure ResetActiveTexture
   is
   begin
      activeTextureID := 0;
   end ResetActiveTexture;


   ---------
   --  Forge
   --

   function new_GlyphImpl (glyth_Slot       : in freetype_c.FT_GlyphSlot.item;
                           texture_Id       : in openGL.Texture.texture_Name;
                           xOffset, yOffset : in Integer;
                           Width,   Height  : in Integer) return GlyphImpl.texture.view
   is
      use freetype_C,
          freetype_C.Binding,
          GL,
          GL.Binding;

      use type interfaces.C.unsigned,
               GLint;

      Self : constant GlyphImpl.texture.view := new GlyphImpl.texture.item;
   begin
      Tasks.check;

      Self.define (glyth_Slot);

      Self.destWidth   := 0;
      Self.destHeight  := 0;
      Self.glTextureID := texture_Id;
      Self.Err         := FT_Render_Glyph (glyth_Slot,
                                           FT_RENDER_MODE_NORMAL);
      if Self.Err /= no_Error
      then
         raise openGL.Error with "FT_Render_Glyph failed with error code: " & Self.Err'Image;
      end if;

      if FT_GlyphSlot_Get_Format (glyth_Slot) /= get_FT_GLYPH_FORMAT_BITMAP
      then
         raise openGL.Error with "Glyph is not a bitmap format.";
      end if;

      declare
         use GL.Pointers;
         Bitmap : constant freetype_C.FT_Bitmap.item := FT_GlyphSlot_Get_Bitmap (glyth_Slot);
      begin
         Self.destWidth  := Bitmap.Width;
         Self.destHeight := Bitmap.Rows;

         if         Self.destWidth  /= 0
           and then Self.destHeight /= 0
         then
            glPixelStorei   (GL_UNPACK_ALIGNMENT, 1);

            glBindTexture   (GL_TEXTURE_2D, Self.glTextureID);
            Errors.log;

            glTexSubImage2D (GL_TEXTURE_2D,    0,
                             GLint (xOffset),  GLint (yOffset),
                             Self.destWidth,   Self.destHeight,
                             GL_ALPHA,
                             GL_UNSIGNED_BYTE,
                             to_GLvoid_access (Bitmap.Buffer));
            Errors.log;
         end if;
      end;

      --        0
      --        +----+
      --        |    |
      --        |    |
      --        |    |
      --        +----+
      --             1

      Self.UV (1).S := Real (xOffset) / Real (Width);
      Self.UV (1).T := Real (yOffset) / Real (Height);

      Self.UV (2).S := Real (GLint (xOffset) + Self.destWidth)  / Real (Width);
      Self.UV (2).T := Real (GLint (yOffset) + Self.destHeight) / Real (Height);

      Self.Corner   := (Real (FT_GlyphSlot_Get_bitmap_left (glyth_Slot)),
                        Real (FT_GlyphSlot_Get_bitmap_top  (glyth_Slot)),
                        0.0);
      declare
         use openGL.Primitive;
         the_Indices : constant openGL.Indices := (1, 2, 3, 4);
      begin
         Self.Primitive := Primitive.indexed.new_Primitive (triangle_Fan, the_Indices);
      end;

      return Self;
   end new_GlyphImpl;


   --------------
   --  Attributes
   --

   function Quad (Self : in Item;   Pen : in Vector_3) return Quad_t
   is
      dx : constant Real := Real'Floor (Pen (1) + Self.Corner (1));
      dy : constant Real := Real'Floor (Pen (2) + Self.Corner (2));

      the_Quad : aliased constant Quad_t := (NW => (Site   => (dx,
                                                               dy,
                                                               0.0),
                                                    Coords => (S => Self.UV (1).S,
                                                               T => Self.UV (1).T)),

                                             SW => (Site   => (dx,
                                                               dy - Real (Self.destHeight),
                                                               0.0),
                                                    Coords => (S => Self.UV (1).S,
                                                               T => Self.UV (2).T)),

                                             SE => (Site   => (dx + Real (Self.destWidth),
                                                               dy - Real (Self.destHeight),
                                                               0.0),
                                                    Coords => (S => Self.UV (2).S,
                                                               T => Self.UV (2).T)),

                                             NE => (Site   => (dx + Real (Self.destWidth),
                                                               dy,
                                                               0.0),
                                                    Coords => (S => Self.UV (2).S,
                                                               T => Self.UV (1).T)),
                                             Advance => Self.Advance);
   begin
      return the_Quad;
   end Quad;


   --------------
   --  Operations
   --

   function renderImpl (Self : in Item;   Pen        : in Vector_3;
                                          renderMode : in Integer) return Vector_3
   is
      pragma unreferenced (renderMode);
   begin
      return Self.Advance;
   end renderImpl;


end openGL.GlyphImpl.Texture;
