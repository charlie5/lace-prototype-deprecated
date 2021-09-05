with
     openGL.Palette,
     openGL.Tasks,

     GL.Binding,
     GL.Pointers,

     freetype_c.Binding,
     freetype_c.FT_Bitmap,

     Interfaces.C;


package body openGL.GlyphImpl.Texture
is

   -----------
   --  Globals
   --

   activeTextureID : openGL.texture.texture_Name;
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
                           texture_id       : in openGL.Texture.texture_Name;
                           xOffset, yOffset : in Integer;
                           width, height    : in Integer) return GlyphImpl.Texture.view
   is
      use freetype_c,
          freetype_c.Binding,
          GL,
          GL.Binding;

      use type interfaces.c.unsigned,
               GLint;

      check_is_OK : constant Boolean                := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      Self        : constant GlyphImpl.Texture.view := new GlyphImpl.Texture.item;
   begin
      Self.define (glyth_Slot);

      Self.destWidth   := 0;
      Self.destHeight  := 0;
      Self.glTextureID := texture_id;
      Self.err         := error_Kind'Val (FT_Render_Glyph (glyth_Slot,
                                                           FT_RENDER_MODE_NORMAL));
      if Self.err /= no_Error
      then
         raise openGL.Error with "FT_Render_Glyph failed with error code: " & error_Kind'Image (Self.err);
      end if;

      if FT_GlyphSlot_Get_Format (glyth_Slot) /= get_FT_GLYPH_FORMAT_BITMAP
      then
         raise openGL.Error with "Glyph is not a bitmap format.";
      end if;

      declare
         use GL.Pointers;
         bitmap : constant freetype_c.FT_Bitmap.item := FT_GlyphSlot_Get_Bitmap (glyth_Slot);
      begin
         Self.destWidth  := bitmap.width;
         Self.destHeight := bitmap.rows;

         if         Self.destWidth  /= 0
           and then Self.destHeight /= 0
         then
            glPixelStorei   (GL_UNPACK_ALIGNMENT, 1);

            glBindTexture   (GL_TEXTURE_2D,    Self.glTextureID);
            glTexSubImage2D (GL_TEXTURE_2D,    0,
                             GLint (xOffset),  GLint (yOffset),
                             Self.destWidth,   Self.destHeight,
                             GL_ALPHA,
                             GL_UNSIGNED_BYTE, to_GLvoid_access (bitmap.buffer));
         end if;
      end;

      --        0
      --        +----+
      --        |    |
      --        |    |
      --        |    |
      --        +----+
      --             1

      Self.uv (1).S := Real (xOffset) / Real (width);
      Self.uv (1).T := Real (yOffset) / Real (height);

      Self.uv (2).S := Real (GLint (xOffset) + Self.destWidth)  / Real (width);
      Self.uv (2).T := Real (GLint (yOffset) + Self.destHeight) / Real (height);

      Self.corner   := (Real (FT_GlyphSlot_Get_bitmap_left (glyth_Slot)),
                        Real (FT_GlyphSlot_Get_bitmap_top  (glyth_Slot)),
                        0.0);
      declare
         use openGL.Primitive;
         the_Indices : constant openGL.Indices := (1, 2, 3, 4);
      begin
         Self.Primitive := Primitive.indexed.new_Primitive (triangle_Fan,  the_Indices);
      end;

      return Self;
   end new_GlyphImpl;



   --------------
   --  Attributes
   --

   function Quad (Self : in Item;   Pen : in Vector_3) return Quad_t
   is
      dx       :         constant Real   := Real'Floor (Pen (1) + Self.corner (1));
      dy       :         constant Real   := Real'Floor (Pen (2) + Self.corner (2));

      the_Quad : aliased constant Quad_t := (NW      => (site   => (dx,
                                                                    dy,
                                                                    0.0),
                                                         coords => (s => Self.uv (1).S,
                                                                    t => Self.uv (1).T)),

                                             SW      => (site   => (dx,
                                                                    dy - Real (Self.destHeight),
                                                                    0.0),
                                                         coords => (s => Self.uv (1).S,
                                                                    t => Self.uv (2).T)),

                                             SE      => (site   => (dx + Real (Self.destWidth),
                                                                    dy - Real (Self.destHeight),
                                                                    0.0),
                                                         coords => (s => Self.uv (2).S,
                                                                    t => Self.uv (2).T)),

                                             NE      => (site   => (dx + Real (Self.destWidth),
                                                                    dy,
                                                                    0.0),
                                                         coords => (s => Self.uv (2).S,
                                                                    t => Self.uv (1).T)),

                                             Advance => Self.advance);
   begin
      return the_Quad;
   end Quad;



   --------------
   --  Operations
   --

   function renderImpl (Self : in Item;   Pen        : in Vector_3;
                                          renderMode : in Integer) return Vector_3
   is
      pragma Unreferenced (renderMode);

      dx : constant Real := Real'Floor (Pen (1) + Self.corner (1));
      dy : constant Real := Real'Floor (Pen (2) + Self.corner (2));

   begin
      declare
         use openGL.Palette;

         the_Vertices : aliased Geometry.lit_colored_textured.Vertex_array
           := (1 => (site   => (dx,
                                dy,
                                0.0),
                     normal => (0.0, 0.0, 1.0),
                     color  => (White, Opaque),
                     coords => (s => Self.uv (1).S,
                                t => Self.uv (1).T)),

               2 => (site   => (dx,
                                dy - Real (Self.destHeight),
                                0.0),
                     normal => (0.0, 0.0, 1.0),
                     color  => (White, Opaque),
                     coords => (s => Self.uv (1).S,
                                t => Self.uv (2).T)),

               3 => (site   => (dx + Real (Self.destWidth),
                                dy - Real (Self.destHeight),
                                0.0),
                     normal => (0.0, 0.0, 1.0),
                     color  => (White, Opaque),
                     coords => (s => Self.uv (2).S,
                                t => Self.uv (2).T)),

               4 => (site   => (dx + Real (Self.destWidth),
                                dy,
                                0.0),
                     normal => (0.0, 0.0, 1.0),
                     color  => (White, Opaque),
                     coords => (s => Self.uv (2).S,
                                t => Self.uv (1).T)));
      begin
         null;
      end;

      return Self.advance;
   end renderImpl;


end openGL.GlyphImpl.Texture;
