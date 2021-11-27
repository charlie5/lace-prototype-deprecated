with
     freetype_C.Binding,
     freetype_C.FT_Vector,
     freetype_C.FT_Bitmap,
     freetype_C.FT_Size_Metrics,
     freetype_C.FT_BBox,
     freetype_C.FT_CharMapRec,
     interfaces.C.Strings;

procedure launch_freetype_linkage_Test
--
--  Tests linkage to Freetype functions.
--  Is not meant to be run.
--
is
   use Freetype_C,
       freetype_C.Binding,
       Interfaces;

   an_Error           :        FT_Error;
   pragma Unreferenced (an_Error);
   an_FT_UShort       :        FT_UShort;
   pragma Unreferenced (an_FT_UShort);
   an_FT_Uint         :        FT_Uint;
   pragma Unreferenced (an_FT_Uint);
   an_FT_Int          :        FT_Int;
   pragma Unreferenced (an_FT_Int);
   an_FT_Long         :        FT_Long;
   pragma Unreferenced (an_FT_Long);
   an_FT_Outline      : access FT_Outline;
   pragma Unreferenced (an_FT_Outline);
   an_FT_Vector       :        FT_Vector.Item;
   pragma Unreferenced (an_FT_Vector);
   an_FT_Bitmap       :        FT_Bitmap.Item;
   pragma Unreferenced (an_FT_Bitmap);
   an_Unsigned        :        interfaces.c.unsigned;
   pragma Unreferenced (an_Unsigned);
   an_FT_Size_Metrics :        FT_Size_Metrics.Item;
   pragma Unreferenced (an_FT_Size_Metrics);
   an_FT_Face         : access freetype_c.FT_FaceRec;
   pragma Unreferenced (an_FT_Face);
   an_FT_SizeRec      : access freetype_c.FT_SizeRec;
   pragma Unreferenced (an_FT_SizeRec);
   an_FT_BBox         :        FT_BBox.item;
   pragma Unreferenced (an_FT_BBox);
   an_FT_CharMap      : access freetype_c.FT_CharMapRec.Item;
   pragma Unreferenced (an_FT_CharMap);
   an_FT_GlyphSlot    : access freetype_c.FT_GlyphSlotRec;
   pragma Unreferenced (an_FT_GlyphSlot);

begin
   FT_Outline_Get_CBox (null, null);

   an_Error      := FT_Init_FreeType  (null);
   an_Error      := FT_Done_FreeType  (null);
   an_Error      := FT_Render_Glyph   (null, FT_RENDER_MODE_NORMAL);
   an_Error      := FT_Set_Char_Size  (null, 0, 0, 0, 0);
   an_Error      := FT_Done_Face      (null);
   an_Error      := FT_Attach_File    (null, Interfaces.C.Strings.null_ptr);
   an_Error      := FT_Set_Charmap    (null, null);
   an_Error      := FT_Select_Charmap (null, 0);

   an_FT_uint    := FT_Get_Char_Index (null, 0);
   an_Error      := FT_Get_Kerning    (null, 0, 0, 0, null);

   an_Error      := FT_Load_Glyph                 (null, 0, 0);
   an_FT_Outline := FT_GlyphSlot_Get_Outline      (null);
   an_FT_Vector  := FT_GlyphSlot_Get_Advance      (null);
   an_FT_Bitmap  := FT_GlyphSlot_Get_Bitmap       (null);
   an_FT_Int     := FT_GlyphSlot_Get_bitmap_left  (null);
   an_FT_Int     := FT_GlyphSlot_Get_bitmap_top   (null);
   an_Unsigned   := FT_GlyphSlot_Get_Format       (null);

   an_FT_Size_Metrics := FT_Size_Get_Metrics      (null);
   an_FT_Face         := new_FT_Face              (null, C.Strings.null_ptr);
   an_FT_Face         := new_FT_Memory_Face       (null, null, 0);
   an_FT_SizeRec      := FT_Face_Get_Size         (null);
   an_FT_Long         := FT_Face_IS_SCALABLE      (null);
   an_FT_Long         := FT_Face_HAS_KERNING      (null);
   an_FT_BBox         := FT_Face_Get_BBox         (null);
   an_FT_UShort       := FT_Face_Get_units_per_EM (null);
   an_FT_Long         := FT_Face_Get_num_glyphs   (null);
   an_FT_CharMap      := FT_Face_Get_charmap      (null);
   an_FT_CharMap      := FT_Face_Get_charmap_at   (null, 0);
   an_FT_Int          := FT_Face_Get_num_charmaps (null);
   an_FT_GlyphSlot    := FT_Face_Get_glyph        (null);
   an_Error           := FT_Face_Attach_Stream    (null, null, 0);

   an_Unsigned := get_FT_GLYPH_FORMAT_NONE;
   an_Unsigned := get_FT_GLYPH_FORMAT_COMPOSITE;
   an_Unsigned := get_FT_GLYPH_FORMAT_BITMAP;
   an_Unsigned := get_FT_GLYPH_FORMAT_OUTLINE;
   an_Unsigned := get_FT_GLYPH_FORMAT_PLOTTER;

   an_Unsigned := FT_ENCODING_NONE_enum;
   an_Unsigned := FT_ENCODING_MS_SYMBOL_enum;
   an_Unsigned := FT_ENCODING_UNICODE_enum;
   an_Unsigned := FT_ENCODING_SJIS_enum;
   an_Unsigned := FT_ENCODING_GB2312_enum;
   an_Unsigned := FT_ENCODING_BIG5_enum;
   an_Unsigned := FT_ENCODING_WANSUNG_enum;
   an_Unsigned := FT_ENCODING_JOHAB_enum;
   an_Unsigned := FT_ENCODING_ADOBE_STANDARD_enum;
   an_Unsigned := FT_ENCODING_ADOBE_EXPERT_enum;
   an_Unsigned := FT_ENCODING_ADOBE_CUSTOM_enum;
   an_Unsigned := FT_ENCODING_ADOBE_LATIN_1_enum;
   an_Unsigned := FT_ENCODING_OLD_LATIN_2_enum;
   an_Unsigned := FT_ENCODING_APPLE_ROMAN_enum;

   an_Unsigned := FT_LOAD_DEFAULT_flag;
   an_Unsigned := FT_LOAD_NO_SCALE_flag;
   an_Unsigned := FT_LOAD_NO_HINTING_flag;
   an_Unsigned := FT_LOAD_RENDER_flag;
   an_Unsigned := FT_LOAD_NO_BITMAP_flag;
   an_Unsigned := FT_LOAD_VERTICAL_LAYOUT_flag;
   an_Unsigned := FT_LOAD_FORCE_AUTOHINT_flag;
   an_Unsigned := FT_LOAD_CROP_BITMAP_flag;
   an_Unsigned := FT_LOAD_PEDANTIC_flag;
   an_Unsigned := FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH_flag;
   an_Unsigned := FT_LOAD_NO_RECURSE_flag;
   an_Unsigned := FT_LOAD_IGNORE_TRANSFORM_flag;
   an_Unsigned := FT_LOAD_MONOCHROME_flag;
   an_Unsigned := FT_LOAD_LINEAR_DESIGN_flag;
   an_Unsigned := FT_LOAD_NO_AUTOHINT_flag;

end launch_freetype_linkage_Test;
