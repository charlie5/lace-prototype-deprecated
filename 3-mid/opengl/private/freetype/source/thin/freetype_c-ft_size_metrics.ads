package freetype_c.FT_Size_Metrics
is

   type Item is
      record
         X_ppem      : aliased FT_UShort;
         Y_ppem      : aliased FT_UShort;
         X_Scale     : aliased FT_Fixed;
         Y_Scale     : aliased FT_Fixed;
         Ascender    : aliased FT_Pos;
         Descender   : aliased FT_Pos;
         Height      : aliased FT_Pos;
         max_Advance : aliased FT_Pos;
      end record;

   type Item_array is array (C.Size_t range <>) of aliased FT_Size_Metrics.Item;


   type Pointer       is access all freetype_c.FT_Size_Metrics.Item;
   type Pointer_array is array (C.Size_t range <>) of aliased freetype_c.FT_Size_Metrics.Pointer;

   type pointer_Pointer is access all freetype_c.FT_Size_Metrics.Pointer;

end freetype_c.FT_Size_Metrics;
