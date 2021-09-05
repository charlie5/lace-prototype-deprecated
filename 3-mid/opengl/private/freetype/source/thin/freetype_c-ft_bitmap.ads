package freetype_c.FT_Bitmap
is

   type Item is
      record
         Rows         : aliased c.int;
         Width        : aliased c.int;
         Pitch        : aliased c.int;
         Buffer       : access  c.unsigned_char;
         num_Grays    : aliased c.short;
         pixel_Mode   : aliased c.char;
         palette_Mode : aliased c.char;
         Palette      : aliased System.Address;
      end record;

   type Item_array is array (C.Size_t range <>) of aliased FT_Bitmap.Item;


   type Pointer       is access all FT_Bitmap.Item;
   type Pointer_array is array (C.Size_t range <>) of aliased FT_Bitmap.Pointer;

   type pointer_Pointer is access all freetype_c.FT_Bitmap.Pointer;

end freetype_c.FT_Bitmap;
