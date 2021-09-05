package freetype_c.FT_CharMapRec
is

   type Item is
      record
         Face        : access  FT_FaceRec;
         Encoding    : aliased FT_Encoding;
         Platform_Id : aliased FT_UShort;
         Encoding_Id : aliased FT_UShort;
      end record;

   type Item_array is array (C.Size_t range <>) of aliased FT_CharMapRec.Item;


   type Pointer       is access all FT_CharMapRec.Item;
   type Pointer_array is array (C.Size_t range <>) of aliased FT_CharMapRec.Pointer;

   type pointer_Pointer is access all FT_CharMapRec.Pointer;

end freetype_c.FT_CharMapRec;
