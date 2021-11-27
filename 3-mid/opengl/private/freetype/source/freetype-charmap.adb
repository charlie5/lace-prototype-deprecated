with
     freetype.Face,
     freeType_C.Binding;

package body freetype.charMap
is
   use freeType_C;

   -----------
   --  Utility
   --

   function to_characterCode (From : in Character) return characterCode
   is
   begin
      return Character'Pos (From) + 1;
   end to_characterCode;


   ---------
   --  Forge
   --

   function to_charMap (parent_Face : access Face.item'Class) return Item
   is
      use freetype_c.Binding;
      use type FT_int;

      Self : Item;

   begin
      Self.ftFace := parent_Face.freetype_Face;
      Self.Err    := 0;

      if FT_Face_Get_charmap (Self.ftFace) = null
      then
         if FT_Face_Get_num_charmaps (Self.ftFace) = 0
         then
            Self.Err := 16#96#;
            return Self;
         end if;

         Self.Err := FT_Set_Charmap (Self.ftFace,
                                     FT_Face_Get_charmap_at (Self.ftFace, 0));
      end if;

      Self.ftEncoding := FT_Face_Get_charmap (Self.ftFace).Encoding;

      for i in characterCode'(1) .. max_Precomputed
      loop
         Self.charIndexCache (i) := FT_Get_Char_Index (Self.ftFace,
                                                       FT_ULong (i - 1));
      end loop;

      return Self;
   end to_charMap;



   procedure destruct (Self : in out Item)
   is
   begin
      Self.charMap.clear;
   end destruct;


   --------------
   --  Attributes
   --

   function Encoding (Self : in Item) return FT_Encoding
   is
   begin
      return Self.ftEncoding;
   end Encoding;



   function CharMap (Self : access Item;   Encoding : in FT_Encoding) return Boolean
   is
      use freeType_C.Binding;
      use type FT_Encoding,
               FT_Error;
   begin
      if Self.ftEncoding = Encoding
      then
         Self.Err := 0;
         return True;
      end if;

      Self.Err := FT_Select_Charmap (Self.ftFace, Encoding);

      if Self.Err = 0
      then
         Self.ftEncoding := Encoding;
         Self.charMap.clear;
      end if;

      return Self.Err = 0;
   end CharMap;



   function GlyphListIndex (Self : in Item;   Character : in CharacterCode) return GlyphIndex
   is
   begin
      return Self.charMap.Element (Character);

   exception
      when Constraint_Error =>
         return -1;
   end GlyphListIndex;



   function FontIndex (Self : in Item;   Character : in characterCode) return GlyphIndex
   is
      use freeType_C.Binding;
   begin
      if Character < max_Precomputed
      then
         return GlyphIndex (Self.charIndexCache (Character));
      end if;

      return GlyphIndex (FT_Get_Char_Index (Self.ftFace,
                                            Character));
   end FontIndex;



   procedure insertIndex (Self : in out Item;   Character      : in characterCode;
                                                containerIndex : in ada.Containers.Count_type)
   is
   begin
      Self.charMap.insert (Character,
                           GlyphIndex (containerIndex));
   end insertIndex;



   function Error (Self : in Item) return FT_Error
   is
   begin
      return Self.Err;
   end Error;


end freetype.charMap;
