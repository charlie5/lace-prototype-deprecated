with
     freeType_C.Binding,
     freeType_C.FT_Library,
     freeType_C.FT_Vector,
     freeType_C.Pointers,

     interfaces.C.Strings,

     ada.unchecked_Conversion,
     ada.unchecked_Deallocation,
     ada.Finalization;

package body freetype.Face
is
   -----------
   --- Globals
   --

   the_FT_Library : aliased FT_Library.item;


   -----------
   --- Utility
   --

   function  to_Flag    is new ada.unchecked_Conversion   (FT_Kerning_Mode, C.unsigned);
   procedure deallocate is new ada.Unchecked_Deallocation (float_Array,     float_Array_view);


   ---------
   --- Forge
   --

   package body Forge
   is
      function to_Face (fontFilePath      : in String;
                        precomputeKerning : in Boolean) return Face.item
      is
         use freeType_C.Binding,
             freeType_C.Pointers,
             C.Strings;
         use type freeType_C.FT_Long;

         Self          : Item;
         the_font_Path : chars_ptr := new_String (fontFilePath);

      begin
         Self.numGlyphs := 0;
         Self.Err       := 0;

         Self.ftFace := new_FT_Face (the_FT_Library, the_font_Path);

         free (the_font_Path);

         if Self.ftFace = null
         then
            raise freetype.Error with "Failed to create a freeType face for '" & fontFilePath & "'.";
         end if;

         Self.numGlyphs       := Integer (FT_Face_Get_num_glyphs (Self.ftFace));
         Self.hasKerningTable := FT_Face_HAS_KERNING (Self.ftFace) /= 0;

         if    Self.hasKerningTable
           and precomputeKerning
         then
            Self.BuildKerningCache;
         end if;

         return Self;
      end to_Face;



      function to_Face (pBufferBytes      : access C.unsigned_char;
                        bufferSizeInBytes : in     Positive;
                        precomputeKerning : in     Boolean) return Face.item
      is
         use freeType_C.Binding,
             freeType_C.Pointers;
         use type FT_Long;

         Self : Face.item;

      begin
         Self.numGlyphs := 0;
         Self.Err       := 0;

         Self.ftFace := new_FT_Memory_Face (the_FT_Library,
                                            pBufferBytes.all'Access,
                                            C.int (bufferSizeInBytes));
         if Self.ftFace = null
         then
            raise freetype.Error with "Failed to create a freeType memory face.";
         end if;

         Self.numGlyphs       := Integer (FT_Face_Get_num_glyphs (Self.ftFace));
         Self.hasKerningTable := FT_Face_HAS_KERNING (Self.ftFace) /= 0;

         if    Self.hasKerningTable
           and precomputeKerning
         then
            Self.BuildKerningCache;
         end if;

         return Self;
      end to_Face;



      procedure destruct (Self : in out Item)
      is
         use freeType_C.Binding;
         use type Pointers.FT_FaceRec_Pointer;
      begin
         if Self.kerningCache /= null
         then
            deallocate (Self.kerningCache);
         end if;

         if Self.ftFace /= null
         then
            Self.Err    := FT_Done_Face (Self.ftFace);
            Self.ftFace := null;
         end if;
      end destruct;

   end Forge;



   function attach (Self : access Item;   fontFilePath : in String) return Boolean
   is
      use freeType_C.Binding,
          C.Strings;
      use type FT_Error;

      the_font_Path : chars_ptr := new_String (fontFilePath);
   begin
      Self.Err := FT_Attach_File (Self.ftFace, the_font_Path);
      free (the_font_Path);

      return Self.Err = 0;
   end attach;



   function attach (Self : access Item;   pBufferBytes      : access C.unsigned_char;
                                          bufferSizeInBytes : in     Positive) return Boolean
   is
      use freeType_C.Binding;
      use type FT_Error;
   begin
      Self.Err := FT_Face_Attach_Stream (Self.ftFace,
                                         pBufferBytes.all'Access,
                                         C.size_t (bufferSizeInBytes));
      return Self.Err = 0;
   end Attach;



   function freetype_Face (Self : in Item) return FT_Face.item
   is
   begin
      return Self.ftFace;
   end freetype_Face;



   function Size (Self : access Item;   Size         : in Natural;
                                        x_Res, y_Res : in Natural) return freetype.face_Size.item
   is
      Success : Boolean;
      pragma unreferenced (Success);
   begin
      Success  := Self.charSize.CharSize (Self.ftFace,
                                          Size,
                                          x_Res, y_Res);
      Self.Err := Self.charSize.Error;

      return Self.charSize;
   end Size;



   function CharMapCount (Self : in Item) return Natural
   is
      use freeType_C.Binding;
   begin
      return Natural (FT_Face_Get_num_charmaps (Self.ftFace));
   end CharMapCount;



   function CharMapList (Self : access Item) return FT_Encodings_view
   is
      use freeType_C.Binding;
   begin
      if Self.fontEncodingList = null
      then
         Self.fontEncodingList := new FT_Encodings (1 .. Self.CharMapCount);

         for i in 1 .. Self.CharMapCount
         loop
            Self.fontEncodingList (i) := FT_Face_Get_charmap_at (Self.ftFace,
                                                                 C.int (i)  ).Encoding;
         end loop;
      end if;

      return Self.fontEncodingList;
   end CharMapList;



   function KernAdvance (Self : access Item;   Index1 : in Natural;
                                               Index2 : in Natural) return Vector_3
   is
      use freeType_C.Binding;
      use type FT_Error;

      X, Y        :         Float;
      kernAdvance : aliased FT_Vector.item;

   begin
      if   not Self.hasKerningTable
        or Index1 = 0
        or Index2 = 0
      then
         return (0.0, 0.0, 0.0);
      end if;

      if    Self.kerningCache /= null
        and Index1 < max_Precomputed     -- TODO: Check this whole function matches C code.
        and Index2 < max_Precomputed
      then
         declare
            max_Index : C.ptrdiff_t := C.ptrdiff_t (2 * (Index2 * max_Precomputed + Index1) + 1);     -- TODO: Check this against C code.
         begin
            X := Float (Self.kerningCache (C.size_t (2 * (Index2 * max_Precomputed + Index1))));
            Y := Float (Self.kerningCache (C.size_t (2 * (Index2 * max_Precomputed + Index1) + 1)));

            return (X, Y, 0.0);
         end;
      end if;

      kernAdvance.X := 0;
      kernAdvance.Y := 0;

      Self.Err := FT_Get_Kerning (Self.ftFace,
                                  C.unsigned (index1),
                                  C.unsigned (index2),
                                  to_Flag    (ft_Kerning_unfitted),
                                  kernAdvance'unchecked_Access);
      if Self.Err /= 0
      then
         return (0.0, 0.0, 0.0);
      end if;

      X := Float (kernAdvance.x) / 64.0;
      Y := Float (kernAdvance.y) / 64.0;

      return (X, Y, 0.0);
   end KernAdvance;



   function GlyphCount (Self : in Item) return Natural
   is
   begin
      return Self.numGlyphs;
   end GlyphCount;



   function Glyph (Self : access Item;   Index      : in freetype.charMap.glyphIndex;
                                         load_Flags : in freeType_C.FT_Int) return FT_GlyphSlot.item
   is
      use freeType_C.Binding;
      use type FT_Error,
               FT_Face.item;
   begin
      if Self.ftFace = null
      then
         raise freetype.Error with "Face is null.";
      end if;

      Self.Err := FT_Load_Glyph (Self.ftFace,  FT_UInt (Index),  load_Flags);

      if Self.Err /= 0 then
         return null;
      end if;

      return FT_GlyphSlot.item (FT_Face_Get_glyph (Self.ftFace));
   end Glyph;



   function Error (Self : in Item) return FT_Error
   is
   begin
      return Self.Err;
   end Error;



   procedure BuildKerningCache (Self : in out Item)
   is
      use freeType_C.Binding;
      use type FT_UInt,
               FT_Error,
               C.C_float;

      max_Index   : constant C.ptrdiff_t   := C.ptrdiff_t (max_Precomputed * max_Precomputed * 2);
      kernAdvance : aliased  FT_Vector.item;

   begin
      kernAdvance.x     := 0;
      kernAdvance.y     := 0;
      Self.kerningCache := new float_Array' (1 .. C.size_t (max_Index) => <>);

      for j in 1 .. FT_UInt' (max_Precomputed)
      loop
         for i in 1 .. FT_UInt' (max_Precomputed)
         loop
            Self.Err := FT_Get_Kerning (Self.ftFace,
                                        i, j,
                                        to_Flag (ft_Kerning_unfitted),
                                        kernAdvance'unchecked_Access);
            if Self.Err /= 0
            then
               deallocate (Self.kerningCache);
               return;
            end if;

            Self.kerningCache (C.size_t (2 * (j * max_Precomputed + i)    )) := C.C_float (kernAdvance.X) / 64.0;
            Self.kerningCache (C.size_t (2 * (j * max_Precomputed + i) + 1)) := C.C_float (kernAdvance.Y) / 64.0;
         end loop;
      end loop;

   end BuildKerningCache;



   -------------------
   --  Package Closure
   --
   type Closure is new ada.Finalization.controlled with null record;

   overriding
   procedure finalize (Object : in out Closure)
   is
      use freeType_C.Binding;
      Status : FT_Error with unreferenced;
   begin
      Status := FT_Done_FreeType (the_FT_Library);
   end finalize;

   the_Closure : Closure with Unreferenced;



   --------------------------
   --  Package Initialisation
   --
   use freeType_C.Binding;
   Status : FT_Error with unreferenced;

begin
   Status := FT_init_FreeType (the_FT_Library'Access);
end freetype.Face;
