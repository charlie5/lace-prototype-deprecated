with
     freeType_C.Binding,
     freeType_C.Pointers;

package body freetype.face_Size
is
   use freeType_C;


   --------------
   --- Attributes
   --

   function CharSize (Self : access Item;   Face         : in FT_Face.item;
                                            point_Size   : in Natural;
                                            x_Resolution,
                                            y_Resolution : in Natural) return Boolean
   is
      use      freeType_C.Binding;
      use type FT_Error,
               FT_F26Dot6;
   begin
      if        Self.Size        /= point_Size
        or else Self.xResolution /= x_Resolution
        or else Self.yResolution /= y_Resolution
      then
         Self.Err := FT_Set_Char_Size (Face,
                                       0,
                                       FT_F26Dot6 (point_size) * 64,
                                       FT_UInt (Self.xResolution),
                                       FT_UInt (Self.yResolution));
         if Self.Err = 0
         then
            Self.ftFace      := Face;
            Self.Size        := point_Size;
            Self.xResolution := x_Resolution;
            Self.yResolution := y_Resolution;
            Self.ftSize      := FT_Face_Get_Size (Self.ftFace);
         end if;
      end if;

      return Self.Err = 0;
   end CharSize;



   function CharSize (Self : in Item) return Natural
   is
   begin
      return Self.Size;
   end CharSize;



   function Ascender (Self : in Item) return Float
   is
      use freeType_C.Binding,
          freeType_C.Pointers;
   begin
      if Self.ftSize = null
      then   return 0.0;
      else   return Float (FT_Size_Get_Metrics (Self.ftSize).Ascender) / 64.0;
      end if;
   end Ascender;



   function Descender (Self : in Item) return Float
   is
      use freeType_C.Binding,
          freeType_C.Pointers;
   begin
      if Self.ftSize = null
      then   return 0.0;
      else   return Float (FT_Size_Get_Metrics (Self.ftSize).Descender) / 64.0;
      end if;
   end Descender;



   function Height (Self : in Item) return Float
   is
      use      freeType_C.Binding,
               freeType_C.Pointers;
      use type FT_Long;

   begin
      if Self.ftSize = null
      then
         return 0.0;
      end if;

      if FT_Face_IS_SCALABLE (Self.ftFace) /= 0
      then
         return    Float (FT_Face_get_BBox    (Self.ftFace).yMax     -         FT_Face_get_BBox (Self.ftFace).yMin)
                * (Float (FT_Size_get_Metrics (Self.ftSize).y_ppem)  /  Float (FT_Face_get_Units_per_EM (Self.ftFace)));
      else
         return Float (FT_Size_get_Metrics (Self.ftSize).Height) / 64.0;
      end if;
   end Height;



   function Width (Self : in Item) return Float
   is
      use      freeType_C.Binding,
               freeType_C.Pointers;
      use type FT_Long;
   begin
      if Self.ftSize = null
      then
         return 0.0;
      end if;

      if FT_Face_IS_SCALABLE (Self.ftFace) /= 0
      then
         return    Float (FT_Face_get_BBox    (Self.ftFace).xMax     -         FT_Face_get_BBox         (Self.ftFace).xMin)
                * (Float (FT_Size_get_Metrics (Self.ftSize).x_ppem)  /  Float (FT_Face_get_Units_per_EM (Self.ftFace)));
      else
         return Float (FT_Size_get_Metrics (Self.ftSize).max_Advance) / 64.0;
      end if;
   end Width;



   function Underline (Self : in Item) return Float
   is
      pragma unreferenced (Self);
   begin
      return 0.0;
   end Underline;



   function Error (Self : in Item) return FT_Error
   is
   begin
      return Self.Err;
   end Error;


end freetype.face_Size;
