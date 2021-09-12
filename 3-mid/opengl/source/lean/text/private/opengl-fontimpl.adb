with
     openGL.Font,

     freetype_c.Binding,
     freetype_c.FT_GlyphSlot,
     freetype_c.Pointers,
     freetype_c.FT_Size_Metrics,

     ada.unchecked_Deallocation;

package body openGL.FontImpl
is
   use freetype_c.Pointers;

   -----------
   --  Utility
   --

   procedure deallocate is new ada.unchecked_Deallocation (Glyph.Container.item'Class,
                                                           glyph_Container_view);

   ---------
   --  Forge
   --

   procedure define (Self : access Item;   ftFont       : access Font.item'Class;
                                           fontFilePath : in     String)
   is
      use freetype.Face,
          openGL.Glyph.container,
          Freetype_C,
          Freetype_C.Binding;
      use type FT_Error;

   begin
      Self.Face       := Forge.to_Face (fontFilePath, precomputeKerning => True);
      Self.load_Flags := FT_Int (FT_LOAD_DEFAULT_flag);
      Self.Intf       := ftFont;
      Self.Err        := Self.face.Error;

      if Self.Err = 0
      then
         Self.glyphList := new Glyph.Container.item' (to_glyph_Container (Self.Face'Access));
      else
         raise Error with "Unable to create face for font '" & fontFilePath & "'.";
      end if;
   end define;



   procedure define (Self : access Item;   ftFont            : access  Font.item'Class;
                                           pBufferBytes      : access  C.unsigned_char;
                                           bufferSizeInBytes : in      Integer)
   is
      use freetype.Face,
          openGL.Glyph.container,
          Freetype_C,
          Freetype_c.Binding;
      use type FT_Error;
   begin
      Self.Face       := Forge.to_Face (pBufferBytes, bufferSizeInBytes, precomputeKerning => True);
      Self.load_Flags := FT_Int (FT_LOAD_DEFAULT_flag);
      Self.Intf       := ftFont;
      Self.Err        := Self.face.Error;

      if Self.Err = 0
      then
         Self.glyphList := new Glyph.Container.item' (to_glyph_Container (Self.Face'Access));
      end if;
   end define;



   procedure destruct (Self : in out Item)
   is
   begin
      if Self.glyphList /= null
      then
         Self.glyphList.destruct;
         deallocate (Self.glyphList);
      end if;
   end destruct;


   --------------
   --  Attributes
   --

   function Err (Self : in Item) return freetype_c.FT_Error
   is
   begin
      return Self.Err;
   end Err;



   function attach (Self : access Item;   fontFilePath : in String) return Boolean
   is
   begin
      if not Self.Face.attach (fontFilePath)
      then
         Self.Err := Self.Face.Error;
         return False;
      end if;

      Self.Err := 0;
      return True;
   end attach;



   function attach (Self : access Item;   pBufferBytes      : access C.unsigned_char;
                                          bufferSizeInBytes : in     Integer) return Boolean
   is
   begin
      if not Self.Face.attach (pBufferBytes, bufferSizeInBytes)
      then
         Self.Err := Self.Face.Error;
         return False;
      end if;

      Self.Err := 0;
      return True;
   end attach;



   procedure GlyphLoadFlags (Self : in out Item;   Flags : in freetype_c.FT_Int)
   is
   begin
      Self.load_Flags := Flags;
   end GlyphLoadFlags;



   function CharMap (Self : access Item;   Encoding : in freetype_c.FT_Encoding) return Boolean
   is
      Result : constant Boolean := Self.glyphList.CharMap (Encoding);
   begin
      Self.Err := Self.glyphList.Error;
      return Result;
   end CharMap;



   function CharMapCount (Self : in Item) return Natural
   is
   begin
      return Self.Face.CharMapCount;
   end CharMapCount;



   function CharMapList (Self : access Item) return freetype.face.FT_Encodings_view
   is
   begin
      return Self.Face.CharMapList;
   end CharMapList;



   function Ascender (Self : in Item) return Real
   is
   begin
      return Self.charSize.Ascender;
   end Ascender;



   function Descender (Self : in Item) return Real
   is
   begin
      return Self.charSize.Descender;
   end Descender;



   function LineHeight (Self : in Item) return Real
   is
   begin
      return Self.charSize.Height;
   end LineHeight;



   function FaceSize (Self : access Item;   Size         : in Natural;
                                            x_Res, y_Res : in Natural) return Boolean
   is
      use      Glyph.Container;
      use type freetype_c.FT_Error;

   begin
      if Self.glyphList /= null
      then
         Self.glyphList.destruct;
         deallocate (Self.glyphList);
      end if;

      Self.charSize := Self.Face.Size (Size, x_Res, y_Res);
      Self.Err      := Self.Face.Error;

      if Self.Err /= 0 then
         return False;
      end if;

      Self.glyphList := new Glyph.Container.item' (to_glyph_Container (Self.Face'unchecked_Access));
      return True;
   end FaceSize;



   function FaceSize (Self : in Item) return Natural
   is
   begin
      return Self.charSize.CharSize;
   end FaceSize;



   procedure Depth (Self : in out Item;   Depth : in Real)
   is
   begin
      null;   -- NB: This is 'null' in FTGL also.
   end Depth;



   procedure Outset (Self : in out Item;   Outset : in Real)
   is
   begin
      null;   -- NB: This is 'null' in FTGL also.
   end Outset;



   procedure Outset (Self : in out Item;   Front  : in Real;
                                           Back   : in Real)

   is
   begin
      null;   -- NB: This is 'null' in FTGL also.
   end Outset;



   function CheckGlyph (Self : access Item;   Character : in freetype.charmap.CharacterCode) return Boolean
   is
      use type Glyph.Container.Glyph_view,
               freetype_c.FT_Error;

      glyphIndex : freetype.charMap.glyphIndex;
      ftSlot     : freetype_c.FT_GlyphSlot.item;
      tempGlyph  : glyph.Container.Glyph_view;

   begin
      if Self.glyphList.Glyph (Character) /= null
      then
         return True;
      end if;

      glyphIndex := freetype.charMap.glyphIndex (Self.glyphList.FontIndex (Character));
      ftSlot     := Self.Face.Glyph             (glyphIndex,  Self.load_flags);

      if ftSlot = null
      then
         Self.Err := Self.Face.Error;
         return False;
      end if;

      if Self.Intf = null
      then
         raise Error with "CheckGlyph ~ Self.Intf = null";
      end if;

      tempGlyph := Self.Intf.MakeGlyph (ftSlot);

      if tempGlyph = null
      then
         if Self.Err = 0 then
            Self.Err := 16#13#;
         end if;

         return False;
      end if;

      if Self.glyphList.Glyph (character) = null
      then
         Self.glyphList.add (tempGlyph, Character);
      end if;

      return True;
   end CheckGlyph;



   function BBox (Self : access Item;   Text     : in String;
                                        Length   : in Integer;
                                        Position : in Vector_3;
                                        Spacing  : in Vector_3) return Bounds
   is
      pragma unreferenced (Length);

      use freetype.charMap,
          Geometry_3d;

      Pos       : Vector_3 := Position;
      totalBBox : Bounds   := null_Bounds;
   begin
      if Text = ""
      then
         totalBBox.Box := totalBBox.Box or Pos;
         set_Ball_from_Box (totalBBox);

         return totalBBox;
      end if;

      --  Only compute the bounds if string is non-empty.
      --
      if Text'Length > 0     -- TODO: Rid this useless check.
      then
         --  For multibyte, we can't rely on sizeof (T) == character
         --
         declare
            use type freetype.charMap.characterCode;

            thisChar : Character;
            nextChar : Character;

         begin
            --  Expand totalBox by each glyph in string
            --
            for i in Text'Range
            loop
               thisChar := Text (i);

               if i /= Text'Last
               then   nextChar := Text (i + 1);
               else   nextChar := ' ';
               end if;

               if Self.CheckGlyph (to_characterCode (thisChar))
               then
                  declare
                     tempBBox : Bounds := Self.glyphList.BBox (to_characterCode (thisChar));
                  begin
                     tempBBox.Box  := tempBBox.Box + Pos;
                     totalBBox.Box := totalBBox.Box or tempBBox.Box;

                     Pos := Pos  +  spacing;
                     Pos := Pos  +  Vector_3' (Self.glyphList.Advance (to_characterCode (thisChar),
                                                                       to_characterCode (nextChar)),
                                               0.0,
                                               0.0);
                  end;
               end if;
            end loop;
         end;
      end if;

      set_Ball_from_Box (totalBBox);

      return totalBBox;
   end BBox;



   function kern_Advance (Self : in Item;   From, To : in Character) return Real
   is
      use freetype.charMap;
   begin
      return Self.glyphList.Advance (to_characterCode (From),
                                     to_characterCode (To));
   end kern_Advance;



   function x_PPEM (Self : in Item) return Real
   is
      use freetype_c.Binding;

      ft_Size    : constant FT_SizeRec_Pointer              := FT_Face_Get_Size    (Self.Face.freetype_Face);
      ft_Metrics : constant freetype_c.FT_Size_Metrics.item := FT_Size_Get_Metrics (ft_Size);
   begin
      return Real (ft_Metrics.x_PPEM);
   end x_PPEM;



   function x_Scale (Self : in Item) return Real
   is
      use freetype_c.Binding;

      ft_Size    : constant FT_SizeRec_Pointer              := FT_Face_Get_Size    (Self.Face.freetype_Face);
      ft_Metrics : constant freetype_c.FT_Size_Metrics.item := FT_Size_Get_Metrics (ft_Size);
   begin
      return Real (ft_Metrics.x_Scale);
   end x_Scale;



   function y_Scale (Self : in Item) return Real
   is
      use freetype_c.Binding;

      ft_Size    : constant FT_SizeRec_Pointer              := FT_Face_Get_Size    (Self.Face.freetype_Face);
      ft_Metrics : constant freetype_c.FT_Size_Metrics.item := FT_Size_Get_Metrics (ft_Size);
   begin
      return Real (ft_Metrics.y_Scale);
   end y_Scale;



   function Advance (Self : access Item;   Text     : in String;
                                           Length   : in Integer;
                                           Spacing  : in Vector_3)  return Real
   is
      pragma unreferenced (Length);

      Advance : Real    := 0.0;
      ustr    : Integer := 1;
      i       : Integer := 0;

   begin
      while i < Text'Length
      loop
         declare
            use      freetype.charMap;
            use type freetype.charmap.characterCode;

            thisChar : constant Character := Text (ustr);
            nextChar :          Character;

         begin
            ustr := ustr + 1;

            if ustr <= Text'Length
            then   nextChar := Text (ustr);
            else   nextChar := Character'Val (0);
            end if;

            if         nextChar /= Character'Val (0)
              and then Self.CheckGlyph (to_characterCode (thisChar))
            then
               Advance := Advance + Self.glyphList.Advance (to_characterCode (thisChar),
                                                            to_characterCode (nextChar));
            end if;

            if nextChar /= Character'Val (0)
            then
               Advance := Advance + Spacing (1);
            end if;

            i := i + 1;
         end;
      end loop;

      return advance;
   end Advance;


   --------------
   --- Operations
   --

   function render (Self : access Item;   Text       : in String;
                                          Length     : in Integer;
                                          Position   : in Vector_3;
                                          Spacing    : in Vector_3;
                                          renderMode : in Integer) return Vector_3
   is
      use type freetype.charMap.characterCode;

      ustr : Integer  := 1;
      i    : Integer  := 0;
      Pos  : Vector_3 := Position;

   begin
      while     (Length <  0  and then  i <  Text'Length)
        or else (Length >= 0  and then  i <  Length)
      loop
         declare
            use freetype.charMap;

            thisChar : constant Character := Text (ustr);
            nextChar :          Character;

         begin
            ustr := ustr + 1;

            if ustr <= Text'Length
            then   nextChar := Text (ustr);
            else   nextChar := Character'Val (0);
            end if;

            if         nextChar /= Character'Val (0)
              and then Self.CheckGlyph (to_characterCode (thisChar))
            then
               Pos := Pos + Self.glyphList.render (to_characterCode (thisChar),
                                                   to_characterCode (nextChar),
                                                   Position,
                                                   renderMode);
            end if;

            if nextChar /= Character'Val (0)
            then
               Pos := Pos + Spacing;
            end if;

            i := i + 1;
         end;
      end loop;

      return Pos;
   end Render;


end openGL.FontImpl;
