with
     ada.unchecked_Deallocation;

package body openGL.Glyph.Container
is
   ---------
   --- Forge
   --

   function to_glyph_Container (parent_Face : in freetype.Face.view) return openGL.glyph.Container.item
   is
      Self : openGL.glyph.Container.item;
   begin
      Self.Face    := parent_Face;
      Self.Err     := 0;
      Self.charMap := new freetype.charMap.Item' (freetype.charMap.to_charMap (Self.Face));

      return Self;
   end to_glyph_Container;



   procedure destruct (Self : in out Item)
   is
      use Glyph_Vectors;

      procedure deallocate is new ada.unchecked_Deallocation (openGL.Glyph.item'Class,     Glyph_view);
      procedure deallocate is new ada.unchecked_Deallocation (freetype.charMap.item'Class, charMap_view);

      Cursor    : Glyph_Vectors.Cursor := Self.Glyphs.First;
      the_Glyph : Glyph_view;

   begin
      while has_Element (Cursor)
      loop
         the_Glyph := Element (Cursor);
         deallocate (the_Glyph);

         next (Cursor);
      end loop;

      Self.Glyphs .clear;
      Self.charMap.destruct;

      deallocate (Self.charMap);
   end destruct;


   --------------
   --  Attributes
   --

   function CharMap (Self : access Item;   Encoding : in freeType_c.FT_Encoding) return Boolean
   is
      Result : constant Boolean := Self.charMap.CharMap (Encoding);
   begin
      Self.Err := Self.charMap.Error;
      return Result;
   end CharMap;



   function FontIndex (Self : in Item;   Character : in freetype.charMap.characterCode) return Natural
   is
   begin
      return Natural (Self.charMap.FontIndex (Character));
   end FontIndex;



   procedure add (Self : in out Item;   Glyph     : in Glyph_view;
                                        Character : in freetype.charMap.characterCode)
   is
   begin
      Self.glyphs.append (Glyph);
      Self.charMap.insertIndex (Character, Self.Glyphs.Length);
   end add;



   function Glyph (Self : in Item;   Character : in freetype.charMap.characterCode) return Glyph_view
   is
      use type freetype.charMap.glyphIndex;
      Index : constant freetype.charMap.glyphIndex := Self.charMap.GlyphListIndex (Character);
   begin
      if Index = -1
      then   return null;
      else   return Self.Glyphs.Element (Integer (Index));
      end if;
   end Glyph;



   function BBox (Self : in Item;   Character : in freetype.charMap.characterCode) return Bounds
   is
   begin
      return Self.Glyph (Character).BBox;
   end BBox;



   function Advance (Self : in Item;   Character         : in freetype.charMap.characterCode;
                                       nextCharacterCode : in freetype.charMap.characterCode) return Real
   is
      Left  : constant freetype.charMap.glyphIndex := Self.charMap.FontIndex (Character);
      Right : constant freetype.charMap.glyphIndex := Self.charMap.FontIndex (nextCharacterCode);
   begin
      return Real (Self.Face.KernAdvance (Integer (Left),
                                          Integer (Right)) (1)  +  Float (Self.Glyph (Character).Advance));
   end Advance;



   function Error (Self : in Item) return freetype_c.FT_Error
   is
   begin
      return Self.Err;
   end Error;


   --------------
   --  Operations
   --

   function render (Self : access Item;   Character         : in freetype.charMap.characterCode;
                                          nextCharacterCode : in freetype.charMap.characterCode;
                                          penPosition       : in Vector_3;
                                          renderMode        : in Integer) return Vector_3
   is
      use type freetype_c.FT_Error,
               freetype.charMap.glyphIndex;

      Left  : constant freetype.charMap.glyphIndex := Self.charMap.FontIndex (Character)         - 0;
      Right : constant freetype.charMap.glyphIndex := Self.charMap.FontIndex (nextCharacterCode) - 0;

      ft_kernAdvance : constant freetype.Vector_3  := Self.Face.KernAdvance (Integer (Left),
                                                                             Integer (Right));
      kernAdvance : Vector_3 := (ft_kernAdvance (1),
                                 ft_kernAdvance (2),
                                 ft_kernAdvance (3));
      Index : freetype.charMap.glyphIndex;

   begin
      if Self.Face.Error = 0
      then
         Index       := Self.charMap.GlyphListIndex (Character);
         kernAdvance := kernAdvance + Self.Glyphs.Element (Integer (Index)).Render (penPosition,
                                                                                    renderMode);
      else
         raise openGL.Error with "Unable to render character '" & Character'Image & "'";
      end if;

      return kernAdvance;
   end Render;


end openGL.Glyph.Container;
