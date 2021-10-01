with
     ada.unchecked_Deallocation,
     ada.unchecked_Conversion;

package body openGL.Font
is
   -----------
   --  Utility
   --

   function Hash (the_Id : in font_Id) return ada.Containers.Hash_type
   is
      use ada.Containers;
   begin
      return Hash (the_Id.Name) + Hash_type (the_Id.Size);
   end Hash;


   ---------
   --  Forge
   --

   procedure define (Self : in out Item;   fontFilePath : in String)
   is
   begin
      Self.Impl := new FontImpl.item;
      Self.Impl.define (Self'Access, fontFilePath);
   end define;


   procedure define (Self : in out Item;   pBufferBytes      : in FontImpl.unsigned_char_Pointer;
                                           bufferSizeInBytes : in Natural)
   is
   begin
      Self.Impl := new FontImpl.item;
      Self.Impl.define (Self'Access, pBufferBytes, bufferSizeInBytes);
   end define;


   procedure define (Self : in out Item;   pImpl : in FontImpl.view)
   is
   begin
      Self.Impl := pImpl;
   end define;


   procedure destruct (Self : in out Item)
   is
      procedure free is new ada.unchecked_Deallocation (FontImpl.item'Class,
                                                        FontImpl.view);
   begin
      Self.Impl.destruct;
      free (Self.Impl);
   end destruct;


   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      Self.destruct;
      deallocate (Self);
   end free;


   --------------
   --  Attributes
   --

   function CharMap (Self : in Item;   Encoding : in freetype_c.FT_Encoding) return Boolean
   is
   begin
      return Self.impl.CharMap (Encoding);
   end CharMap;


   function CharMapCount (Self : in Item) return Natural
   is
   begin
      return Self.impl.CharMapCount;
   end CharMapCount;


   function CharMapList (Self : access Item) return freetype.face.FT_Encodings_view
   is
   begin
      return Self.impl.CharMapList;
   end CharMapList;


   function Ascender (Self : in Item) return Real
   is
   begin
      return Self.impl.Ascender;
   end Ascender;


   function Descender (Self : in Item) return Real
   is
   begin
      return Self.impl.Descender;
   end Descender;


   function LineHeight (Self : in Item) return Real
   is
   begin
      return Self.impl.LineHeight;
   end LineHeight;


   function FaceSize (Self : access Item;   Size          : in Natural;
                                            x_Res, y_Res  : in Natural) return Boolean
   is
   begin
      return Self.impl.FaceSize (Size, x_Res, y_Res);
   end FaceSize;


   function FaceSize (Self : in Item) return Natural
   is
   begin
      return Self.impl.FaceSize;
   end FaceSize;


   procedure Depth (Self : in out Item;   Depth : in Real)
   is
   begin
      Self.impl.Depth (Depth);
   end Depth;


   procedure Outset (Self : in out Item;   Outset : in Real)
   is
   begin
      Self.impl.Outset (Outset);
   end Outset;


   procedure Outset (Self : in out Item;   Front : in Real;
                                           Back  : in Real)
   is
   begin
      Self.impl.Outset (Front, Back);
   end Outset;


   function BBox (Self : access Item;   Text     : in String;
                                        Length   : in Integer  := -1;
                                        Position : in Vector_3 := Origin_3D;
                                        Spacing  : in Vector_3 := Origin_3D) return Bounds
   is
   begin
      return Self.impl.BBox (Text, Length, Position, Spacing);
   end BBox;


   function Error (Self : in Item) return freetype_c.FT_Error
   is
   begin
      return Self.impl.Err;
   end Error;


   --------------
   --  Operations
   --

   function attach (Self : in Item;   Font_File_Path : in String) return Boolean
   is
   begin
      return Self.impl.attach (Font_File_Path);
   end Attach;


   function attach (Self : in Item;   pBufferBytes      : in FontImpl.unsigned_char_Pointer;
                                      bufferSizeInBytes : in Natural) return Boolean
   is
   begin
      return Self.impl.Attach (pBufferBytes, bufferSizeInBytes);
   end Attach;


   procedure glyph_load_Flags (Self : in out Item;   Flags : in freetype_c.FT_Int)
   is
   begin
      Self.impl.GlyphLoadFlags (Flags);
   end glyph_load_Flags;


   function Advance (Self : access Item;   Text    : in String;
                                           Length  : in Integer  := -1;
                                           Spacing : in Vector_3 := Origin_3D) return Real
   is
   begin
      return Self.impl.Advance (Text, Length, Spacing);
   end Advance;


   function kern_Advance (Self : in Item;   From, To : in Character) return Real
   is
   begin
      return Self.impl.kern_Advance (From, To);
   end kern_Advance;


   function x_PPEM (Self : in Item) return Real
   is
   begin
      return Self.impl.x_PPEM;
   end x_PPEM;


   function x_Scale (Self : in Item) return Real
   is
   begin
      return Self.impl.x_Scale;
   end x_Scale;


   function y_Scale (Self : in Item) return Real
   is
   begin
      return Self.impl.y_Scale;
   end y_Scale;


   function check_Glyphs (Self : access Item;   Text     : in String;
                                                Length   : in Integer             := -1;
                                                Position : in Vector_3            := Origin_3D;
                                                Spacing  : in Vector_3            := Origin_3D;
                                                Mode     : in fontImpl.RenderMode := fontImpl.RENDER_ALL) return Vector_3
   is
      function to_Integer is new ada.Unchecked_Conversion (fontImpl.RenderMode, Integer);
   begin
      return Self.impl.Render (Text,
                               Length,
                               Position,
                               Spacing,
                               to_Integer (Mode));
   end check_Glyphs;


end openGL.Font;
