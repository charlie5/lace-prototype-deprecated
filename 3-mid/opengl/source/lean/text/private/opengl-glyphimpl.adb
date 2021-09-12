with
     freetype_c.Binding,
     freetype_c.FT_BBox,
     freetype_c.FT_Vector;

package body openGL.GlyphImpl
is
   -----------
   --  Utility
   --

   function Bounds_of (glyth_Slot : in freetype_c.FT_GlyphSlot.item) return Bounds
   is
      use freetype_c.Binding;

      bBox       : aliased freetype_c.FT_BBox.item;
      the_Bounds :         Bounds;

   begin
      FT_Outline_Get_CBox (FT_GlyphSlot_Get_Outline (glyth_Slot).all'unchecked_Access,
                           bBox'unchecked_Access);

      the_Bounds := (Ball => <>,
                     Box  => (Lower => (1 => Real (bbox.xMin) / 64.0,
                                        2 => Real (bbox.yMin) / 64.0,
                                        3 => 0.0),
                              Upper => (1 => Real (bbox.xMax) / 64.0,
                                        2 => Real (bbox.yMax) / 64.0,
                                        3 => 0.0)));
      set_Ball_from_Box (the_Bounds);

      return the_Bounds;
   end Bounds_of;


   ---------
   --  Forge
   --

   procedure define (Self : in out Item;   glyth_Slot : in freetype_c.FT_GlyphSlot.item)
   is
      use type freetype_c.FT_GlyphSlot.item;
   begin
      Self.Err := no_Error;

      if glyth_Slot /= null
      then
         Self.bBox := Bounds_of (glyth_Slot);

         declare
            use freetype_c.Binding;
            the_Advance : constant freetype_c.FT_Vector.item := FT_GlyphSlot_Get_Advance (glyth_Slot);
         begin
            Self.Advance := (Real (the_Advance.x) / 64.0,
                             Real (the_Advance.y) / 64.0,
                             0.0);
         end;
      end if;
   end define;


   procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;


   --------------
   --  Attributes
   --

   function Advance (Self : in Item) return Real
   is
   begin
      return Self.Advance (1);
   end Advance;


   function BBox (Self : in Item) return Bounds
   is
   begin
      return Self.bBox;
   end BBox;


   function Error (Self : in Item) return error_Kind
   is
   begin
      return Self.Err;
   end Error;


end openGL.GlyphImpl;
