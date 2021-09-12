with
     freetype_C.FT_GlyphSlot;

package openGL.GlyphImpl
--
--  Implements an openGL glyph.
--
is
   type Item is tagged private;
   type View is access all Item'Class;


   ---------
   --  Types
   --
   subtype error_Kind is freetype_C.FT_Error;

   no_Error : constant error_Kind;


   ---------
   --  Forge
   --
   procedure define (Self : in out Item;   glyth_Slot : in freetype_c.FT_GlyphSlot.item);
   --
   --  glyth_Slot: The Freetype glyph to be processed.


   --------------
   --  Attributes
   --
   function Advance (Self : in Item) return Real;         -- The advance distance for this glyph.
   function BBox    (Self : in Item) return Bounds;       -- Return the bounding box for this glyph.
   function Error   (Self : in Item) return error_Kind;   -- Return the current error code.



private

   type Item is tagged
      record
         Advance : Vector_3;
         bBox    : Bounds;
         Err     : error_Kind;
      end record;

   procedure destruct (Self : in out Item);

   no_Error : constant error_Kind := 0;

end openGL.GlyphImpl;
