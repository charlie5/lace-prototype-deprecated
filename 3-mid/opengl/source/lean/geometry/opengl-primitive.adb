with
     openGL.Tasks,
     ada.Unchecked_Deallocation;


package body openGL.Primitive
is

   ---------
   --  Forge
   --

   procedure define (Self : in out Item;   Kind    : in facet_Kind)
   is
   begin
      Self.facet_Kind := Kind;
   end define;


   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.Unchecked_Deallocation (Primitive.item'Class,
                                                              Primitive.view);
   begin
      Self.destroy;
      deallocate (Self);
   end free;



   --------------
   --  Attributes
   --

   function  Texture (Self : in Item) return openGL.Texture.Object
   is
   begin
      return self.Texture;
   end Texture;


   procedure Texture_is (Self : in out Item;   Now : in openGL.Texture.Object)
   is
   begin
      self.Texture := Now;
   end Texture_is;



   function  Bounds (self : in Item) return openGL.Bounds
   is
   begin
      return self.Bounds;
   end Bounds;


   procedure Bounds_are (Self : in out Item;   Now : in openGL.Bounds)
   is
   begin
      self.Bounds := Now;
   end Bounds_are;



   function  is_Transparent (self : in Item) return Boolean
   is
   begin
      return self.is_Transparent;
   end is_Transparent;


   procedure is_Transparent (Self : in out Item;   Now : in Boolean := True)
   is
   begin
      self.is_Transparent := Now;
   end is_Transparent;



   --------------
   --- Operations
   --

   procedure render (Self : in out Item)
   is
      use GL;
      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
   begin
      if Self.line_Width /= unused_line_Width
      then
         glLineWidth (glFloat (Self.line_Width));
      end if;
   end render;


end openGL.Primitive;
