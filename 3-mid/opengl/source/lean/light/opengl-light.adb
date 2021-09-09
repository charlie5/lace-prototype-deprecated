package body openGL.Light
is

   function is_On (Self : in Item) return Boolean
   is
   begin
      return Self.On;
   end is_On;


   procedure is_On (Self : in out Item;   Now : in Boolean := True)
   is
   begin
      Self.On := Now;
   end is_On;



   function Site (Self : in Item) return openGL.Site
   is
   begin
      return Self.Site;
   end Site;


   procedure Site_is (Self : in out Item;   Now : in openGL.Site)
   is
   begin
      Self.Site := Now;
   end Site_is;


end openGL.Light;
