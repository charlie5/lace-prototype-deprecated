package body openGL.Light
is

   function  Site    (Self : in Item)     return openGL.Site
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
