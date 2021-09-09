package openGL.Light
--
--  Models a light.
--
is
   type Item is abstract tagged private;

   function  is_On   (Self : in     Item)     return Boolean;
   procedure is_On   (Self : in out Item;   Now : in Boolean := True);

   function  Site    (Self : in     Item)     return openGL.Site;
   procedure Site_is (Self : in out Item;   Now : in openGL.Site);



private

   type Item is abstract tagged
      record
         On   : Boolean     := False;
         Site : openGL.Site := (0.0, 0.0, 1.0);     -- The GL default.
      end record;

end openGL.Light;
