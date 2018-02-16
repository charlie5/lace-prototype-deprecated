package openGL.Light
--
--  Models a light.
--
is

   type Item is abstract tagged private;

   function  Site    (Self : in     Item)     return openGL.Site;
   procedure Site_is (Self : in out Item;   Now : in openGL.Site);



private

   type Item is abstract tagged
      record
         Site : openGL.Site := (0.0, 0.0, 100_000_000.0);
      end record;

end openGL.Light;
