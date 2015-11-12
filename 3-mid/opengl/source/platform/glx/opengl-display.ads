with
     Interfaces.C;


package openGL.Display
--
--  Models an openGL display.
--
is

   type Item is tagged private;


   function Default                    return Item;
   function screen_Id (Self : in Item) return interfaces.C.int;



private

   use Interfaces;

   type Item is tagged
      record
         screen_Id  : aliased interfaces.C.int;
      end record;

end openGL.Display;


