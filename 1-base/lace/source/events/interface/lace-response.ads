with
     lace.Event;


package lace.Response
--
--  Provides a base class for all derived event 'response' classes.
--
is
   pragma remote_Types;

   type Item is abstract tagged limited private;
   type View is access all Item'class;


   -------------
   -- Attributes
   --

   function Name (Self : in Item) return String;


   -------------
   -- Operations
   --

   procedure respond (Self : in out Item;   to_Event : in Event.item'Class) is abstract;



private

   type Item is abstract tagged limited null record;

end lace.Response;
