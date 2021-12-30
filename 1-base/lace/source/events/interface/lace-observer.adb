with
     lace.Event.Logger;


package body lace.Observer
is
   the_Logger : Event.Logger.view;


   procedure Logger_is (Now : in Event.Logger.view)
   is
   begin
      the_Logger := Now;
   end Logger_is;


   function Logger return Event.Logger.view
   is
   begin
      return the_Logger;
   end Logger;

end lace.Observer;
