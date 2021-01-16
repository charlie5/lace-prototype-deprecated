package body lace.Observer
is
   the_Logger : access Event.Logger.item'Class;


   procedure Logger_is (Now : access lace.Event.Logger.item'Class)
   is
   begin
      the_Logger := Now;
   end Logger_is;


   function Logger return access lace.Event.Logger.item'Class
   is
   begin
      return the_Logger;
   end Logger;

end lace.Observer;
