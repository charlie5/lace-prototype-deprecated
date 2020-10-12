package body lace.remote.Observer
is
   the_Logger : access remote.event.Logger.item'Class;


   procedure Logger_is (Now : access lace.remote.event.Logger.item'Class)
   is
   begin
      the_Logger := Now;
   end Logger_is;


   function Logger return access lace.remote.event.Logger.item'Class
   is
   begin
      return the_Logger;
   end Logger;

end lace.remote.Observer;
