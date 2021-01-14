package body lace.remote.Observer
is
   the_Logger : access remote.Event.Logger.item'Class;


   procedure Logger_is (Now : access lace.remote.Event.Logger.item'Class)
   is
   begin
      the_Logger := Now;
   end Logger_is;


   function Logger return access lace.remote.Event.Logger.item'Class
   is
   begin
      return the_Logger;
   end Logger;

end lace.remote.Observer;
