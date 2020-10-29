package body lace.remote.Subject
is
   the_Logger : access remote.event.Logger.item'Class;


   procedure Logger_is (Now : access remote.event.Logger.item'Class)
   is
   begin
      the_Logger := Now;
   end Logger_is;


   function Logger return access remote.event.Logger.item'Class
   is
   begin
      return the_Logger;
   end Logger;

end lace.remote.Subject;
