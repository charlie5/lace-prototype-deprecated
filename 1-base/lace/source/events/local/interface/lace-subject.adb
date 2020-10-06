package body lace.Subject
is
   the_Logger : access event.Logger.item'Class;


   procedure Logger_is (Now : access event.Logger.item'Class)
   is
   begin
      the_Logger := Now;
   end Logger_is;


   function Logger return access event.Logger.item'Class
   is
   begin
      return the_Logger;
   end Logger;

end lace.Subject;
