with
     lace.event.Logger.text,
     ada.unchecked_Deallocation,
     system.RPC;

package body lace.Event.utility_r
is
   -- Connections
   --

   procedure connect (the_Observer  : in lace.Observer.view;
                      to_Subject    : in lace.Subject .view;
                      with_Response : in lace.Response.view;
                      to_Event_Kind : in lace.event.Kind)
   is
   begin
      the_Observer.add (with_Response,
                        to_Event_Kind,
                        to_Subject.Name);

      to_Subject.register (the_Observer,
                           to_Event_Kind);
   end connect;


   -- *** ToDo => The following may be useful as a non-remote utility. ***
   --
   --
   procedure disconnect (the_Observer  : in lace.Observer.view;
                         from_Subject  : in lace.Subject .view;
                         for_Response  : in lace.Response.view;
                         to_Event_Kind : in lace.event.Kind;
                         Subject_Name  : in String)
   is
   begin
      begin
         the_Observer.rid (for_Response,
                           to_Event_Kind,
                           Subject_Name);
      exception
         when Storage_Error =>
            null;   -- The observer is dead.
      end;

      begin
         from_Subject.deregister (the_Observer,
                                  to_Event_Kind);
      exception
            when system.RPC.Communication_Error
               | Storage_Error =>
            null;   -- The subject is dead.
      end;
   end disconnect;


   -- Logging
   --

   the_Logger : lace.event.Logger.text.view;


   procedure use_text_Logger (log_Filename : in String)
   is
   begin
      the_Logger := new event.Logger.text.item' (event.Logger.text.to_Logger (log_Filename));

      lace.Subject .Logger_is (the_Logger);
      lace.Observer.Logger_is (the_Logger);
   end use_text_Logger;


   function Logger return access lace.event.Logger.item'Class
   is
   begin
      return the_Logger;
   end Logger;


   -- Termination
   --

   procedure close
   is
      use type lace.event.Logger.text.view;
   begin
      if the_Logger /= null
      then
         declare
            procedure deallocate is new ada.unchecked_Deallocation (event.Logger.text.item'Class,
                                                                    event.Logger.text.view);
         begin
            the_Logger.destruct;
            deallocate (the_Logger);
         end;
      end if;
   end close;

end lace.Event.utility_r;
