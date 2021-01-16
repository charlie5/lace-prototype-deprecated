with
     lace.event.Logger.text,
     ada.unchecked_Deallocation;

package body lace.event.Utility.local
is
   -- Events
   --

   function Kind_of (the_Event : in Event.item'Class) return event.Kind
   is
   begin
      return to_Kind (the_Event'Tag);
   end Kind_of;


   function Name_of (the_Event : in Event.item'Class) return String
   is
   begin
      return Name_of (Kind_of (the_Event));
   end Name_of;


   -- Connections
   --

   procedure connect (the_Observer  : in lace.Observer.view;
                      to_Subject    : in lace.Subject .view;
                      with_Response : in lace.Response.view;
                      to_event_Kind : in event.Kind)
   is
   begin
      the_Observer.add (with_Response,
                        to_event_Kind,
                        to_Subject.Name);

      to_Subject.register (the_Observer,
                           to_event_Kind);
   end connect;


   procedure disconnect (the_Observer  : in lace.Observer.view;
                         from_Subject  : in lace.Subject .view;
                         for_Response  : in lace.Response.view;
                         to_event_Kind : in event.Kind)
   is
   begin
      the_Observer.rid (for_Response,
                        to_event_Kind,
                        from_Subject.Name);

      from_Subject.deregister (the_Observer,
                               to_event_Kind);
   end disconnect;


   -- Logging
   --

   the_event_Logger : event.Logger.text.view;


   procedure use_text_Logger (log_Filename : in String)
   is
   begin
      the_event_Logger := new event.Logger.text.item'(event.Logger.text.to_Logger (log_Filename));

      lace.Subject .Logger_is (the_event_Logger);
      lace.Observer.Logger_is (the_event_Logger);
   end use_text_Logger;


   function Logger return access lace.event.Logger.item'Class
   is
   begin
      return the_event_Logger;
   end Logger;


   -- Termination
   --

   procedure close
   is
      use type event.Logger.text.view;
   begin
      if the_event_Logger /= null
      then
         declare
            procedure deallocate is new ada.unchecked_Deallocation (event.Logger.text.item'Class,
                                                                    event.Logger.text.view);
         begin
            the_event_Logger.destruct;
            deallocate (the_event_Logger);
         end;
      end if;
   end close;


end lace.event.Utility.local;
