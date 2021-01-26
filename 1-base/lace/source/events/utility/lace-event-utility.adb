with
     lace.Event.Logger.text,
     ada.unchecked_Deallocation,
     system.RPC;

package body lace.Event.utility
is
   -- Event Kinds
   --

   function to_Kind (From : in ada.Tags.Tag) return lace.event.Kind
   is
      --  use lace.Event.conversions;
   begin
      return event.Kind (ada.Tags.external_Tag (From));  -- to_event_Kind (From);
   end to_Kind;


   function Name_of (Kind : in event.Kind) return String
   is
   begin
      return String (Kind);
   end Name_of;

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

   procedure connect (the_Observer  : in Observer.view;
                      to_Subject    : in Subject .view;
                      with_Response : in Response.view;
                      to_Event_Kind : in event.Kind)
   is
   begin
      the_Observer.add (with_Response,
                        to_Event_Kind,
                        to_Subject.Name);

      to_Subject.register (the_Observer,
                           to_Event_Kind);
   end connect;


   procedure disconnect (the_Observer  : in Observer.view;
                         from_Subject  : in Subject .view;
                         for_Response  : in Response.view;
                         to_Event_Kind : in event.Kind;
                         Subject_Name  : in String)
   is
   begin
      begin
         the_Observer.rid (for_Response,
                           to_Event_Kind,
                           Subject_Name);
      exception
         when storage_Error =>
            null;   -- The observer is dead.
      end;

      begin
         from_Subject.deregister (the_Observer,
                                  to_Event_Kind);
      exception
            when system.RPC.communication_Error
               | storage_Error =>
            null;   -- The subject is dead.
      end;
   end disconnect;


   -- Logging
   --

   the_Logger : event.Logger.text.view;


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
      use type event.Logger.text.view;
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


end lace.Event.utility;
