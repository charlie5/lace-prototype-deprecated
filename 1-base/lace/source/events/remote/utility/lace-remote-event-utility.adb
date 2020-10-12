with
     lace.remote.Event.Logger.text,
     lace.Event_conversions,
     ada.Tags,
     ada.unchecked_Deallocation;

package body lace.remote.Event.utility
is
   -- Event Kinds
   --

   function to_Kind (From : in ada.Tags.Tag) return lace.Event.Kind
   is
      use lace.Event_conversions;
   begin
      return to_event_Kind (From);
   end to_Kind;


   function Name_of (the_Kind : in lace.Event.Kind) return String
   is
      use lace.Event_conversions,
          ada.Tags;
   begin
      return expanded_Name (to_Tag (the_Kind));
   end Name_of;


   -- Events
   --

   function Kind_of (the_Event : in lace.Event.item'Class) return lace.Event.Kind
   is
   begin
      return to_Kind (the_Event'Tag);
   end Kind_of;


   function Name_of (the_Event : in lace.Event.item'Class) return String
   is
   begin
      return Name_of (Kind_of (the_Event));
   end Name_of;


   -- Connections
   --

   procedure connect (the_Observer  : in lace.remote.Observer.view;
                      to_Subject    : in lace.remote.Subject .view;
                      with_Response : in lace.remote.Response.view;
                      to_Event_Kind : in lace.Event.Kind)
   is
   begin
      the_Observer.add (with_Response,
                        to_Event_Kind,
                        to_Subject.Name);

      to_Subject.register (the_Observer,
                           to_Event_Kind);
   end connect;


   procedure disconnect (the_Observer  : in lace.remote.Observer.view;
                         from_Subject  : in lace.remote.Subject .view;
                         for_Response  : in lace.remote.Response.view;
                         to_Event_Kind : in lace.Event.Kind)
   is
   begin
      the_Observer.rid (for_Response,
                        to_Event_Kind,
                        from_Subject.Name);

      from_Subject.deregister (the_Observer,
                               to_Event_Kind);
   end disconnect;


   -- Logging
   --

   the_Logger : lace.remote.Event.logger.text.view;


   procedure use_text_Logger (log_Filename : in String)
   is
   begin
      the_Logger := new Event.Logger.text.item'(Event.Logger.text.to_Logger (log_Filename));

      lace.remote.Subject .Logger_is (the_Logger);
      lace.remote.Observer.Logger_is (the_Logger);
   end use_text_Logger;


   function Logger return access lace.remote.Event.Logger.item'Class
   is
   begin
      return the_Logger;
   end Logger;


   -- Termination
   --

   procedure close
   is
      use type lace.remote.Event.Logger.text.view;
   begin
      if the_Logger /= null
      then
         declare
            procedure deallocate is new ada.unchecked_Deallocation (Event.Logger.text.item'Class,
                                                                    Event.Logger.text.view);
         begin
            the_Logger.destruct;
            deallocate (the_Logger);
         end;
      end if;
   end close;


end lace.remote.Event.utility;
