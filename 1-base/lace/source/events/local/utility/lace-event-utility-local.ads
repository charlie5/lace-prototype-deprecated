with
     lace.Observer,
     lace.Subject,
     lace.Response,
     lace.Event.Logger;


package lace.event.Utility.local
--
--  Provides convenience subprograms for working with events.
--
is

--     ----------------
--     --- Event Kinds
--     --
--
--     function to_Kind (From     : in ada.Tags.Tag) return lace.Event.Kind;
--     function Name_of (the_Kind : in event.Kind)   return String;





   -----------
   --- Events
   --

   function Name_of (the_Event : in Event.item'Class) return String;
   function Kind_of (the_Event : in Event.item'Class) return event.Kind;





   ----------------
   --- Connections
   --

   procedure connect    (the_Observer  : in lace.Observer.view;
                         to_Subject    : in lace.Subject.view;
                         with_Response : in lace.Response.view;
                         to_event_Kind : in event.Kind);

   procedure disconnect (the_Observer  : in lace.Observer.view;
                         from_Subject  : in lace.Subject.view;
                         for_Response  : in lace.Response.view;
                         to_event_Kind : in event.Kind);




   ------------
   --- Logging
   --


   procedure use_text_Logger (log_Filename : in String);
   --
   --  Requests activation of the default text file logger.



   function  Logger return access lace.event.Logger.item'Class;
   --
   --  Returns the Logger currently in use.
   --  Returns null, if no Logger is in use.






   ----------------
   --- Termination
   --

   procedure close;
   --
   --  Ensures any registered event logger is destroyed.



end lace.event.Utility.local;
