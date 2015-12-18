with
     lace.remote.Observer,
     lace.remote.Subject,
     lace.remote.Response,
     lace.Event.remote.Logger;


package lace.event.remote.Utility
--
--  Provides convenience subprograms for working with remote events.
--
is

   ----------------
   --- Connections
   --

   procedure connect    (the_Observer  : in lace.remote.Observer.view;
                         to_Subject    : in lace.remote.Subject.view;
                         with_Response : in lace.remote.Response.view;
                         to_event_Kind : in event.Kind);

   procedure disconnect (the_Observer  : in lace.remote.Observer.view;
                         from_Subject  : in lace.remote.Subject.view;
                         for_Response  : in lace.remote.Response.view;
                         to_event_Kind : in event.Kind);




   ------------
   --- Logging
   --


   procedure use_text_Logger (log_Filename : in String);
   --
   --  Requests activation of the default text file logger.



   function  Logger return access lace.event.remote.Logger.item'Class;
   --
   --  Returns the Logger currently in use.
   --  Returns null, if no Logger is in use.






   ----------------
   --- Termination
   --

   procedure close;
   --
   --  Ensures any registered event logger is destroyed.



end lace.event.remote.Utility;
