with
     lace.Event,
     lace.remote.Observer,
     lace.remote.Subject,
     lace.remote.Response,
     lace.remote.Event.Logger;

package lace.remote.Event.utility
--
--  Provides convenience subprograms for working with remote events.
--
is
   -- Connections
   --

   procedure connect    (the_Observer  : in lace.remote.Observer.view;
                         to_Subject    : in lace.remote.Subject .view;
                         with_Response : in lace.remote.Response.view;
                         to_Event_Kind : in lace.Event.Kind);

   procedure disconnect (Observer_1    : in lace.remote.Observer.view;
                         Subject_1     : in lace.remote.Subject .view;
                         Observer_2    : in lace.remote.Observer.view;
                         for_Response  : in lace.remote.Response.view;
                         to_Event_Kind : in lace.Event.Kind);
   -- Logging
   --

   procedure use_text_Logger (log_Filename : in String);
   --
   --  Requests activation of the default text file logger.


   function Logger return access lace.remote.Event.Logger.item'Class;
   --
   --  Returns the Logger currently in use.
   --  Returns null, if no Logger is in use.


   -- Termination
   --

   procedure close;
   --
   --  Ensures any registered event logger is destroyed.


end lace.remote.Event.utility;
