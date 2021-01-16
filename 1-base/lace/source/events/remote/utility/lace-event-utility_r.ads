with
     lace.Event,
     lace.Observer,
     lace.Subject,
     lace.Response,
     lace.event.Logger;

package lace.Event.utility_r
--
--  Provides convenience subprograms for working with events.
--
is
   -- Connections
   --

   procedure connect    (the_Observer  : in lace.Observer.view;
                         to_Subject    : in lace.Subject .view;
                         with_Response : in lace.Response.view;
                         to_Event_Kind : in lace.event.Kind);

   procedure disconnect (the_Observer  : in lace.Observer.view;
                         from_Subject  : in lace.Subject .view;
                         for_Response  : in lace.Response.view;
                         to_Event_Kind : in lace.event.Kind;
                         subject_Name  : in String);
   -- Logging
   --

   procedure use_text_Logger (log_Filename : in String);
   --
   --  Requests activation of the default text file logger.


   function Logger return access lace.event.Logger.item'Class;
   --
   --  Returns the Logger currently in use.
   --  Returns null, if no Logger is in use.


   -- Termination
   --

   procedure close;
   --
   --  Ensures any registered event logger is destroyed.


end lace.Event.utility_r;
