with
     lace.Observer,
     lace.Subject,
     lace.Response,
     lace.event.Logger,
     ada.Tags;

package lace.Event.utility
--
--  Provides convenience subprograms for working with events.
--
is
   -- Event Kinds
   --

   function Name_of (Kind : in event.Kind)   return String;

   function to_Kind (From : in ada.Tags.Tag) return event.Kind;
   function "+"     (From : in ada.Tags.Tag) return event.Kind
                     renames to_Kind;

   -- Events
   --

   function Name_of (the_Event : in Event.item'Class) return String;
   function Kind_of (the_Event : in Event.item'Class) return Event.Kind;


   -- Connections
   --

   procedure connect    (the_Observer  : in Observer.view;
                         to_Subject    : in Subject .view;
                         with_Response : in Response.view;
                         to_Event_Kind : in event.Kind);

   procedure disconnect (the_Observer  : in Observer.view;
                         from_Subject  : in Subject .view;
                         for_Response  : in Response.view;
                         to_Event_Kind : in event.Kind;
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

end lace.Event.utility;
