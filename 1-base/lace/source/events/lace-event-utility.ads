with
     ada.Tags;


package lace.Event.Utility
--
--  Provides convenience subprograms for working with events.
--
is

   ----------------
   --- Event Kinds
   --

   function to_Kind (From     : in ada.Tags.Tag) return lace.Event.Kind;
   function Name_of (the_Kind : in event.Kind)   return String;


   -----------
   --- Events
   --

   function Name_of (the_Event : in Event.item'Class) return String;
   function Kind_of (the_Event : in Event.item'Class) return event.Kind;


end lace.Event.Utility;
