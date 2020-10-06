with
     ada.Tags;

package lace.Event.utility
--
--  Provides convenience subprograms for working with events.
--
is
   --- Event Kinds
   --

   function Name_of (Kind : in event.Kind)   return String;

   function to_Kind (From : in ada.tags.Tag) return event.Kind;
   function "+"     (From : in ada.tags.Tag) return event.Kind
                     renames to_Kind;

   --- Events
   --

   function Name_of (the_Event : in Event.item'Class) return String;
   function Kind_of (the_Event : in Event.item'Class) return event.Kind;

end lace.Event.utility;
