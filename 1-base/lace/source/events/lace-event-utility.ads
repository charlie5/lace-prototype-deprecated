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

   function Name_of (the_Kind : in event.Kind)   return String;

   function to_Kind (From     : in ada.Tags.Tag) return lace.Event.Kind;
   function "+"     (From     : in ada.Tags.Tag) return lace.Event.Kind
                     renames to_Kind;


   -----------
   --- Events
   --

   function Name_of (the_Event : in Event.item'Class) return String;
   function Kind_of (the_Event : in Event.item'Class) return event.Kind;


end lace.Event.Utility;
