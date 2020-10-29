with
     lace.Event_conversions;

package body lace.Event.utility
is
   -- Event Kinds
   --

   function to_Kind (From : in ada.tags.Tag) return lace.event.Kind
   is
      use lace.Event_conversions;
   begin
      return to_event_Kind (From);
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

end lace.Event.utility;
