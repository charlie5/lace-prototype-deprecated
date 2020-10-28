package body lace.Event_conversions
--
--  Provides conversions required by the lace event system.
--
is
   function to_event_Kind (From : ada.tags.Tag) return lace.event.Kind
   is
   begin
      return event.Kind (ada.tags.External_Tag (From));
   end to_event_Kind;

   function to_Tag (From : lace.event.Kind) return ada.tags.Tag
   is
   begin
      return ada.tags.Internal_Tag (String (From));
   end to_Tag;

end lace.Event_conversions;
