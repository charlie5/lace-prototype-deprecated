package body lace.Event.conversions
is
   function to_event_Kind (From : in ada.tags.Tag) return lace.event.Kind
   is
   begin
      return event.Kind (ada.tags.external_Tag (From));
   end to_event_Kind;


   function to_Tag (From : in lace.event.Kind) return ada.tags.Tag
   is
   begin
      return ada.tags.internal_Tag (String (From));
   end to_Tag;

end lace.Event.conversions;
