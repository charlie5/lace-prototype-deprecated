with
     lace.Event,
     ada.Tags;

private
package lace.Event_conversions
--
--  Provides conversions required by the lace event system.
--
is
   function to_event_Kind (From : in ada.tags.Tag)    return lace.event.Kind;
   function to_Tag        (From : in lace.event.Kind) return ada.tags.Tag;
end lace.Event_conversions;
