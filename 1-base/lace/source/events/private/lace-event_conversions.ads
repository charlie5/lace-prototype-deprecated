with
     lace.Event,
     ada.Tags,
     ada.unchecked_Conversion;

private
package lace.Event_conversions
--
--  Provides conversions required by the lace event system.
--
is

   --  function to_event_Kind is new ada.unchecked_Conversion (ada.tags.Tag,    lace.event.Kind);
   --  function to_Tag        is new ada.unchecked_Conversion (lace.event.Kind, ada.tags.Tag);

   function to_event_Kind (From : ada.tags.Tag) return lace.event.Kind;
   function to_Tag        (From : lace.event.Kind) return ada.tags.Tag;

end lace.Event_conversions;
