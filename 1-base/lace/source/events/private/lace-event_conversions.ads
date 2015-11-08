with lace.Event,
     ada.Tags,
     ada.unchecked_Conversion;



private
package lace.event_Conversions
--
--  Provides conversions required by the lace event system.
--
is

   function to_event_Kind is new ada.Unchecked_Conversion (ada.Tags.Tag,    lace.Event.Kind);
   function to_Tag        is new ada.Unchecked_Conversion (lace.Event.Kind, ada.Tags.Tag);

end lace.event_Conversions;
