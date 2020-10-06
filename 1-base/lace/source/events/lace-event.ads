private
with
     System;

package lace.Event
--
--  The base class for all derived Event types.
--
is
   pragma Pure;


   type Item is tagged null record;

   null_Event : constant Event.item;


   procedure destruct (Self : in out Item) is null;



   type Kind is private;
   --
   --  Uniquely identifies each derived event class.
   --
   --  Each derived event class will have its own Kind.
   --
   --  Maps to the 'ada.tags.Tag_type' value of each derived
   --  event class (see 'Conversions' section in 'lace.event.Utility').



private

   null_Event : constant Event.item := (others => <>);

   type Kind is new system.Address;

end lace.Event;
