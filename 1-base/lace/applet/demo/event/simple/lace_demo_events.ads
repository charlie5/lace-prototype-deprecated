with
     lace.Event;

package lace_demo_Events
--
-- Provides a simple derived lace 'event'.
--
is
   pragma remote_Types;

   type keyboard_Event is new lace.Event.item with
      record
         Key : Character;
      end record;

end lace_demo_Events;
