with
     lace.Subject.local;

package lace_demo_Keyboard
--
-- Provides a simulated keyboard which periodically emits 'key' events.
--
is
   function as_event_Subject return lace.Subject.local.view;

   procedure start;
   procedure stop;

end lace_demo_Keyboard;
