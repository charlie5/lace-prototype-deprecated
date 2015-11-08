with lace.Subject.local;


package lace_demo_Keyboard
--
-- Provides a simulated keyboard which periodically emit 'key' events.
--
is

   function as_event_Subject return lace.Subject.local.view;

end lace_demo_Keyboard;
