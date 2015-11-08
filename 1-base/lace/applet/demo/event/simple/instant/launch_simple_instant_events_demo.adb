-- pragma Profile (Ravenscar);

with
     lace_demo_Events,
     lace_demo_Keyboard,

     lace.Observer.instant,
     lace.Subject .local,

     lace.Response,
     lace.event.Utility.local,
     lace.event.Logger,

     ada.text_IO,
     ada.Strings.unbounded,
     ada.real_Time;



procedure launch_simple_instant_events_Demo
--
-- A simple demonstration of the Lace event system.
--
is
   use lace_demo_Events,
       lace.Event,
       lace.event.Logger,
       lace.event.Utility,
       Lace,

       ada.text_IO,
       ada.Strings.unbounded,
       ada.real_Time;


   -----------------
   --- key_Response
   --

   type Map_of_key_to_Message is array (Character) of unbounded_String;


   type key_Response is new Response.item with
      record
         key_to_message_Map : Map_of_key_to_Message;
      end record;

   procedure respond (Self : in out key_Response;   to_Event : in Event.item'Class)
   is
      the_Event : keyboard_Event renames keyboard_Event (to_Event);
   begin
      put_Line (   "Message is: "                                            -- Our response is to display the message associated
                &  to_String (Self.key_to_message_Map (the_Event.Key)));     -- with the keyboard event key on the console.
   end;


   ------------
   --- Globals
   --

   the_Subject  :         Subject.local.view;
   the_Observer :         Observer.instant.view := Observer.instant.forge.new_Observer ("demo.Observer");

   the_Response : aliased key_Response          := (Response.item with
                                                    key_to_message_Map => ('a'    => to_unbounded_String ("'a' was received from demo keyboard."),
                                                                           'b'    => to_unbounded_String ("'b' was received from demo keyboard."),
                                                                           others => to_unbounded_String ("Unhandled key was received from demo keyboard.")));
   Now          :         Ada.Real_Time.Time    := Ada.Real_Time.Clock;

begin
   event.Utility.local.use_text_Logger (log_filename => "events_demo");            -- Enable 'simple text file' event logging.


   the_Subject := lace_demo_Keyboard.as_event_Subject;                             -- Get a reference to the keyboard as an event subject.

   event.Utility.local.connect (the_observer  => Observer.view (the_Observer),     -- Setup out response to a keyboard event.
                                to_subject    => Subject .view (the_Subject),
                                with_response => the_Response'unchecked_Access,
                                to_event_kind => to_Kind (keyboard_Event'Tag));
   for Each in 1 .. 5
   loop                                                                            -- Our main loop.
      Now := Now + to_time_Span (1.0);
      delay until Now;
   end loop;


   event.Utility.local.close;                                                      -- Ensures event logging is closed (ie saved to log file).
end launch_simple_instant_events_Demo;
