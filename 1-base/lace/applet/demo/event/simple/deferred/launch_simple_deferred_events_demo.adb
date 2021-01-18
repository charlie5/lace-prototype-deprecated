with
     lace_demo_Events,
     lace_demo_Keyboard,

     lace.Observer.deferred,
     lace.Subject .local,

     lace.Response,
     lace.Event.utility,

     ada.Text_IO,
     ada.Strings.unbounded,
     ada.real_Time;

procedure launch_simple_deferred_events_Demo
--
-- A simple demonstration of the Lace deferred event system.
--
is
   use lace_demo_Events,
       lace.Event,
       lace.event.Utility,
       Lace,

       ada.text_IO,
       ada.Strings.unbounded,
       ada.real_Time;

   -- Key Response
   --

   type key_Map_of_message is array (Character) of unbounded_String;

   type key_Response is new Response.item with
      record
         key_to_message_Map : key_Map_of_message;
      end record;

   overriding
   procedure respond (Self : in out key_Response;   to_Event : in Event.item'Class)
   is
      the_Event : keyboard_Event renames keyboard_Event (to_Event);
   begin
      put_Line (  "Message is: "                                          -- Our response is to display the message associated
                & to_String (Self.key_to_message_Map (the_Event.Key)));   -- with the keyboard event key on the console.
   end respond;


   --- Globals
   --

   the_Subject  :          Subject.local.view;
   the_Observer : constant Observer.deferred.view := Observer.deferred.forge.new_Observer ("demo.Observer");
   the_Response : aliased  key_Response          := (Response.item with
                                                     key_to_message_Map => ('a'    => to_unbounded_String ("'a' was received from demo keyboard."),
                                                                            'b'    => to_unbounded_String ("'b' was received from demo keyboard."),
                                                                            others => to_unbounded_String ("Unhandled key was received from demo keyboard.")));
   Now : ada.real_Time.Time := ada.real_Time.Clock;

begin
   Event.utility.use_text_Logger (log_filename => "events_demo");          -- Enable 'simple text file' event logging.

   the_Subject := lace_demo_Keyboard.as_event_Subject;                    -- Get a reference to the keyboard as an event subject.

   Event.utility.connect (the_observer  => Observer.view (the_Observer),   -- Setup out response to a keyboard event.
                         to_subject    => Subject .view (the_Subject),
                         with_response => the_Response'unchecked_Access,
                         to_event_kind => to_Kind (keyboard_Event'Tag));
   lace_demo_Keyboard.start;

   for Each in 1 .. 5
   loop                       -- Our main loop.
      the_Observer.respond;   -- Response to any queued events occur here.

      Now := Now + to_time_Span (1.0);
      delay until Now;
   end loop;

   lace_demo_Keyboard.stop;
   Event.utility.close;       -- Ensures event logging is closed (ie saved to log file).
end launch_simple_deferred_events_Demo;
