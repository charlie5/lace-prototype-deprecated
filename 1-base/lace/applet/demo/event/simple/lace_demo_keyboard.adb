with
     lace_demo_Events,
     ada.real_Time;



package body lace_demo_Keyboard
is
   use lace_demo_Events,
       Lace,
       ada.real_Time;


   -----------------------
   --- simulated_Keyboard
   --

   the_event_Subject : Subject.local.view := Subject.local.forge.new_Subject ("demo.Subject");


   task type simulated_Keyboard;
   task body simulated_Keyboard
   is
      Now : ada.Real_Time.Time := ada.Real_Time.Clock;

   begin
      for Each in 1 .. 20
      loop
         if Each mod 3 = 0
         then
            the_event_Subject.emit (the_Event => keyboard_Event'(key => 'a'));
         else
            the_event_Subject.emit (the_Event => keyboard_Event'(key => 'b'));
         end if;

         Now := Now + to_time_Span (0.5);
         delay until Now;
      end loop;
   end simulated_Keyboard;

   the_simulated_Keyboard : simulated_Keyboard;



   function as_event_Subject return lace.Subject.local.view
   is
   begin
      return the_event_Subject;
   end;


end lace_demo_Keyboard;

