with
     lace_demo_Events,
     ada.real_Time;

package body lace_demo_Keyboard
is
   use lace_demo_Events,
       Lace,
       ada.real_Time;

   --- Simulated Keyboard
   --

   the_event_Subject : constant Subject.local.view := Subject.local.forge.new_Subject ("demo.Subject");


   task type simulated_Keyboard
   is
      entry start;
      entry stop;
   end simulated_Keyboard;

   task body simulated_Keyboard
   is
      Count : Natural            := 0;
      Now   : ada.real_Time.Time := ada.real_Time.Clock;
      Done  : Boolean            := False;
   begin
      accept start;

      loop
         select
            accept stop
            do
               Done := True;
            end stop;
         or
            delay until Now;
         end select;

         exit when Done;

         if Count mod 3 = 0
         then
            the_event_Subject.emit (the_Event => keyboard_Event'(key => 'a'));
         else
            the_event_Subject.emit (the_Event => keyboard_Event'(key => 'b'));
         end if;

         Count := Count + 1;
         Now   := Now + to_time_Span (0.5);
      end loop;
   end simulated_Keyboard;

   the_simulated_Keyboard : simulated_Keyboard;


   function as_event_Subject return lace.Subject.local.view
   is
   begin
      return the_event_Subject;
   end as_event_Subject;

   procedure start
   is
   begin
      the_simulated_Keyboard.start;
   end start;

   procedure stop
   is
   begin
      the_simulated_Keyboard.stop;
   end stop;

end lace_demo_Keyboard;

