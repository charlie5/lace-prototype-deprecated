private
with
     ada.Calendar;

package openGL.frame_Counter
--
-- A utility which reports frames per second.
--
is
   type Item is tagged private;

   procedure increment (Self : in out Item);



private

   type Item is tagged
      record
         frame_Count   : Natural           := 0;
         next_FPS_Time : ada.Calendar.Time := ada.Calendar.Clock;
      end record;

end openGL.frame_Counter;
