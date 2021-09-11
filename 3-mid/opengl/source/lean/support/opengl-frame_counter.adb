with
     ada.Text_IO;

package body openGL.frame_Counter
is

   procedure increment (Self : in out Item)
   is
      use      ada.Text_IO;
      use type ada.Calendar.Time;

   begin
      if ada.Calendar.Clock >= Self.next_FPS_Time
      then
         put_Line ("FPS:" & Integer'Image (Self.frame_Count));

         Self.next_FPS_Time := Self.next_FPS_Time + 1.0;
         Self.frame_Count := 0;

      else
         Self.frame_Count := Self.frame_Count + 1;
      end if;
   end increment;

end openGL.frame_Counter;
