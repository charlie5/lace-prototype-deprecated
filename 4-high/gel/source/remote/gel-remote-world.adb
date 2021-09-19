package body gel.remote.World
is

   function refined (Self : in coarse_Vector_3) return math.Vector_3
   is
   begin
      return (math.Real (Self (1)),
              math.Real (Self (2)),
              math.Real (Self (3)));
   end refined;



   function coarsen (Self : in math.Vector_3) return coarse_Vector_3
   is
      Result : coarse_Vector_3;
   begin
      begin
         Result (1) := coarse_Real (Self (1));
      exception
         when constraint_Error =>
            if Self (1) > 0.0 then
               Result (1) := coarse_Real'Last;
            else
               Result (1) := coarse_Real'First;
            end if;
      end;

      begin
         Result (2) := coarse_Real (Self (2));
      exception
         when constraint_Error =>
            if Self (2) > 0.0 then
               Result (2) := coarse_Real'Last;
            else
               Result (2) := coarse_Real'First;
            end if;
      end;

      begin
         Result (3) := coarse_Real (Self (3));
      exception
         when constraint_Error =>
            if Self (3) > 0.0 then
               Result (3) := coarse_Real'Last;
            else
               Result (3) := coarse_Real'First;
            end if;
      end;

      return Result;
   end coarsen;



   function refined (Self : in coarse_Quaternion) return math.Quaternion
   is
   begin
      return (R =>  math.Real (Self (1)),
              V => (math.Real (Self (2)),
                    math.Real (Self (3)),
                    math.Real (Self (4))));
   end refined;



   function coarsen (Self : in math.Quaternion) return coarse_Quaternion
   is
      Result : coarse_Quaternion;
   begin
      begin
         Result (1) := coarse_Real2 (Self.R);
      exception
         when constraint_Error =>
            if Self.R > 0.0 then
               Result (1) := coarse_Real2'Last;
            else
               Result (1) := coarse_Real2'First;
            end if;
      end;

      begin
         Result (2) := coarse_Real2 (Self.V (1));
      exception
         when constraint_Error =>
            if Self.V (1) > 0.0 then
               Result (2) := coarse_Real2'Last;
            else
               Result (2) := coarse_Real2'First;
            end if;
      end;

      begin
         Result (3) := coarse_Real2 (Self.V (2));
      exception
         when constraint_Error =>
            if Self.V (2) > 0.0 then
               Result (3) := coarse_Real2'Last;
            else
               Result (3) := coarse_Real2'First;
            end if;
      end;

      begin
         Result (4) := coarse_Real2 (Self.V (3));
      exception
         when Constraint_Error =>
            if Self.V (3) > 0.0 then
               Result (4) := coarse_Real2'Last;
            else
               Result (4) := coarse_Real2'First;
            end if;
      end;

      return Result;
   end coarsen;


   -----------
   --- Streams
   --

   use ada.Streams;

   number_of_stream_Elements_for_a_motion_Update : constant Stream_Element_Offset
     := motion_Update'Size / Stream_Element'Size;


   procedure motion_Updates_write (Stream : access ada.Streams.Root_Stream_type'Class;   Item : in  motion_Updates)
   is
      stream_element_array_Length : constant Stream_Element_Offset
        := Item'Length * number_of_stream_Elements_for_a_Motion_Update;

      subtype the_Stream_Element_Array  is Stream_Element_Array (1 .. stream_element_array_Length);

      function to_Stream_Element_Array is new ada.unchecked_Conversion (motion_Updates, the_Stream_Element_Array);

   begin
      write (Stream.all, to_Stream_Element_Array (Item));
   end motion_Updates_write;



   procedure motion_Updates_read (Stream : access ada.Streams.Root_Stream_type'Class;    Item : out motion_Updates)
   is
      subtype the_Stream_Element_Array
        is Stream_Element_Array (1 .. Item'Length * number_of_stream_Elements_for_a_motion_Update);

      subtype the_motion_Updates is motion_Updates (1 .. Item'Length);

      function to_motion_Updates is new ada.unchecked_Conversion (the_Stream_Element_Array, the_motion_Updates);

      the_Stream_Array : the_Stream_Element_Array;
      Last             : Stream_Element_Offset;

   begin
      read (Stream.all, the_Stream_Array, Last);

      pragma assert (Last = the_Stream_Array'Last);

      Item := to_motion_Updates (the_Stream_Array (1 .. Last));
   end motion_Updates_read;



   procedure Write (Stream    : not null access ada.Streams.Root_Stream_type'Class;
                    the_Event : in              new_model_Event)
   is
   begin
      openGL.remote_Model.item'Class'Output (Stream,
                                             the_Event.Model.all);
   end Write;



   procedure Read (Stream    : not null access ada.Streams.Root_Stream_type'Class;
                   the_Event : out             new_model_Event)
   is
   begin
      the_Event.Model := new openGL.remote_Model.item'Class' (openGL.remote_Model.item'Class'Input (Stream));
   end Read;



   procedure Write (Stream    : not null access ada.Streams.Root_Stream_type'Class;
                    the_Event : in              new_physics_model_Event)
   is
   begin
      physics.Remote.Model.item'Class'Output (Stream, the_Event.Model.all);
   end Write;


   procedure Read (Stream    : not null access ada.Streams.Root_Stream_type'Class;
                   the_Event : out             new_physics_model_Event)
   is
   begin
      the_Event.Model := new physics.remote.Model.item'Class' (physics.remote.Model.item'Class'Input (Stream));
   end Read;


end gel.remote.World;
