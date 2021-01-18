with lace.event.Logger,
     lace.event_Conversions,

     gnat.task_Lock;



package body lace.make_Observer.deferred
is



   procedure receive (Self : access Item;   the_Event    : in Event.item'Class := event.null_Event;
                                            from_Subject : in String)
   is
      use event_Vectors;
   begin
      gnat.task_Lock.lock;

      if not Self.pending_Events.contains (from_Subject) then
         Self.pending_Events.insert (from_Subject,
                                     new event_Vector);
      end if;

      Self.pending_Events.Element (from_Subject).append (the_Event);

      gnat.task_Lock.unlock;
   end receive;





   procedure respond (Self : access Item)
   is
      use event_Vectors;

      my_Name : String := Observer.Item'Class (Self.all).Name;


      --- actuate
      --
      procedure actuate (the_Responses     : in     event_response_Map;
                         for_Events        : in out event_Vector;
                         from_Subject_Name : in     String)
      is
         the_Events : event_Vector         renames for_Events;
         Cursor     : event_Vectors.Cursor;

      begin
         Cursor := the_Events.First;

         while has_Element (Cursor) loop
            declare
               use event_response_Maps,  event_Conversions,  ada.Containers;
               use type Observer.view;

               the_Event : Event.item'Class           := Element (Cursor);
               Response  : event_response_Maps.Cursor := the_Responses.find (to_event_Kind (the_Event'tag));

            begin

               if has_Element (Response) then
                  Element (Response).respond (the_Event);

                  if Observer.Logger /= null then
                     Observer.Logger.log_Response (Element (Response),  Observer.view (Self),  the_Event,  from_Subject_Name);
                  end if;


               elsif Self.relay_Target /= null then
                  -- Self.relay_Target.notify (the_Event, from_Subject_Name);

                  if Observer.Logger /= null then
                     Observer.Logger.log ("[Warning] ~ Relayed events are currently disabled.");
                  else
                     raise program_Error with   "Event relaying is currently disabled";
                  end if;


               else
                  if Observer.Logger /= null then
                     Observer.Logger.log ("[Warning] ~ Observer "             & my_Name & " has no response !");
                     Observer.Logger.log ("            count of responses =>" & Count_type'Image (the_Responses.Length));
                  else
                     raise program_Error with   "Observer " & my_Name & " has no response !";
                  end if;
               end if;

            end;


            next (Cursor);
         end loop;

      end actuate;



      use subject_Maps_of_safe_events;
      subject_Cursor : subject_Maps_of_safe_events.Cursor := Self.pending_Events.First;


   begin

      while has_Element (subject_Cursor)
      loop
         declare
            use subject_Maps_of_event_responses;
            subject_Name : String := Key (subject_Cursor);

         begin
            actuate (Self.subject_Responses.Element (subject_Name).all,
                     Self.pending_Events   .Element (subject_Name).all,
                     subject_Name);

            Self.pending_Events.Element (subject_Name).clear;

         exception
            when constraint_Error =>
               if Observer.Logger /= null then
                  Observer.Logger.log (my_Name & " has no responses for events from " & subject_Name);
               else
                  raise program_Error with    my_Name & " has no responses for events from " & subject_Name;
               end if;
         end;


         next (subject_Cursor);
      end loop;

   end respond;




end lace.make_Observer.deferred;
