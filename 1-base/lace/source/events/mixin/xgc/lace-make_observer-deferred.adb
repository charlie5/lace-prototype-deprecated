with
     lace.Event.Logger,
     lace.Event.utility,
     ada.unchecked_Deallocation;

package body lace.make_Observer.deferred
is
   procedure free is new ada.unchecked_Deallocation (String, String_view);

   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      make_Observer.destroy (make_Observer.item (Self));   -- Destroy base class.
      Self.pending_Events.free;
   end destroy;


   overriding
   procedure receive (Self : access Item;   the_Event    : in Event.item'Class := event.null_Event;
                                            from_Subject : in Event.subject_Name)
   is
   begin
      Self.pending_Events.add (the_Event, from_Subject);
   end receive;


   overriding
   procedure respond (Self : access Item)
   is
      use Event_Vectors;

      my_Name : constant String := Observer.item'Class (Self.all).Name;

      procedure actuate (the_Responses     : in event_response_Map;
                         the_Events        : in Event_Vector;
                         from_subject_Name : in Event.subject_Name)
      is
         Cursor : Event_Vectors.Cursor := the_Events.First;
      begin
         while has_Element (Cursor)
         loop
            declare
               use event_response_Maps,
                   Event.utility,
                   ada.Containers;
               use type Observer.view;

               the_Event : constant Event.item'Class           := Element (Cursor);
               Response  : constant event_response_Maps.Cursor := the_Responses.find (to_Kind (the_Event'Tag));
            begin
               if has_Element (Response)
               then
                  Element (Response).respond (the_Event);

                  if observer.Logger /= null
                  then
                     observer.Logger.log_Response (Element (Response),
                                                   Observer.view (Self),
                                                   the_Event,
                                                   from_subject_Name);
                  end if;

               elsif Self.relay_Target /= null
               then
                  --  Self.relay_Target.notify (the_Event, from_Subject_Name);   -- todo: Re-enable relayed events.

                  if observer.Logger /= null
                  then
                     observer.Logger.log ("[Warning] ~ Relayed events are currently disabled.");
                  else
                     raise program_Error with "Event relaying is currently disabled.";
                  end if;

               else
                  if observer.Logger /= null
                  then
                     observer.Logger.log ("[Warning] ~ Observer "
                                          & my_Name
                                          & " has no response.");
                     observer.Logger.log ("            Count of responses =>"
                                          & Count_type'Image (the_Responses.Length));
                  else
                     raise program_Error with "Observer " & my_Name & " has no response.";
                  end if;
               end if;
            end;

            next (Cursor);
         end loop;
      end actuate;

      the_subject_Events : subject_events_Pairs (1 .. 5_000);
      Count              : Natural;

   begin
      Self.pending_Events.fetch (the_subject_Events, Count);

      for i in 1 .. Count
      loop
         declare
            use subject_Maps_of_event_responses;

            subject_Name : String_view       := the_subject_Events (i).Subject;
            the_Events   : Event_vector renames the_subject_Events (i).Events;

         begin
            if Self.subject_Responses.contains (subject_Name.all)
            then
               actuate (Self.subject_Responses.Element (subject_Name.all).all,
                        the_Events,
                        subject_Name.all);
            else
               if observer.Logger /= null
               then
                  observer.Logger.log (my_Name & " has no responses for events from " & subject_Name.all & ".");
               else
                  raise program_Error with my_Name & " has no responses for events from '" & subject_Name.all & "'.";
               end if;
            end if;

            free (subject_Name);
         end;
      end loop;

   end respond;


   protected
   body safe_Events
   is
      procedure add (the_Event : in Event.item'Class)
      is
      begin
         the_Events.append (the_Event);
      end add;


      procedure fetch (all_Events : out Event_Vector)
      is
      begin
         all_Events := the_Events;
         the_Events.clear;
      end fetch;
   end safe_Events;


   protected
   body safe_subject_Map_of_safe_events
   is
      procedure add (the_Event    : in Event.item'Class;
                     from_Subject : in String)
      is
      begin
         if not the_Map.contains (from_Subject)
         then
            the_Map.insert (from_Subject,
                            new safe_Events);
         end if;

         the_Map.Element (from_Subject).add (the_Event);
      end add;


      procedure fetch (all_Events : out subject_events_Pairs;
                       Count      : out Natural)
      is
         use subject_Maps_of_safe_events;

         Cursor : subject_Maps_of_safe_events.Cursor := the_Map.First;
         Index  : Natural := 0;

      begin
         while has_Element (Cursor)
         loop
            declare
               the_Events : Event_vector;
            begin
               Element (Cursor).fetch (the_Events);

               Index              := Index + 1;
               all_Events (Index) := (subject => new String' (Key (Cursor)),
                                      events  => the_Events);
            end;

            next (Cursor);
         end loop;

         Count := Index;
      end fetch;


      procedure free
      is
         use subject_Maps_of_safe_events;

         procedure deallocate is new ada.unchecked_Deallocation (safe_Events,
                                                                 safe_Events_view);

         Cursor     : subject_Maps_of_safe_events.Cursor := the_Map.First;
         the_Events : safe_Events_view;
      begin
         while has_Element (Cursor)
         loop
            the_Events := Element (Cursor);
            deallocate (the_Events);

            next (Cursor);
         end loop;
      end free;

   end safe_subject_Map_of_safe_events;


end lace.make_Observer.deferred;
