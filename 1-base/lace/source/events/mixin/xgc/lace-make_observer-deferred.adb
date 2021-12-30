with
     lace.Event.Logger,
     lace.Event.utility,
     ada.unchecked_Deallocation;


package body lace.make_Observer.deferred
is
   use type Event.Logger.view;


   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      make_Observer.destroy (make_Observer.item (Self));   -- Destroy base class.
      Self.pending_Events.free;
   end destroy;


   -------------
   -- Operations
   --

   overriding
   procedure receive (Self : access Item;   the_Event    : in Event.item'Class := Event.null_Event;
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

                  if Observer.Logger /= null
                  then
                     Observer.Logger.log_Response (Element (Response),
                                                   Observer.view (Self),
                                                   the_Event,
                                                   from_subject_Name);
                  end if;

               elsif Self.Responses.relay_Target /= null
               then
                  --  Self.relay_Target.notify (the_Event, from_Subject_Name);   -- todo: Re-enable relayed events.

                  if Observer.Logger /= null
                  then
                     Observer.Logger.log ("[Warning] ~ Relayed events are currently disabled.");
                  else
                     raise program_Error with "Event relaying is currently disabled.";
                  end if;

               else
                  if Observer.Logger /= null
                  then
                     Observer.Logger.log ("[Warning] ~ Observer "
                                          & my_Name
                                          & " has no response to " & Name_of (the_Event)
                                          & " from " & from_subject_Name & ".");
                     Observer.Logger.log ("            Count of responses =>"
                                          & the_Responses.Length'Image);
                  else
                     raise program_Error with "Observer " & my_Name & " has no response to " & Name_of (the_Event)
                                            & " from " & from_subject_Name & ".";
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
            procedure deallocate is new ada.unchecked_Deallocation (String, String_view);

            subject_Name : String_view       := the_subject_Events (i).Subject;
            the_Events   : Event_vector renames the_subject_Events (i).Events;
         begin
            if Self.Responses.Contains (subject_Name.all)
            then
               actuate (Self.Responses.Element (subject_Name.all),
                        the_Events,
                        subject_Name.all);
            else
               declare
                  Message : constant String := my_Name & " has no responses for events from " & subject_Name.all & ".";
               begin
                  if Observer.Logger /= null
                  then
                     Observer.Logger.log (Message);
                  else
                     raise program_Error with Message;
                  end if;
               end;
            end if;

            deallocate (subject_Name);
         end;
      end loop;

   end respond;


   --------------
   -- Safe Events
   --
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


   ----------------------------------
   -- safe Subject Map of safe Events
   --
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
