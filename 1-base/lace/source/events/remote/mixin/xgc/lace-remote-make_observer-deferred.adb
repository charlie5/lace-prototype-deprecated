with lace.event.remote.Logger,
     lace.event_Conversions;
with ada.unchecked_Deallocation;
--  with Ada.Text_IO; use Ada.Text_IO;


package body lace.remote.make_Observer.deferred
is


   overriding
   procedure destroy (Self : in out Item)
   is
      use subject_Maps_of_safe_events;

      procedure free is new ada.unchecked_Deallocation (safe_Events,
                                                        safe_Events_view);

      Cursor     : subject_Maps_of_safe_events.Cursor := Self.pending_Events.First;
      the_Events : safe_Events_view;
   begin
      make_Observer.destroy (make_Observer.item (Self));   -- Destroy base class.

      while has_Element (Cursor)
      loop
         the_Events := Element (Cursor);
         free (the_Events);
--           put_Line ("KKK 555555555555");

         next (Cursor);
      end loop;

      Self.pending_Events.clear;
   end destroy;





   overriding
   procedure receive (Self : access Item;   the_Event    : in Event.item'Class := event.null_Event;
                                            from_Subject : in String)
   is
   begin
      if not Self.pending_Events.contains (from_Subject) then
         Self.pending_Events.insert (from_Subject,
                                     new safe_Events);
      end if;

      Self.pending_Events.Element (from_Subject).add (the_Event);
   end receive;





   overriding
   procedure respond (Self : access Item)
   is
      use event_Vectors;

      my_Name : constant String := Observer.Item'Class (Self.all).Name;


      --- actuate
      --
      procedure actuate (the_Responses     : in     event_response_Map;
                         the_Events        : in     event_Vector;
                         from_Subject_Name : in     String)
      is
         Cursor : event_Vectors.Cursor;

      begin
         Cursor := the_Events.First;

         while has_Element (Cursor) loop
            declare
               use event_response_Maps,  event_Conversions,  Ada.Containers;
               use type Observer.view;

               the_Event : constant Event.item'Class           := Element (Cursor);
               Response  : constant event_response_Maps.Cursor := the_Responses.find (to_event_Kind (the_Event'Tag));

            begin
               if has_Element (Response) then
                  Element (Response).respond (the_Event);

                  if Observer.Logger /= null then
                     Observer.Logger.log_Response (Element (Response),
                                                   Observer.view (Self),
                                                   the_Event,
                                                   from_Subject_Name);
                  end if;


               elsif Self.relay_Target /= null then
                  --  Self.relay_Target.notify (the_Event, from_Subject_Name);

                  if Observer.Logger /= null then
                     Observer.Logger.log ("[Warning] ~ Relayed events are currently disabled.");
                  else
                     raise Program_Error with   "Event relaying is currently disabled";
                  end if;


               else
                  if Observer.Logger /= null then
                     Observer.Logger.log ("[Warning] ~ Observer "
                                          & my_Name
                                          & " has no response !");
                     Observer.Logger.log ("            count of responses =>"
                                          & Count_type'Image (the_Responses.Length));
                  else
                     raise Program_Error with   "Observer " & my_Name & " has no response !";
                  end if;
               end if;

            end;


            next (Cursor);
         end loop;

      end actuate;


      use subject_Maps_of_safe_events;
      Cursor : subject_Maps_of_safe_events.Cursor := Self.pending_Events.First;

   begin

      while has_Element (Cursor)
      loop
         declare
            use subject_Maps_of_event_responses;

            subject_Name : constant String      := Key (Cursor);
            the_Events   : event_Vector;

         begin
            Self.pending_Events.Element (subject_Name).fetch (the_Events);

            if Self.subject_Responses.contains (subject_Name) then
               actuate (Self.subject_Responses.Element (subject_Name).all,
                        the_Events, -- Self.pending_Events   .Element (subject_Name).all,
                        subject_Name);
            else
               if Observer.Logger /= null then
                  Observer.Logger.log (my_Name & " has no responses for events from " & subject_Name);
               else
                  raise Program_Error with    my_Name & " has no responses for events from '" & subject_Name & "'";
               end if;
            end if;
         end;


         next (Cursor);
      end loop;

   end respond;





   protected
   body safe_Events
   is

      procedure add (the_Event : in Event.Item'Class)
      is
      begin
         the_Events.append (the_Event);
      end add;


      procedure fetch (all_Events : out event_Vector)
      is
      begin
         all_Events := the_Events;
         the_Events.clear;
      end fetch;

   end safe_Events;


end lace.remote.make_Observer.deferred;
