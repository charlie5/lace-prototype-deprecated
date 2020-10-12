with
     lace.remote.Event.Logger,
     lace.Event_conversions,

     ada.unchecked_Conversion,
     ada.unchecked_Deallocation;

package body lace.remote.make_Observer
is

   procedure destroy (Self : in out Item)
   is
      use subject_Maps_of_event_responses;

      procedure free is new ada.unchecked_Deallocation (event_response_Map,
                                                        event_response_Map_view);

      Cursor  : subject_Maps_of_event_responses.Cursor := Self.subject_Responses.First;
      the_Map : event_response_Map_view;
   begin
      while has_Element (Cursor)
      loop
         the_Map := Element (Cursor);
         free (the_Map);

         next (Cursor);
      end loop;
   end destroy;


   overriding
   procedure receive (Self : access Item;   the_Event    : in lace.Event.item'Class := lace.event.null_Event;
                      from_Subject : in String)
   is
      use event_response_Maps,
          subject_Maps_of_event_responses,
          lace.Event_conversions,
          ada.Containers;

      use type lace.remote.Observer.view;

      the_Responses :          event_response_Map    renames Self.subject_Responses.Element (from_Subject).all;
      the_Response  : constant event_response_Maps.Cursor := the_Responses.find (to_event_Kind (the_Event'Tag));

      my_Name       : constant String := Observer.item'Class (Self.all).Name;
   begin
      if has_Element (the_Response)
      then
         Element (the_Response).respond (the_Event);

         if observer.Logger /= null
         then
            observer.Logger.log_Response (Element (the_Response),
                                          Observer.view (Self),
                                          the_Event,
                                          from_Subject);
         end if;

      elsif Self.relay_Target /= null
      then
         --  Self.relay_Target.notify (the_Event, from_Subject_Name);   -- tbd: Re-enable event relays.

         if observer.Logger /= null
         then
            observer.Logger.log ("[Warning] ~ Relayed events are currently disabled.");
         else
            raise Program_Error with "Event relaying is currently disabled";
         end if;

      else
         if observer.Logger /= null
         then
            observer.Logger.log ("[Warning] ~ Observer " & my_Name & " has no response.");
            observer.Logger.log ("            count of responses =>" & Count_type'Image (the_Responses.Length));
         else
            raise Program_Error with "Observer " & my_Name & " has no response.";
         end if;
      end if;

   exception
      when constraint_Error =>
         if Observer.Logger /= null
         then
            observer.Logger.log (my_Name & " has no responses for events from " & from_Subject);
         else
            raise Program_Error with my_Name & " has no responses for events from " & from_Subject;
         end if;
   end receive;


   overriding
   procedure add (Self : access Item;   the_Response : in Response.view;
                                        to_Kind      : in lace.event.Kind;
                                        from_Subject : in String)
   is
   begin
      if not Self.subject_Responses.contains (from_Subject)
      then
         Self.subject_Responses.insert (from_Subject,
                                        new event_response_Map);
      end if;

      Self.subject_Responses.Element (from_Subject).insert (to_Kind,
                                                            the_Response);
      if observer.Logger /= null
      then
         observer.Logger.log_new_Response (the_Response,
                                           Observer.item'Class (Self.all),
                                           to_Kind,
                                           from_Subject);
      end if;
   end add;


   overriding
   procedure rid (Self : access Item;   the_Response : in Response.view;
                                        to_Kind      : in lace.event.Kind;
                                        from_Subject : in String)
   is
   begin
      Self.subject_Responses.Element (from_Subject).delete (to_Kind);

      if observer.Logger /= null
      then
         observer.Logger.log_rid_Response (the_Response,
                                           Observer.item'Class (Self.all),
                                           to_Kind,
                                           from_Subject);
      end if;
   end rid;


   overriding
   procedure respond (Self : access Item)
   is
   begin
      null;   -- This is a null operation since there can never be any deferred events for an 'instant' observer.
   end respond;


   overriding
   procedure relay_responseless_Events (Self : in out Item;   To : in Observer.view)
   is
   begin
      Self.relay_Target := To;
   end relay_responseless_Events;


   function to_Hash (Self : in lace.event.Kind) return ada.containers.Hash_type
   is
      function Converted is new ada.unchecked_Conversion (lace.event.Kind, ada.containers.Hash_type);
   begin
      return Converted (Self);
   end to_Hash;

end lace.remote.make_Observer;
