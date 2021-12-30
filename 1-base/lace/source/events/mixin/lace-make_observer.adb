with
     lace.Event.Logger,
     lace.Event.utility,

     ada.unchecked_Conversion,
     ada.unchecked_Deallocation;


package body lace.make_Observer
is
   use type Event.Logger.view;


   procedure destroy (Self : in out Item)
   is
   begin
      Self.Responses.destroy;
   end destroy;


   ------------
   -- Responses
   --

   overriding
   procedure add (Self : access Item;   the_Response : in Response.view;
                                        to_Kind      : in Event.Kind;
                                        from_Subject : in Event.subject_Name)
   is
   begin
      Self.Responses.add (Self, the_Response, to_Kind, from_Subject);
   end add;


   overriding
   procedure rid (Self : access Item;   the_Response : in Response.view;
                                        to_Kind      : in Event.Kind;
                                        from_Subject : in Event.subject_Name)
   is
   begin
      Self.Responses.rid (Self, the_Response, to_Kind, from_Subject);
   end rid;


   overriding
   procedure relay_responseless_Events (Self : in out Item;   To : in Observer.view)
   is
   begin
      Self.Responses.relay_responseless_Events (To);
   end relay_responseless_Events;


   -------------
   -- Operations
   --

   overriding
   procedure receive (Self : access Item;   the_Event    : in Event.item'Class := Event.null_Event;
                                            from_Subject : in Event.subject_Name)
   is
   begin
      Self.Responses.receive (Self, the_Event, from_Subject);
   end receive;


   overriding
   procedure respond (Self : access Item)
   is
   begin
      null;   -- This is a null operation since there can never be any deferred events for an 'instant' observer.
   end respond;


   -----------------
   -- Safe Responses
   --
   protected
   body safe_Responses
   is
      procedure destroy
      is
         use subject_Maps_of_event_responses;

         procedure free is new ada.unchecked_Deallocation (event_response_Map,
                                                           event_response_Map_view);

         Cursor  : subject_Maps_of_event_responses.Cursor := my_Responses.First;
         the_Map : event_response_Map_view;
      begin
         while has_Element (Cursor)
         loop
            the_Map := Element (Cursor);
            free (the_Map);

            next (Cursor);
         end loop;
      end destroy;


      ------------
      -- Responses
      --

      procedure add (Self         : access Item'Class;
                     the_Response : in     Response.view;
                     to_Kind      : in     Event.Kind;
                     from_Subject : in     Event.subject_Name)
      is
      begin
         if not my_Responses.contains (from_Subject)
         then
            my_Responses.insert (from_Subject,
                                 new event_response_Map);
         end if;

         my_Responses.Element (from_Subject).insert (to_Kind,
                                                     the_Response);
         if Observer.Logger /= null
         then
            Observer.Logger.log_new_Response (the_Response,
                                              Observer.item'Class (Self.all),
                                              to_Kind,
                                              from_Subject);
         end if;
      end add;


      procedure rid (Self         : access Item'Class;
                     the_Response : in     Response.view;
                     to_Kind      : in     Event.Kind;
                     from_Subject : in     Event.subject_Name)
      is
      begin
         my_Responses.Element (from_Subject).delete (to_Kind);

         if Observer.Logger /= null
         then
            Observer.Logger.log_rid_Response (the_Response,
                                              Observer.item'Class (Self.all),
                                              to_Kind,
                                              from_Subject);
         end if;
      end rid;


      procedure relay_responseless_Events (To : in Observer.view)
      is
      begin
         my_relay_Target := To;
      end relay_responseless_Events;


      function relay_Target return Observer.view
      is
      begin
         return my_relay_Target;
      end relay_Target;


      function Contains (Subject : in Event.subject_Name) return Boolean
      is
      begin
         return my_Responses.Contains (Subject);
      end Contains;


      function Element (Subject : in Event.subject_Name) return event_response_Map
      is
      begin
         return my_Responses.Element (Subject).all;
      end Element;


      -------------
      -- Operations
      --

      procedure receive (Self         : access Item'Class;
                         the_Event    : in     Event.item'Class := Event.null_Event;
                         from_Subject : in     Event.subject_Name)
      is
         use event_response_Maps,
             subject_Maps_of_event_responses,
             lace.Event.utility,
             ada.Containers;

         use type lace.Observer.view;

         the_Responses :          event_response_Map    renames my_Responses.Element (from_Subject).all;
         the_Response  : constant event_response_Maps.Cursor := the_Responses.find (to_Kind (the_Event'Tag));

         my_Name : constant String := Observer.item'Class (Self.all).Name;

      begin
         if has_Element (the_Response)
         then
            Element (the_Response).respond (the_Event);

            if Observer.Logger /= null
            then
               Observer.Logger.log_Response (Element (the_Response),
                                             Observer.view (Self),
                                             the_Event,
                                             from_Subject);
            end if;

         elsif relay_Target /= null
         then
            --  Self.relay_Target.notify (the_Event, from_Subject_Name);   -- todo: Re-enable event relays.

            if Observer.Logger /= null
            then
               Observer.Logger.log ("[Warning] ~ Relayed events are currently disabled.");
            else
               raise program_Error with "Event relaying is currently disabled.";
            end if;

         else
            if Observer.Logger /= null
            then
               Observer.Logger.log ("[Warning] ~ Observer " & my_Name & " has no response to " & Name_of (the_Event)
                                    & " from " & from_Subject & ".");
               Observer.Logger.log ("            count of responses =>" & the_Responses.Length'Image);
            else
               raise program_Error with "Observer " & my_Name & " has no response to " & Name_of (the_Event)
                                      & " from " & from_Subject & ".";
            end if;
         end if;

      exception
         when constraint_Error =>
            if Observer.Logger /= null
            then
               Observer.Logger.log (my_Name & " has no responses for events from " & from_Subject & ".");
            else
               raise Program_Error with my_Name & " has no responses for events from " & from_Subject & ".";
            end if;
      end receive;

   end safe_Responses;


end lace.make_Observer;
