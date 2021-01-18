with
     lace.Event.Logger,
     lace.Event.utility,
     system.RPC,
     ada.unchecked_Deallocation;

package body lace.make_Subject
is

   procedure destroy (Self : in out Item)
   is
   begin
      Self.safe_Observers.destruct;
   end destroy;


   -- Attributes
   --

   overriding
   function Observers (Self : in Item;   of_Kind : in event.Kind) return subject.Observer_views
   is
   begin
      return Self.safe_Observers.fetch_Observers (of_Kind);
   end Observers;


   overriding
   function observer_Count (Self : in Item) return Natural
   is
   begin
      return Self.safe_Observers.observer_Count;
   end observer_Count;


   -- Operations
   --

   overriding
   procedure register (Self : access Item;   the_Observer : in Observer.view;
                                             of_Kind      : in event.Kind)
   is
   begin
      Self.safe_Observers.add (the_Observer, of_Kind);

      if subject.Logger /= null
      then
         subject.Logger.log_Connection (the_Observer,
                                        Subject.view (Self),
                                        of_Kind);
      end if;
   end register;


   overriding
   procedure deregister (Self : in out Item;   the_Observer : in Observer.view;
                                               of_Kind      : in event.Kind)
   is
   begin
      Self.safe_Observers.rid (the_Observer, of_Kind);

      if subject.Logger /= null
      then
         subject.Logger.log_disconnection (the_Observer,
                                           Self'unchecked_Access,
                                           of_Kind);
      end if;
   end deregister;


   overriding
   procedure emit (Self : access Item;   the_Event : in Event.item'Class := event.null_Event)
   is
      use lace.Event.utility;
      my_Observers : constant subject.Observer_views := Self.Observers (to_Kind (the_Event'Tag));
   begin
      for Each in my_Observers'Range
      loop
         begin
            my_Observers (Each).receive (the_Event,
                                         from_subject => Subject.item'Class (Self.all).Name);
            if subject.Logger /= null
            then
               subject.Logger.log_Emit (Subject.view (Self),
                                        my_Observers (Each),
                                        the_Event);
            end if;

         exception
            when system.RPC.communication_Error
               | storage_Error =>
               if subject.Logger /= null
               then
                  subject.Logger.log_Emit (Subject.view (Self),
                                           my_Observers (Each),
                                           the_Event);
               end if;
         end;
      end loop;
   end emit;


   overriding
   function emit (Self : access Item;   the_Event : in Event.item'Class := event.null_Event)
                  return subject.Observer_views
   is
      use lace.Event.utility;
      my_Observers  : constant subject.Observer_views := Self.Observers (to_Kind (the_Event'Tag));
      bad_Observers :          subject.Observer_views (my_Observers'Range);
      bad_Count     :          Natural := 0;
   begin
      for Each in my_Observers'Range
      loop
         begin
            my_Observers (Each).receive (the_Event,
                                         from_subject => Subject.item'Class (Self.all).Name);
            if subject.Logger /= null
            then
               subject.Logger.log_Emit (Subject.view (Self),
                                        my_Observers (Each),
                                        the_Event);
            end if;

         exception
            when system.RPC.communication_Error
               | storage_Error =>
               bad_Count                 := bad_Count + 1;
               bad_Observers (bad_Count) := my_Observers (Each);
         end;
      end loop;

      return bad_Observers (1 .. bad_Count);
   end emit;


   -- Safe Observers
   --

   protected
   body safe_Observers
   is
      procedure destruct
      is
         use event_kind_Maps_of_event_observers;

         procedure free is new ada.unchecked_Deallocation (event_Observer_Vector,
                                                           event_Observer_Vector_view);

         Cursor                    : event_kind_Maps_of_event_observers.Cursor := the_Observers.First;
         the_event_Observer_Vector : event_Observer_Vector_view;
      begin
         while has_Element (Cursor)
         loop
            the_event_Observer_Vector := Element (Cursor);
            free (the_event_Observer_Vector);

            next (Cursor);
         end loop;
      end destruct;


      procedure add (the_Observer : in Observer.view;
                     of_Kind      : in event.Kind)
      is
         use event_Observer_Vectors,
             event_kind_Maps_of_event_observers;

         Cursor              : constant event_kind_Maps_of_event_observers.Cursor := the_Observers.find (of_Kind);
         the_event_Observers :          event_Observer_Vector_view;
      begin
         if has_Element (Cursor)
         then
            the_event_Observers := Element (Cursor);
         else
            the_event_Observers := new event_Observer_Vector;
            the_Observers.insert (of_Kind,
                                  the_event_Observers);
         end if;

         the_event_Observers.append (the_Observer);
      end add;


      procedure rid (the_Observer : in Observer.view;
                     of_Kind      : in event.Kind)
      is
         the_event_Observers : event_Observer_Vector renames the_Observers.Element (of_Kind).all;
      begin
         the_event_Observers.delete (the_event_Observers.find_Index (the_Observer));
      end rid;


      function fetch_Observers (of_Kind : in event.Kind) return subject.Observer_views
      is
      begin
         if the_Observers.Contains (of_Kind)
         then
            declare
               the_event_Observers : constant event_Observer_Vector_view := the_Observers.Element (of_Kind);
               my_Observers        :          subject.Observer_views (1 .. Natural (the_event_Observers.Length));
            begin
               for Each in my_Observers'Range
               loop
                  my_Observers (Each) := the_event_Observers.Element (Each);
               end loop;

               return my_Observers;
            end;
         else
            return (1 .. 0 => <>);
         end if;
      end fetch_Observers;


      function observer_Count return Natural
      is
         use event_kind_Maps_of_event_observers;

         Cursor : event_kind_Maps_of_event_observers.Cursor := the_Observers.First;
         Count  : Natural := 0;
      begin
         while has_Element (Cursor)
         loop
            Count := Count + Natural (Element (Cursor).Length);
            next (Cursor);
         end loop;

         return Count;
      end observer_Count;

   end safe_Observers;


end lace.make_Subject;
