with
     lace.Event;

private
with
     ada.containers.indefinite_Vectors,
     ada.containers.indefinite_hashed_Maps,
     ada.strings.Hash;

generic
   type T is abstract new lace.remote.make_Observer.item with private;

package lace.remote.make_Observer.deferred
--
--  Makes a user class T into a remote deferred event Observer.
--
is
   pragma remote_Types;


   type Item is abstract limited new T with private;
   type View is access all Item'Class;

   --   pragma Asynchronous (View);        -- tbd: Needed for lossy events.

   overriding
   procedure destroy (Self : in out Item);


   --- Operations
   --

   overriding
   procedure receive (Self : access Item;   the_Event    : in lace.Event.item'Class := event.null_Event;
                                            from_Subject : in String);
   overriding
   procedure respond (Self : access Item);



private

   --- event Vectors
   --

   use type Event.item;

   package event_Vectors     is new ada.containers.indefinite_Vectors (Positive, lace.Event.item'Class);
   subtype event_Vector      is event_vectors.Vector;
   type    event_Vector_view is access all event_Vector;


   --- safe Events
   --

   protected
   type safe_Events
   is
      procedure add   (the_Event  : in     lace.Event.item'Class);
      procedure fetch (all_Events :    out event_Vector);

   private
      the_Events : event_Vector;
   end safe_Events;

   type safe_Events_view is access all safe_Events;


   --- subject Maps of safe events
   --

   use type event_Vector;
   package subject_Maps_of_safe_events
   is new ada.containers.indefinite_hashed_Maps (key_type        => String,   -- Subject name.
                                                 element_type    => safe_Events_view,
                                                 hash            => Ada.Strings.Hash,
                                                 equivalent_keys => "=");
   subtype subject_Map_of_safe_events is subject_Maps_of_safe_events.Map;


   --- Observer Item
   --

   type Item is abstract limited new T with
      record
         pending_Events : subject_Map_of_safe_events;
      end record;

end lace.remote.make_Observer.deferred;
