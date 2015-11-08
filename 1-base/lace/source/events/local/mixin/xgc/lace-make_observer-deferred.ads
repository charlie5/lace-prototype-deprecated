with lace.Event;
--       lace.Response,
--       lace.Subject,
--       lace.Observer;

private
with ada.Containers.indefinite_Vectors,
     ada.Containers.indefinite_hashed_Maps,
     ada.Strings.hash;



generic
   type T is abstract new lace.make_Observer.item with private;

package lace.make_Observer.deferred
--
--  Makes a user class T into a deferred event Observer.
--
is
   --     pragma remote_Types;


   ------------------
   --- Observer Item
   --

   type Item is abstract limited new T with private;

   type View is access all Item'Class;

   --   pragma Asynchronous (View);        -- tbd: Needed for lossy events.


   overriding
   procedure destroy (Self : in out Item);


   ---------------
   --- Operations
   --


   overriding
   procedure receive (Self : access Item;   the_Event    : in Event.item'Class := event.null_Event;
                                            from_Subject : in String);

   overriding
   procedure respond (Self : access Item);






private

   --- event_Vectors
   --

   use type Event.item;

   package event_Vectors     is new ada.containers.indefinite_Vectors (Positive,  Event.item'Class);
   subtype event_Vector      is event_vectors.Vector;
   type    event_Vector_view is access all event_Vector;



   --- safe_Events
   --

   protected
   type safe_Events is

      procedure add   (the_Event  : in     Event.Item'Class);
      procedure fetch (all_Events :    out event_Vector);

   private
      the_Events : event_Vector;
   end safe_Events;


   type safe_Events_view is access all safe_Events;




   --- subject_Maps_of_safe_events
   --

   use type event_Vector;
   package subject_Maps_of_safe_events is new ada.containers.indefinite_hashed_Maps (key_type        => String,   -- Subject Name,
                                                                                     element_type    => safe_Events_view,
                                                                                     hash            => Ada.Strings.Hash,
                                                                                     equivalent_keys => "=");
   subtype subject_Map_of_safe_events  is subject_Maps_of_safe_events.Map;

   type String_view is access all String;


   type subject_events_Pair is
      record
         Subject : String_view;
         Events  : event_Vector;
      end record;

   type subject_events_Pairs is array (Positive range <>) of subject_events_Pair;

   protected
   type safe_subject_Map_of_safe_events is

      procedure add   (the_Event  : in     Event.Item'Class;      from_Subject : in     String);
      procedure fetch (all_Events :    out subject_events_Pairs;  Count        :    out Natural);
      procedure free;

   private
      the_Map : subject_Map_of_safe_events;
   end safe_subject_Map_of_safe_events;


   ------------------
   --- Observer Item
   --

   type Item is abstract limited new T with
      record
         pending_Events : safe_subject_Map_of_safe_events; -- subject_Map_of_safe_events;
      end record;


end lace.make_Observer.deferred;
