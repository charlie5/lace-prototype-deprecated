pragma Profile (Ravenscar);


with lace.Event,
     lace.Response,
     lace.Subject,
     lace.Observer;

private
with ada.Containers.indefinite_Vectors,
     ada.Containers.indefinite_hashed_Maps,
     ada.Strings.hash;



generic
   type T is abstract new lace.make_Observer.item with private;

package lace.make_Observer.deferred
--
-- Makes a user class T into a deferred event Observer.
--
is
--     pragma remote_Types;


   ------------------
   --- Observer Item
   --

   type Item is abstract limited new T with private;

   type View is access all Item'Class;

   --   pragma Asynchronous (View);        -- tbd: Needed for lossy events.





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



   --- subject_Maps_of_safe_events
   --

   use type event_Vector;
   package subject_Maps_of_safe_events is new ada.containers.indefinite_hashed_Maps (key_type        => String,             -- Subject Name,
                                                                                     element_type    => event_Vector_view,
                                                                                     hash            => Ada.Strings.Hash,
                                                                                     equivalent_keys => "=");
   subtype subject_Map_of_safe_events  is subject_Maps_of_safe_events.Map;




   ------------------
   --- Observer Item
   --

   type Item is abstract limited new T with
      record
         pending_Events : subject_Map_of_safe_events;
      end record;


end lace.make_Observer.deferred;
