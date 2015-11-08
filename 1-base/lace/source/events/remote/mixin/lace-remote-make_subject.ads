with
     lace.Event,
     lace.remote.Subject,
     lace.remote.Observer;

private
with ada.containers.Vectors,
     ada.containers.hashed_Maps,
     ada.unchecked_Conversion;



generic
   type T is abstract tagged limited private;

package lace.remote.make_Subject
--
--  Makes a user class T into an event Subject.
--
is
--     pragma remote_Types;



   -----------------
   --- Subject Item
   --

   type Item is abstract limited new T
                                 and Subject.item with private;

   type View is access all Item'Class;

   procedure destroy (Self : in out Item);




   ---------------
   --- Attributes
   --

   overriding
   procedure register   (Self : access Item;   the_Observer  : in Observer.view;
                                               of_Kind       : in event.Kind);

   overriding
   procedure deregister (Self : in out Item;   the_Observer  : in Observer.view;
                                               of_Kind       : in event.Kind);


   overriding
   function  Observers      (Self : in Item;   of_Kind : in event.Kind) return subject.Observer_views;
   overriding
   function  observer_Count (Self : in Item) return Natural;





   ---------------
   --- Operations
   --

   overriding procedure emit (Self : access Item;   the_Event : in Event.item'Class := event.null_Event);





private

   --- event_Observer_Vectors
   --

   use type Observer.view;

   package event_Observer_Vectors     is new ada.Containers.Vectors (Positive,  Observer.view);
   subtype event_observer_Vector      is event_Observer_Vectors.Vector;
   type    event_observer_Vector_view is access all event_Observer_Vector;




   --- event_kind_Maps_of_event_observers
   --

   function to_Hash is new ada.unchecked_Conversion (event.Kind,  ada.Containers.Hash_type);
   use type event.Kind;

   package event_kind_Maps_of_event_observers is new ada.Containers.hashed_Maps (event.Kind,
                                                                                 event_observer_Vector_view,
                                                                                 to_Hash,
                                                                                 "=");
   subtype event_kind_Map_of_event_observers  is event_kind_Maps_of_event_observers.Map;




   --- safe_Observers
   --

   protected
   type safe_Observers
   is
      procedure destruct;

      procedure add (the_Observer : in Observer.view;
                     of_Kind      : in event.Kind);

      procedure rid (the_Observer : in Observer.view;
                     of_Kind      : in event.Kind);


      function  fetch_Observers (of_Kind : in event.Kind) return subject.Observer_views;

      function  observer_Count return Natural;

   private
      the_Observers : event_kind_Map_of_event_observers;
   end safe_Observers;




   -----------------
   --- Subject Item
   --

   type Item is abstract limited new T
                                 and Subject.item
   with
      record
         safe_Observers : make_Subject.safe_Observers;
      end record;

end lace.remote.make_Subject;
