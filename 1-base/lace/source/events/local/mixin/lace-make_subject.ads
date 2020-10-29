with lace.Event,
     lace.Subject,
     lace.Observer;

private
with ada.containers.Vectors,
     ada.containers.indefinite_hashed_Maps,
     ada.unchecked_Conversion;


generic
   type T is abstract tagged limited private;

package lace.make_Subject
--
--  Makes a user class T into an event Subject.
--
is
   type Item is abstract limited new T
                                 and Subject.item with private;
   type View is access all Item'Class;

   procedure destroy (Self : in out Item);


   -- Attributes
   --

   overriding
   function  Observers      (Self : in Item;   of_Kind : in event.Kind) return subject.Observer_views;
   overriding
   function  observer_Count (Self : in Item) return Natural;

   -- Operations
   --

   overriding
   procedure   register (Self : access Item;   the_Observer  : in Observer.view;
                                               of_Kind       : in event.Kind);
   overriding
   procedure deregister (Self : in out Item;   the_Observer  : in Observer.view;
                                               of_Kind       : in event.Kind);
   overriding
   procedure emit       (Self : access Item;   the_Event     : in Event.item'Class := event.null_Event);



private
   -- event_Observer_vectors
   --
   use type Observer.view;

   package event_Observer_vectors     is new ada.containers.Vectors (Positive, Observer.view);
   subtype event_Observer_vector      is event_Observer_vectors.Vector;
   type    event_Observer_vector_view is access all event_Observer_vector;

   -- event_kind_Maps_of_event_observers
   --
   use type event.Kind;

   package event_kind_Maps_of_event_observers is new ada.containers.indefinite_hashed_Maps (event.Kind,
                                                                                            event_Observer_Vector_view,
                                                                                            event.Hash,
                                                                                            "=");
   subtype event_kind_Map_of_event_observers  is event_kind_Maps_of_event_observers.Map;

   -- Safe Observers
   --

   protected
   type safe_Observers
   is
      procedure destruct;

      procedure add (the_Observer : in Observer.view;
                     of_Kind      : in event.Kind);

      procedure rid (the_Observer : in Observer.view;
                     of_Kind      : in event.Kind);

      function fetch_Observers (of_Kind : in event.Kind) return subject.Observer_views;
      function observer_Count return Natural;

   private
      the_Observers : event_kind_Map_of_event_observers;
   end safe_Observers;


   -- Subject Item
   --

   type Item is abstract limited new T
                                 and Subject.item
   with
      record
         safe_Observers : make_Subject.safe_Observers;
      end record;

end lace.make_Subject;
