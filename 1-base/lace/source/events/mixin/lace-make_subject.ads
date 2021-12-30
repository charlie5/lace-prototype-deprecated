with
     lace.Event,
     lace.Subject,
     lace.Observer;

private
with
     ada.Containers.Vectors,
     ada.Containers.indefinite_hashed_Maps;


generic
   type T is abstract tagged limited private;

package lace.make_Subject
--
--  Makes a user class T into an event Subject.
--
is
   pragma remote_Types;

   type Item is abstract limited new T
                                 and Subject.item with private;
   type View is access all Item'Class;

   procedure destroy (Self : in out Item);


   -------------
   -- Attributes
   --

   overriding
   function Observers      (Self : in Item;   of_Kind : in Event.Kind) return Subject.Observer_views;
   overriding
   function observer_Count (Self : in Item) return Natural;


   -------------
   -- Operations
   --

   overriding
   procedure   register (Self : access Item;   the_Observer : in Observer.view;
                                               of_Kind      : in Event.Kind);
   overriding
   procedure deregister (Self : in out Item;   the_Observer : in Observer.view;
                                               of_Kind      : in Event.Kind);

   overriding
   procedure emit (Self : access Item;   the_Event : in Event.item'Class := Event.null_Event);

   overriding
   function  emit (Self : access Item;   the_Event : in Event.item'Class := Event.null_Event)
                   return subject.Observer_views;



private

   -------------------------
   -- Event observer vectors
   --
   use type Observer.view;

   package event_Observer_Vectors     is new ada.Containers.Vectors (Positive, Observer.view);
   subtype event_Observer_Vector      is event_Observer_Vectors.Vector;
   type    event_Observer_Vector_view is access all event_Observer_Vector;


   -------------------------------------
   -- Event kind Maps of event observers
   --
   use type Event.Kind;
   package event_kind_Maps_of_event_observers is new ada.Containers.indefinite_hashed_Maps (Event.Kind,
                                                                                            event_Observer_Vector_view,
                                                                                            Event.Hash,
                                                                                            "=");
   subtype event_kind_Map_of_event_observers  is event_kind_Maps_of_event_observers.Map;


   -----------------
   -- Safe observers
   --
   protected
   type safe_Observers
   is
      procedure destruct;

      procedure add (the_Observer : in Observer.view;
                     of_Kind      : in Event.Kind);

      procedure rid (the_Observer : in Observer.view;
                     of_Kind      : in Event.Kind);

      function  fetch_Observers (of_Kind : in Event.Kind) return Subject.Observer_views;
      function  observer_Count return Natural;

   private
      the_Observers : event_kind_Map_of_event_observers;
   end safe_Observers;


   ---------------
   -- Subject Item
   --
   type Item is abstract limited new T
                                 and Subject.item
   with
      record
         safe_Observers : make_Subject.safe_Observers;
      end record;

end lace.make_Subject;
