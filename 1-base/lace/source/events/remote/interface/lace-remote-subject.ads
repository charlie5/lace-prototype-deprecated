with lace.Event,
     lace.remote.Observer;

limited
with lace.event.remote.Logger;



package lace.remote.Subject
--
--  Provides an interface for an event Subject.
--
is
   pragma Remote_Types;



   type Item  is limited interface;
   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;

   --     pragma Asynchronous (View);





   ---------------
   --- Containers
   --

   type Observer_views is array (Positive range <>) of Observer.view;






   ---------------
   --- Attributes
   --


   --- Name
   --
   function  Name           (Self : in Item) return String   is abstract;



   --- Observers
   --

   procedure register       (Self : access Item;   the_Observer : in Observer.view;
                                                   of_Kind      : in event.Kind)   is abstract;

   procedure deregister     (Self : in out Item;   the_Observer : in Observer.view;
                                                   of_Kind      : in event.Kind)   is abstract;

   function  Observers      (Self : in     Item;   of_Kind : in event.Kind) return Observer_views   is abstract;
   function  observer_Count (Self : in     Item)                            return Natural          is abstract;






   ---------------
   --- Operations
   --

   procedure emit (Self : access Item;   the_Event : in Event.item'Class := event.null_Event)   is abstract;






   ------------
   --- Logging
   --

   procedure Logger_is (Now : access event.remote.Logger.item'Class);
   function  Logger    return access event.remote.Logger.item'Class;


end lace.remote.Subject;
