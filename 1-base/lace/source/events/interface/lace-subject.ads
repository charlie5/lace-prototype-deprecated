with
     lace.Event,
     lace.Observer;

limited
with
     lace.Event.Logger;


package lace.Subject
--
--  Provides an interface for an event subject.
--
is
   pragma remote_Types;

   type Item  is limited interface;
   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;

   type fast_View  is access all Item'Class with Asynchronous;
   type fast_Views is array (Positive range <>) of fast_View;


   -------------
   -- Containers
   --

   type Observer_views is array (Positive range <>) of Observer.view;


   -------------
   -- Attributes
   --

   function Name (Self : in Item) return Event.subject_Name is abstract;


   ------------
   -- Observers
   --

   procedure register   (Self : access Item;   the_Observer : in Observer.view;
                                               of_Kind      : in Event.Kind) is abstract;

   procedure deregister (Self : in out Item;   the_Observer : in Observer.view;
                                               of_Kind      : in Event.Kind) is abstract;

   function  Observers      (Self : in Item;   of_Kind : in Event.Kind) return Observer_views is abstract;
   function  observer_Count (Self : in Item)                            return Natural        is abstract;


   -------------
   -- Operations
   --

   procedure emit (Self : access Item;   the_Event : in Event.item'Class := Event.null_Event) is abstract;
   --
   -- Communication errors are ignored.

   function  emit (Self : access Item;   the_Event : in Event.item'Class := Event.null_Event)
                   return Observer_views is abstract;
   --
   -- Observers who cannot be communicated with are returned.


   ----------
   -- Logging
   --

   procedure Logger_is (Now : in Event.Logger.view);
   function  Logger       return Event.Logger.view;

end lace.Subject;
