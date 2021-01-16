with
     lace.Event,
     lace.Observer;

limited
with
     lace.Event.Logger;

package lace.Subject
--
--  Provides an interface for an event Subject.
--
is
   pragma remote_Types;

   type Item  is limited interface;
   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;


   -- Containers
   --

   type Observer_views is array (Positive range <>) of Observer.view;


   -- Attributes
   --

   function Name (Self : in Item) return event.subject_Name is abstract;


   -- Observers
   --

   procedure register       (Self : access Item;   the_Observer : in Observer.view;
                                                   of_Kind      : in lace.event.Kind) is abstract;

   procedure deregister     (Self : in out Item;   the_Observer : in Observer.view;
                                                   of_Kind      : in lace.event.Kind) is abstract;

   function  Observers      (Self : in     Item;   of_Kind : in lace.event.Kind) return Observer_views is abstract;
   function  observer_Count (Self : in     Item) return Natural is abstract;


   -- Operations
   --

   procedure emit (Self : access Item;   the_Event : in lace.Event.item'Class := lace.event.null_Event) is abstract;
   --
   -- Communication errors are ignored.

   function  emit (Self : access Item;   the_Event : in lace.Event.item'Class := lace.event.null_Event)
                   return Observer_views is abstract;
   --
   -- Observers who cannot be communicated with are returned.


   -- Logging
   --

   procedure Logger_is (Now : access Event.Logger.item'Class);
   function  Logger    return access Event.Logger.item'Class;

end lace.Subject;
