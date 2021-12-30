with
     lace.Event,
     lace.Response;

limited
with
     lace.Event.Logger;


package lace.Observer
--
--  Provides an interface for an event Observer.
--
is
   pragma remote_Types;

   type Item  is limited interface;
   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;

   type fast_View  is access all Item'Class with Asynchronous;
   type fast_Views is array (Positive range <>) of fast_View;


   -------------
   -- Attributes
   --

   function Name (Self : in Item) return event.observer_Name is abstract;


   ------------
   -- Responses
   --

   procedure add (Self : access Item;   the_Response : in Response.view;
                                        to_Kind      : in event.Kind;
                                        from_Subject : in event.subject_Name) is abstract;

   procedure rid (Self : access Item;   the_Response : in Response.view;
                                        to_Kind      : in event.Kind;
                                        from_Subject : in event.subject_Name) is abstract;

   procedure relay_responseless_Events
                 (Self : in out Item;   To : in Observer.view) is abstract;


   -------------
   -- Operations
   --

   procedure receive (Self : access Item;   the_Event    : in Event.item'Class := event.null_Event;
                                            from_Subject : in event.subject_Name) is abstract;
   --
   -- Accepts an Event from a Subject.

   procedure respond (Self : access Item) is abstract;
   --
   -- Performs the Response for (and then removes) each pending Event.


   ----------
   -- Logging
   --

   procedure Logger_is (Now : in Event.Logger.view);
   function  Logger    return    Event.Logger.view;

end lace.Observer;
