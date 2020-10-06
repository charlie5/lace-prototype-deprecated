with
     lace.Event,
     lace.Response;

limited
with
     lace.event.Logger;

package lace.Observer
--
--  Provides an interface for a local event Observer.
--
is
   type Item  is limited interface;
   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;


   --- Attributes
   --

   function Name (Self : in Item) return String is abstract;


   --- Responses
   --

   procedure add (Self : access Item;   the_Response : in Response.view;
                                        to_Kind      : in event.Kind;
                                        from_Subject : in String) is abstract;

   procedure rid (Self : access Item;   the_Response : in Response.view;
                                        to_Kind      : in event.Kind;
                                        from_Subject : in String) is abstract;

   procedure relay_responseless_Events (Self : in out Item;   To : in Observer.view) is abstract;


   --- Operations
   --

   procedure receive (Self : access Item;   the_Event    : in Event.item'Class := event.null_Event;
                                            from_Subject : in String) is abstract;
   --
   --  Accepts an Event from a Subject.


   procedure respond (Self : access Item)   is abstract;
   --
   --  Performs the Response for (and then removes) each pending Event.


   --- Logging
   --

   procedure Logger_is (Now : access event.Logger.item'Class);
   function  Logger    return access event.Logger.item'Class;


end lace.Observer;
