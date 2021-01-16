with
     lace.Event,
     lace.Observer,
     lace.Subject,
     lace.Response;

package lace.Event.Logger
--
--  Provides an event logging interface.
--
is
   type Item is limited interface;


   -- Operations
   --

   procedure destruct (Self : in out Item) is null;


   -- Logging of event consfiguration.
   --

   procedure log_Connection    (Self : in out Item;   From     : in lace.Observer.view;
                                                      To       : in lace.Subject .view;
                                                      for_Kind : in lace.event.Kind) is abstract;

   procedure log_Disconnection (Self : in out Item;   From     : in lace.Observer.view;
                                                      To       : in lace.Subject .view;
                                                      for_Kind : in lace.event.Kind) is abstract;


   procedure log_new_Response  (Self : in out Item;   the_Response : in lace.Response.view;
                                                      of_Observer  : in lace.Observer.item'Class;
                                                      to_Kind      : in lace.event.Kind;
                                                      from_Subject : in String) is abstract;

   procedure log_rid_Response  (Self : in out Item;   the_Response : in lace.Response.view;
                                                      of_Observer  : in lace.Observer.item'Class;
                                                      to_Kind      : in lace.event.Kind;
                                                      from_Subject : in String) is abstract;
   -- Logging of event transmission.
   --

   procedure log_Emit     (Self : in out Item;   From         : in lace.Subject .view;
                                                 To           : in lace.Observer.view;
                                                 the_Event    : in lace.Event.item'Class) is abstract;

   procedure log_Relay    (Self : in out Item;   From         : in lace.Observer.view;
                                                 To           : in lace.Observer.view;
                                                 the_Event    : in lace.Event.item'Class) is abstract;

   procedure log_Response (Self : in out Item;   the_Response : in lace.Response.view;
                                                 of_Observer  : in lace.Observer.view;
                                                 to_Event     : in lace.Event.item'Class;
                                                 from_Subject : in String) is abstract;
   -- Logging of miscellaneous messages.
   --

   procedure log (Self : in out Item;   Message : in String) is abstract;


   -- Log filtering.
   --

   procedure ignore (Self : in out Item;   Kind : in lace.event.Kind) is abstract;


end lace.Event.Logger;
