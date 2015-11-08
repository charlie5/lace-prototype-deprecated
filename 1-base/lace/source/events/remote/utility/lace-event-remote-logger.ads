with
     lace.remote.Observer,
     lace.remote.Subject,
     lace.remote.Response;


package lace.event.remote.Logger
--
--  Provides an event logging inteface.
--
is

   type Item is limited interface;


   ----------
   --  Forge
   --

   procedure destruct (Self : in out Item)   is null;



   ---------------
   --  Operations
   --


   --- Logging of event consfiguration.
   --

   procedure log_Connection    (Self : in out Item;   From           : in lace.remote.Observer.view;
                                                      To             : in lace.remote.Subject .view;
                                                      for_Kind       : in event.Kind) is abstract;

   procedure log_Disconnection (Self : in out Item;   From           : in lace.remote.Observer.view;
                                                      To             : in lace.remote.Subject .view;
                                                      for_Kind       : in event.Kind)   is abstract;


   procedure log_new_Response  (Self : in out Item;   the_Response   : in lace.remote.Response.view;
                                                      of_Observer    : in lace.remote.Observer.item'Class;
                                                      to_Kind        : in event.Kind;
                                                      from_Subject   : in String)   is abstract;

   procedure log_rid_Response  (Self : in out Item;   the_Response   : in lace.remote.Response.view;
                                                      of_Observer    : in lace.remote.Observer.item'Class;
                                                      to_Kind        : in event.Kind;
                                                      from_Subject   : in String)   is abstract;


   --- Logging of event transmission.
   --

   procedure log_Emit          (Self : in out Item;   From           : in lace.remote.Subject .view;
                                                      To             : in lace.remote.Observer.view;
                                                      the_Event      : in Event.item'Class)   is abstract;

   procedure log_Relay         (Self : in out Item;   From           : in lace.remote.Observer.view;
                                                      To             : in lace.remote.Observer.view;
                                                      the_Event      : in Event.item'Class)   is abstract;

   procedure log_Response      (Self : in out Item;   the_Response   : in lace.remote.Response.view;
                                                      of_Observer    : in lace.remote.Observer     .view;
                                                      to_Event       : in Event.item'Class;
                                                      from_Subject   : in String)   is abstract;


   --- Logging of miscellaneous messages.
   --

   procedure log               (Self : in out Item;   any_Message : in String)   is abstract;



   --- Log filtering
   --

   procedure ignore            (Self : in out Item;    the_Kind : in event.Kind)   is abstract;


end lace.event.remote.Logger;
