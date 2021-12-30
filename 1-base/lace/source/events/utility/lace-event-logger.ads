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
   type View is access all Item'Class;


   --------
   -- Forge
   --
   procedure destruct (Self : in out Item) is null;


   -------------
   -- Operations
   --

   -- Logging of event configuration.
   --

   procedure log_Connection    (Self : in out Item;   From     : in Observer.view;
                                                      To       : in Subject .view;
                                                      for_Kind : in Event.Kind) is abstract;

   procedure log_Disconnection (Self : in out Item;   From     : in Observer.view;
                                                      To       : in Subject .view;
                                                      for_Kind : in Event.Kind) is abstract;


   procedure log_new_Response  (Self : in out Item;   the_Response : in Response.view;
                                                      of_Observer  : in Observer.item'Class;
                                                      to_Kind      : in Event.Kind;
                                                      from_Subject : in subject_Name) is abstract;

   procedure log_rid_Response  (Self : in out Item;   the_Response : in Response.view;
                                                      of_Observer  : in Observer.item'Class;
                                                      to_Kind      : in Event.Kind;
                                                      from_Subject : in subject_Name) is abstract;
   -- Logging of event transmission.
   --

   procedure log_Emit     (Self : in out Item;   From         : in Subject .view;
                                                 To           : in Observer.view;
                                                 the_Event    : in Event.item'Class) is abstract;

   procedure log_Relay    (Self : in out Item;   From         : in Observer.view;
                                                 To           : in Observer.view;
                                                 the_Event    : in Event.item'Class) is abstract;

   procedure log_Response (Self : in out Item;   the_Response : in Response.view;
                                                 of_Observer  : in Observer.view;
                                                 to_Event     : in Event.item'Class;
                                                 from_Subject : in subject_Name) is abstract;
   -- Logging of miscellaneous messages.
   --

   procedure log (Self : in out Item;   Message : in String) is abstract;


   -- Log filtering.
   --

   procedure ignore (Self : in out Item;   Kind : in Event.Kind) is abstract;


end lace.Event.Logger;
