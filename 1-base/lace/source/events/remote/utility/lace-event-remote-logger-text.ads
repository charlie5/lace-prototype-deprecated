with
     lace.remote.Observer,
     lace.remote.Subject,
     lace.remote.Response;

private
with
     ada.Text_IO,
     ada.Containers.hashed_Sets;


package lace.Event.remote.Logger.text
--
--  Provides a logger which logs to a text file.
--
is

   type Item is limited new Logger.item with private;
   type View is access all item'Class;


   ----------
   --- Forge
   --

   function  to_Logger (Name : in     String) return Item;

   overriding
   procedure destruct  (Self : in out Item);



   ---------------
   --- Operations
   --

   --- Logging of event consfiguration.
   --

   overriding
   procedure log_Connection    (Self : in out Item;   From           : in lace.remote.Observer.view;
                                                      To             : in lace.remote.Subject.view;
                                                      for_Kind       : in event.Kind);

   overriding
   procedure log_Disconnection (Self : in out Item;   From           : in lace.remote.Observer.view;
                                                      To             : in lace.remote.Subject.view;
                                                      for_Kind       : in event.Kind);


   overriding
   procedure log_new_Response  (Self : in out Item;   the_Response   : in lace.remote.Response.view;
                                                      of_Observer    : in lace.remote.Observer.item'Class;
                                                      to_Kind        : in event.Kind;
                                                      from_Subject   : in String);

   overriding
   procedure log_rid_Response  (Self : in out Item;   the_Response   : in lace.remote.Response.view;
                                                      of_Observer    : in lace.remote.Observer.item'Class;
                                                      to_Kind        : in event.Kind;
                                                      from_Subject   : in String);

   --- Logging of event transmission.
   --

   overriding
   procedure log_Emit          (Self : in out Item;   From           : in lace.remote.Subject.view;
                                                      To             : in lace.remote.Observer.view;
                                                      the_Event      : in Event.item'Class);

   overriding
   procedure log_Relay         (Self : in out Item;   From           : in lace.remote.Observer.view;
                                                      To             : in lace.remote.Observer.view;
                                                      the_Event      : in Event.item'Class);

   overriding
   procedure log_Response      (Self : in out Item;   the_Response   : in lace.remote.Response.view;
                                                      of_Observer    : in lace.remote.Observer.view;
                                                      to_Event       : in Event.item'Class;
                                                      from_Subject   : in String);

   --- Logging of miscellaneous messages.
   --

   overriding
   procedure log               (Self : in out Item;   any_Message    : in String);


   --- Log filtering
   --

   overriding
   procedure ignore            (Self : in out Item;    the_Kind      : in event.Kind);





private

   function Hash (Item : in Event.Kind) return Ada.Containers.Hash_Type;

   package  event_kind_Sets is new Ada.Containers.hashed_Sets (event.Kind,  Hash,  "=");
   subtype  event_kind_Set  is     event_kind_Sets.Set;


   type Item is limited new Logger.item with
      record
         File    : ada.Text_IO.File_Type;
         Ignored : event_kind_Set;
      end record;

end lace.Event.remote.Logger.text;
