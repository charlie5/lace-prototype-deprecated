with
     lace.Event,
     lace.remote.Response,
     lace.remote.Subject,
     lace.remote.Observer;


package chat.Client
--
-- Provides an interface to a chat client.
--
is
   pragma remote_Types;


   type Item is  limited interface
             and lace.remote.Subject .item
             and lace.remote.Observer.item;

   type Remote  is access all Item'Class;
   type Remotes is array (Positive range <>) of Remote;



   procedure   register_Client (Self : in out Item;   Other_Client : lace.remote.Subject .view)   is abstract;
   procedure deregister_Client (Self : in out Item;   Other_Client : lace.remote.Observer.view)   is abstract;



   type Message (Length : Natural)  is new lace.Event.item with
      record
         Text : String (1..Length);
      end record;

end chat.Client;
