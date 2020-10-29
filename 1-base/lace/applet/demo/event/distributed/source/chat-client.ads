with
     lace.Event,
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

   type View  is access all Item'Class;
   type Views is array (Positive range <>) of view;


   procedure   register_Client (Self : in out Item;   other_Client : lace.remote.Subject .view) is abstract;
   procedure deregister_Client (Self : in out Item;   other_Client : lace.remote.Observer.view) is abstract;

   procedure   register_Client (Self : in out Item;   other_Client : lace.remote.Observer.view) is abstract;

   type Message (Length : Natural) is new lace.Event.item with
      record
         Text : String (1..Length);
      end record;

end chat.Client;
