with
     lace.Event,
     lace.Subject,
     lace.Observer;

package chat.Client
--
-- Provides an interface to a chat client.
--
is
   pragma remote_Types;

   type Item is  limited interface
             and lace.Subject .item
             and lace.Observer.item;

   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;


   procedure Registrar_has_shutdown (Self : in out Item) is abstract;
   procedure ping                   (Self : in     Item) is null;

   procedure   register_Client (Self : in out Item;   other_Client : in Client.view) is abstract;

   procedure deregister_Client (Self : in out Item;   other_Client_as_Observer : in lace.Observer.view;
                                                      other_Client_Name        : in String) is abstract;
   --
   -- Raises unknown_Client exception when the other_Client is unknown.


   function  as_Observer (Self : access Item) return lace.Observer.view is abstract;
   function  as_Subject  (Self : access Item) return lace.Subject .view is abstract;


   type Message (Length : Natural) is new lace.Event.item with
      record
         Text : String (1..Length);
      end record;

   unknown_Client : exception;

end chat.Client;
