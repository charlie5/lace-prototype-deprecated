with
     lace.remote.Response,
     lace.Any;

private
with
     lace.remote.make_Subject,
     lace.remote.make_Observer;

package chat.Client.local
--
-- Provides a local client.
--
is
   type Item is limited new lace.Any.limited_item
                        and chat.Client     .item with private;

   type View is access all Item'Class;


   -- Forge
   --

   function new_Client (Name : in String) return View;


   -- Attributes
   --

   overriding
   function Name (Self : in Item) return String;


   -- Operations
   --

   overriding
   procedure   register_Client (Self : in out Item;   Other : lace.remote.Subject.view);

   overriding
   procedure deregister_Client (Self : in out Item;   Other : lace.remote.Observer.view);


   -- Responses
   --

   type Show is new lace.remote.Response.item with null record;


   overriding
   procedure respond (Self : in out Show;   to_Event : in lace.Event.item'Class);
   --
   -- Response is to display the chat message on the users console.



private

   package Observer is new lace.remote.make_Observer (lace.Any.limited_item);
   package Subject  is new lace.remote.make_Subject  (Observer        .item);

   type Item is limited new Subject    .item
                        and chat.Client.item with
      record
         Name : access String;
      end record;

end chat.Client.local;
