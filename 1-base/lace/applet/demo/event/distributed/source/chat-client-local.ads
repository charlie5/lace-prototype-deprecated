with
     lace.Any;

private
with
     lace.make_Subject,
     lace.make_Observer,
     ada.Strings.unbounded;

package chat.Client.local
--
-- Provides a local client.
-- Names must be unique.
--
is
   type Item is limited new lace.Any.limited_item
                        and chat.Client     .item with private;

   type View is access all Item'Class;


   -- Forge
   --
   function to_Client (Name : in String) return Item;


   -- Attributes
   --
   overriding
   function Name (Self : in Item) return String;

   overriding
   function as_Observer (Self : access Item) return lace.Observer.view;

   overriding
   function as_Subject  (Self : access Item) return lace.Subject.view;


   -- Operations
   --
   procedure start (Self : in out chat.Client.local.item);

   overriding
   procedure   register_Client (Self : in out Item;   other_Client : in Client.view);

   overriding
   procedure deregister_Client (Self : in out Item;   other_Client_as_Observer : in lace.Observer.view;
                                                      other_Client_Name        : in String);
   overriding
   procedure Registrar_has_shutdown (Self : in out Item);


private

   package Observer is new lace.make_Observer (lace.Any.limited_item);
   package Subject  is new lace.make_Subject  (Observer        .item);

   use ada.Strings.unbounded;

   type Item is limited new Subject    .item
                        and chat.Client.item with
      record
            Name                   : unbounded_String;
            Registrar_has_shutdown : Boolean := False;
            Registrar_is_dead      : Boolean := False;
      end record;

end chat.Client.local;
