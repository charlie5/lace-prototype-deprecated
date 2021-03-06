with
     chat.Registrar,

     lace.Response,
     lace.Observer,
     lace.Event.utility,

     system.RPC,
     ada.Exceptions,
     ada.Text_IO;

package body chat.Client.local
is
   -- Utility
   --
   function "+" (From : in unbounded_String) return String
                 renames to_String;

   -- Responses
   --
   type Show is new lace.Response.item with null record;

   -- Response is to display the chat message on the users console.
   --
   overriding
   procedure respond (Self : in out Show;   to_Event : in lace.Event.item'Class)
   is
      pragma Unreferenced (Self);
      use ada.Text_IO;
      the_Message : constant Message := Message (to_Event);
   begin
      put_Line (the_Message.Text (1 .. the_Message.Length));
   end respond;

   the_Response : aliased chat.Client.local.show;


   -- Forge
   --
   function to_Client (Name : in String) return Item
   is
   begin
      return Self : Item
      do
         Self.Name := to_unbounded_String (Name);
      end return;
   end to_Client;


   -- Attributes
   --
   overriding
   function Name (Self : in Item) return String
   is
   begin
      return to_String (Self.Name);
   end Name;


   overriding
   function as_Observer (Self : access Item) return lace.Observer.view
   is
   begin
      return Self;
   end as_Observer;


   overriding
   function as_Subject  (Self : access Item) return lace.Subject.view
   is
   begin
      return Self;
   end as_Subject;


   -- Operations
   --

   overriding
   procedure register_Client (Self : in out Item;   other_Client : in Client.view)
   is
      use lace.Event.utility,
          ada.Text_IO;
   begin
      lace.Event.utility.connect (the_Observer  => Self'unchecked_Access,
                                  to_Subject    => other_Client.as_Subject,
                                  with_Response => the_Response'Access,
                                  to_Event_Kind => to_Kind (chat.Client.Message'Tag));
      put_Line (other_Client.Name & " is here.");
   end register_Client;


   overriding
   procedure deregister_Client (Self : in out Item;   other_Client_as_Observer : in lace.Observer.view;
                                                      other_Client_Name        : in String)
   is
      use lace.Event.utility,
          ada.Text_IO;
   begin
      begin
         Self.as_Subject.deregister (other_Client_as_Observer,
                                     to_Kind (chat.Client.Message'Tag));
      exception
         when constraint_Error =>
            raise unknown_Client with "Other client not known. Deregister is not required.";
      end;

      Self.as_Observer.rid (the_Response'unchecked_Access,
                            to_Kind (chat.Client.Message'Tag),
                            other_Client_Name);

      put_Line (other_Client_Name & " leaves.");
   end deregister_Client;


   overriding
   procedure Registrar_has_shutdown  (Self : in out Item)
   is
      use ada.Text_IO;
   begin
      put_Line ("The Registrar has shutdown. Press <Enter> to exit.");
      Self.Registrar_has_shutdown := True;
   end Registrar_has_shutdown;


   task check_Registrar_lives
   is
      entry start (Self : in chat.Client.local.view);
      entry halt;
   end check_Registrar_lives;

   task body check_Registrar_lives
   is
      use ada.Text_IO;
      Done : Boolean := False;
      Self : chat.Client.local.view;
   begin
      loop
         select
            accept start (Self : in chat.Client.local.view)
            do
               check_Registrar_lives.Self := Self;
            end start;
         or
            accept halt
            do
               Done := True;
            end halt;
         or
            delay 15.0;
         end select;

         exit when Done;

         begin
            chat.Registrar.ping;
         exception
            when system.RPC.communication_Error =>
               put_Line ("The Registrar has died. Press <Enter> to exit.");
               Self.Registrar_is_dead := True;
         end;
      end loop;

   exception
      when E : others =>
         new_Line;
         put_Line ("Error in check_Registrar_lives task.");
         new_Line;
         put_Line (ada.exceptions.exception_Information (E));
   end check_Registrar_lives;


   procedure start (Self : in out chat.Client.local.item)
   is
      use ada.Text_IO;
   begin
      -- Setup
      --
      begin
         chat.Registrar.register (Self'unchecked_Access);   -- Register our client with the registrar.
      exception
         when chat.Registrar.Name_already_used =>
            put_Line (+Self.Name & " is already in use.");
            check_Registrar_lives.halt;
            return;
      end;

      lace.Event.utility.use_text_Logger ("events");

      check_Registrar_lives.start (Self'unchecked_Access);

      declare
         Peers : constant chat.Client.views := chat.Registrar.all_Clients;
      begin
         for i in Peers'Range
         loop
            if Self'unchecked_Access /= Peers (i)
            then
               begin
                  Peers (i).register_Client (Self'unchecked_Access);    -- Register our client with all other clients.
                  Self     .register_Client (Peers (i));                -- Register all other clients with our client.
               exception
                  when system.RPC.communication_Error
                     | storage_Error =>
                     null;     -- Peer (i) has died, so ignore it and do nothing.
               end;
            end if;
         end loop;
      end;

      -- Main loop
      --
      loop
         declare
            procedure broadcast (the_Text : in String)
            is
               the_Message : constant chat.Client.Message := (Length (Self.Name) + 2 + the_Text'Length,
                                                              +Self.Name & ": " & the_Text);
            begin
               Self.emit (the_Message);
            end broadcast;

            chat_Message : constant String := get_Line;
         begin
            exit
              when chat_Message = "q"
              or   Self.Registrar_has_shutdown
              or   Self.Registrar_is_dead;

            broadcast (chat_Message);
         end;
      end loop;

      -- Shutdown
      --
      if    not Self.Registrar_has_shutdown
        and not Self.Registrar_is_dead
      then
         begin
            chat.Registrar.deregister (Self'unchecked_Access);
         exception
            when system.RPC.communication_Error =>
               Self.Registrar_is_dead := True;
         end;

         if not Self.Registrar_is_dead
         then
            declare
               Peers : constant chat.Client.views := chat.Registrar.all_Clients;
            begin
               for i in Peers'Range
               loop
                  if Self'unchecked_Access /= Peers (i)
                  then
                     begin
                        Peers (i).deregister_Client ( Self'unchecked_Access,   -- Deregister our client with every other client.
                                                     +Self.Name);
                     exception
                        when system.RPC.communication_Error
                           | storage_Error =>
                           null;   -- Peer is dead, so do nothing.
                     end;
                  end if;
               end loop;
            end;
         end if;
      end if;

      check_Registrar_lives.halt;
      lace.Event.utility.close;
   end start;


   -- 'last_chance_Handler' is commented out to avoid multiple definitions
   --  of link symbols in 'build_All' test procedure (Tier 5).
   --

   --  procedure last_chance_Handler (Msg  : in system.Address;
   --                                 Line : in Integer);
   --
   --  pragma Export (C, last_chance_Handler,
   --                 "__gnat_last_chance_handler");
   --
   --  procedure last_chance_Handler (Msg  : in System.Address;
   --                                 Line : in Integer)
   --  is
   --     pragma Unreferenced (Msg, Line);
   --     use ada.Text_IO;
   --  begin
   --     put_Line ("The Registrar is not running.");
   --     put_Line ("Press Ctrl-C to quit.");
   --     check_Registrar_lives.halt;
   --     delay Duration'Last;
   --  end last_chance_Handler;


end chat.Client.local;
