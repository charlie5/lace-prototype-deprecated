with
     lace.Observer,

     system.RPC,

     ada.Exceptions,
     ada.Strings.unbounded,
     ada.Text_IO;

package body chat.Registrar
is
   use ada.Strings.unbounded;
   use type Client.view;

   procedure last_chance_Handler (Msg  : in system.Address;
                                  Line : in Integer);

   pragma Export (C, last_chance_Handler,
                  "__gnat_last_chance_handler");

   procedure last_chance_Handler (Msg  : in System.Address;
                                  Line : in Integer)
   is
      pragma Unreferenced (Msg, Line);
      use ada.Text_IO;
   begin
      put_Line ("Unable to start the Registrar.");
      put_Line ("Please ensure the 'po_cos_naming' server is running.");
      put_Line ("Press Ctrl-C to quit.");

      delay Duration'Last;
   end last_chance_Handler;


   type client_Info is
         record
            View        : Client.view;
            Name        : unbounded_String;
            as_Observer : lace.Observer.view;
         end record;

   type client_Info_array is array (Positive range <>) of client_Info;

   max_Clients : constant := 5_000;


   -- Protection against race conditions.
   --

   protected safe_Clients
   is
      procedure add (the_Client : in Client.view);
      procedure rid (the_Client : in Client.view);

      function  all_client_Info return client_Info_array;
   private
      Clients : client_Info_array (1 .. max_Clients);
   end safe_Clients;


   protected body safe_Clients
   is
      procedure add (the_Client : in Client.view)
      is
         function "+" (From : in String) return unbounded_String
           renames to_unbounded_String;
      begin
         for i in Clients'Range
         loop
            if Clients (i).View = null then
               Clients (i).View        :=  the_Client;
               Clients (i).Name        := +the_Client.Name;
               Clients (i).as_Observer :=  the_Client.as_Observer;
               return;
            end if;
         end loop;
      end add;


      procedure rid (the_Client : in Client.view)
      is
      begin
         for i in Clients'Range
         loop
            if Clients (i).View = the_Client then
               Clients (i).View := null;
               return;
            end if;
         end loop;

         raise Program_Error with "Unknown client";
      end rid;


      function all_client_Info return client_Info_array
      is
         Count  : Natural := 0;
         Result : client_Info_array (1..max_Clients);
      begin
         for i in Clients'Range
         loop
            if Clients (i).View /= null
            then
               Count          := Count + 1;
               Result (Count) := Clients (i);
            end if;
         end loop;

         return Result (1..Count);
      end all_client_Info;

   end safe_Clients;


   procedure register (the_Client : in Client.view)
   is
      Name     : constant String            := the_Client.Name;
      all_Info : constant client_Info_array := safe_Clients.all_client_Info;
   begin
      for Each of all_Info
      loop
         if Each.Name = Name
         then
            raise Name_already_used;
         end if;
      end loop;

      safe_Clients.add (the_Client);
   end register;


   procedure deregister (the_Client : in Client.view)
   is
   begin
      safe_Clients.rid (the_Client);
   end deregister;


   function all_Clients return chat.Client.views
   is
      all_Info : constant client_Info_array := safe_Clients.all_client_Info;
      Result   :          chat.Client.views (all_Info'Range);
   begin
      for i in Result'Range
      loop
         Result (i) := all_Info (i).View;
      end loop;

      return Result;
   end all_Clients;


   task check_Client_lives
   is
      entry halt;
   end check_Client_lives;

   task body check_Client_lives
   is
      use ada.Text_IO;
      Done : Boolean := False;
   begin
      loop
         select
            accept halt
            do
               Done := True;
            end halt;
         or
            delay 15.0;
         end select;

         exit when Done;

         declare
            all_Info    : constant client_Info_array := safe_Clients.all_client_Info;

            Dead        : client_Info_array (all_Info'Range);
            dead_Count  : Natural := 0;

            function "+" (From : in unbounded_String) return String
                          renames to_String;
         begin
            for Each of all_Info
            loop
               begin
                  Each.View.ping;
               exception
                  when system.RPC.communication_Error
                     | storage_Error =>
                     put_Line (+Each.Name & " has died.");
                     deregister (Each.View);

                     dead_Count        := dead_Count + 1;
                     Dead (dead_Count) := Each;
               end;
            end loop;

            declare
               all_Clients : constant Client.views := chat.Registrar.all_Clients;
            begin
               for Each of all_Clients
               loop
                  for i in 1 .. dead_Count
                  loop
                     begin
                        put_Line ("Ridding " & (+Dead (i).Name) & " from " & Each.Name);
                        Each.deregister_Client ( Dead (i).as_Observer,
                                                +Dead (i).Name);
                     exception
                        when chat.Client.unknown_Client =>
                           put_Line ("Deregister of " & (+Dead (i).Name) & " from " & Each.Name & " is not needed.");
                     end;
                  end loop;
               end loop;
            end;
         end;
      end loop;

   exception
      when E : others =>
         new_Line;
         put_Line ("Error in check_Client_lives task.");
         new_Line;
         put_Line (ada.Exceptions.exception_Information (E));
   end check_Client_lives;


   procedure shutdown
   is
      all_Clients : constant Client.views := chat.Registrar.all_Clients;
   begin
      for Each of all_Clients
      loop
         begin
            Each.Registrar_has_shutdown;
         exception
            when system.RPC.communication_Error =>
               null;   -- Client has died. No action needed since we are shutting down.
         end;
      end loop;

      check_Client_lives.halt;
   end shutdown;


   procedure ping is null;

end chat.Registrar;
