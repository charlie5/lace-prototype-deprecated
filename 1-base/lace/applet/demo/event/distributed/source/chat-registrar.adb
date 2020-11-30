with
     lace.remote.Observer,

     system.RPC,

     ada.Strings.unbounded,
     ada.Text_IO;

package body chat.Registrar
is
   use ada.Strings.unbounded;
   use type Client.view;


   -- Protection against race conditions.
   --

   type client_Info is
         record
            View        : Client.view;
            Name        : unbounded_String;
            as_Observer : lace.remote.Observer.view;
         end record;

   type client_Info_array is array (Positive range <>) of client_Info;

   max_Clients : constant := 5_000;

   protected safe_Clients
   is
      procedure add (the_Client : in Client.view);
      procedure Rid (the_Client : in Client.view);

      function  all_client_Info return client_Info_array;
   private
      Clients : client_Info_array (1..max_Clients);
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

         raise Program_Error with "Unknown Client";
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
      Done : Boolean := False;
   begin
      loop
         select
            accept halt
            do
               Done := True;
            end halt;
         or
            delay 5.0;
         end select;

         exit when Done;

         declare
            use ada.Text_IO;
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
                  when system.RPC.Communication_Error =>
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
                     put_Line ("Ridding " & (+Dead (i).Name) & " from " & Each.Name);
                     Each.rid_Client (+Dead (i).Name,
                                       Dead (i).as_Observer);
                  end loop;
               end loop;
            end;
         end;
      end loop;
   end check_Client_lives;


   procedure shutdown
   is
      all_Clients : constant Client.views := chat.Registrar.all_Clients;
   begin
      for Each of all_Clients
      loop
         Each.Registrar_has_shutdown;
      end loop;

      check_Client_lives.halt;
   end shutdown;

end chat.Registrar;
