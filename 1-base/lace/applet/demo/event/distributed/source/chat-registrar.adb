package body chat.Registrar
is
   use type Client.view;


   protected body safe_Clients
   is
      procedure add (the_Client : in Client.view)
      is
      begin
         for i in Clients'Range
         loop
            if Clients (i) = null then
               Clients (i) := the_Client;
               return;
            end if;
         end loop;
      end add;


      procedure rid (the_Client : in Client.view)
      is
      begin
         for i in Clients'Range
         loop
            if Clients (i) = the_Client then
               Clients (i) := null;
               return;
            end if;
         end loop;

         raise Program_Error with "Unknown Client";
      end rid;


      function all_Clients return Client.views
      is
         Count  : Natural := 0;
         Result : chat.Client.views (1..5);
      begin
         for i in Clients'Range
         loop
            if Clients (i) /= null
            then
               Count          := Count + 1;
               Result (Count) := Clients (i);
            end if;
         end loop;

         return Result (1..Count);
      end all_Clients;

   end safe_Clients;


   procedure register (the_Client : in Client.view)
   is
   begin
      safe_Clients.add (the_Client);
   end register;


   procedure deregister (the_Client : in Client.view)
   is
   begin
      safe_Clients.rid (the_Client);
   end deregister;


   function all_Clients return chat.Client.views
   is
   begin
      return safe_Clients.all_Clients;
   end all_Clients;


   task      keep_Alive;
   task body keep_Alive
   is
   begin
      loop
         delay 1.0;
      end loop;
   end keep_Alive;

end chat.Registrar;
