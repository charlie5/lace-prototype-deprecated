package body chat.Registrar
is

   ------------
   --- Clients
   --

   use type Client.remote;



   procedure register (the_Client : in Client.remote)
   is
   begin
      for i in Clients'Range
      loop
         if Clients (i) = null  then
            Clients (i) := the_Client;
            return;
         end if;
      end loop;
   end register;





   procedure deregister (the_Client : in Client.remote)
   is
   begin
      for i in Clients'Range
      loop
         if Clients (i) = the_Client then
            Clients (i) := null;
            return;
         end if;
      end loop;

      raise Program_Error with "unknown Client";
   end;





   function all_Clients return chat.Client.remotes
   is
      Count  : Natural := 0;
      Result : chat.Client.remotes (1..5);
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
   end;





   -------------
   --- Subjects
   --

   use type lace.remote.Subject.view;



   procedure register (the_Client : in lace.remote.Subject.view)
   is
   begin
      for i in Subjects'Range
      loop
         if Subjects (i) = null then
            Subjects (i) := the_Client;
            return;
         end if;
      end loop;
   end register;





   procedure deregister (the_Client : in lace.remote.Subject.view)
   is
   begin
      for i in Subjects'Range
      loop
         if Subjects (i) = the_Client then
            Subjects (i) := null;
            return;
         end if;
      end loop;

      raise Program_Error with "unknown Subject";
   end;





   function all_Subjects return lace.remote.Subject.views
   is
      Count  : Natural := 0;
      Result : lace.remote.Subject.views (1..5);
   begin
      for i in Subjects'Range
      loop
         if Subjects (i) /= null
         then
            Count          := Count + 1;
            Result (Count) := Subjects (i);
         end if;
      end loop;

      return Result (1..Count);
   end;




   task      keep_Alive;
   task body keep_Alive
   is
   begin
      loop
         delay 1.0;
      end loop;
   end;


end chat.Registrar;
