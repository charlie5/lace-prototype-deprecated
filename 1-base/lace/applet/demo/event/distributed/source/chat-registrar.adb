with ada.Text_IO;

package body chat.Registrar
is
   -- Clients
   --
   use type Client.remote;

   use ada.Text_IO;

   procedure register (the_Client : in Client.remote)
   is
   begin
      put_Line ("Register a client.");
      for i in Clients'Range
      loop
         if Clients (i) = null then
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

      raise Program_Error with "Unknown Client";
   end deregister;


   function all_Clients return chat.Client.remotes
   is
      Count  : Natural := 0;
      Result : chat.Client.remotes (1..5);
   begin
      for i in Clients'Range
      loop
         if Clients (i) /= null
         then
            put_Line ("all Clinets " & Clients (i).Name);

            Count          := Count + 1;
            Result (Count) := Clients (i);
         end if;
      end loop;

      return Result (1..Count);
   end all_Clients;


   -- Subjects
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

      raise Program_Error with "Unknown Subject";
   end deregister;


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
   end all_Subjects;


   -- Observers
   --
   use type lace.remote.Observer.view;

   procedure register (the_Client : in lace.remote.Observer.view)
   is
   begin
      for i in Observers'Range
      loop
         if Observers (i) = null then
            Observers (i) := the_Client;
            return;
         end if;
      end loop;
   end register;


   procedure deregister (the_Client : in lace.remote.Observer.view)
   is
   begin
      for i in Observers'Range
      loop
         if Observers (i) = the_Client then
            Observers (i) := null;
            return;
         end if;
      end loop;

      raise Program_Error with "Unknown Subject";
   end deregister;


   function all_Observers return lace.remote.Observer.views
   is
      use type lace.remote.Observer.view;
      Count  : Natural := 0;
      Result : lace.remote.Observer.views (1..5);
   begin
      for i in Observers'Range
      loop
         if Observers (i) /= null
         then
            Count          := Count + 1;
            Result (Count) := Observers (i);
         end if;
      end loop;

      return Result (1..Count);
   end all_Observers;


   task      keep_Alive;
   task body keep_Alive
   is
   begin
      loop
         delay 1.0;
      end loop;
   end keep_Alive;

end chat.Registrar;
