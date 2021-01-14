with
     chat.Client;

package chat.Registrar
--
-- A singleton providing the central chat registrar.
-- Limited to a maximum of 5_000 chat clients running at once.
--
is
   pragma remote_Call_interface;

   Name_already_used : exception;

   procedure   register (the_Client : in Client.view);
   procedure deregister (the_Client : in Client.view);

   function  all_Clients return chat.Client.views;

   procedure ping;
   procedure shutdown;

end chat.Registrar;
