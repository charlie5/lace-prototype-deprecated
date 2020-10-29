with
     chat.Client,
     lace.remote.Subject,
     lace.remote.Observer;

package chat.Registrar
--
-- A singleton providing the central chat registrar.
-- Limited to a maximum of 5 chat clients running at once.
--
is
   pragma remote_Call_interface;


   procedure   register (the_Client : in Client.view);
   procedure deregister (the_Client : in Client.view);

   function  all_Clients return chat.Client.views;


   procedure   register (the_Client : in lace.remote.Subject.view);
   procedure deregister (the_Client : in lace.remote.Subject.view);

   procedure   register (the_Client : in lace.remote.Observer.view);
   procedure deregister (the_Client : in lace.remote.Observer.view);

   function  all_Subjects  return lace.remote.Subject .views;
   function  all_Observers return lace.remote.Observer.views;



private

   Clients   : chat.Client         .views (1..5);
   Subjects  : lace.remote.Subject .views (1..5);
   Observers : lace.remote.Observer.views (1..5);

end chat.Registrar;


--- Notes:
--
-- - An additional container for subjects is required to work-around what appears to be
--   a bug in interface conversions when applied to RACW's (see interface_test).
