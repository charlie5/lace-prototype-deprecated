with
     chat.Client,
     lace.remote.Subject;

package chat.Registrar
--
-- A singleton providing the central chat registrar.
-- Limited to a maximum of 5 chat clients running at once.
--
is
   pragma remote_call_Interface;


   procedure   register (the_Client : in Client.remote);
   procedure deregister (the_Client : in Client.remote);

   function  all_Clients return chat.Client.remotes;


   procedure   register (the_Client : in lace.remote.Subject.view);
   procedure deregister (the_Client : in lace.remote.Subject.view);

   function  all_Subjects return lace.remote.Subject.views;



private

   Clients  : chat.Client.remotes (1..5);
   Subjects : lace.remote.Subject.views (1..5);

end chat.Registrar;


--- Notes:
--
-- - An additional container for Subjects is required to work-around what appears to be
--   a bug in interface conversions when applies to RACW's (see interface_test).
