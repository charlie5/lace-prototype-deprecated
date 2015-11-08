with lace.event.Utility;



package body lace.event.Logger.text
is

   use lace.event.Utility,
       ada.Text_IO;




   ----------
   --- Forge
   --


   function  to_Logger (Name : in String) return Item
   is
   begin
      return Self : Item
      do
         create (Self.File,  out_File,  Name & ".log");
      end return;
   end to_Logger;





   overriding
   procedure destruct (Self : in out Item)
   is
   begin
      close (Self.File);
   end destruct;







   ---------------
   --- Operations
   --


   overriding
   procedure log_Connection (Self : in out Item;   From           : in Observer.view;
                                                   To             : in Subject.view;
                                                   for_Kind       : in event.Kind)
   is
   begin
      put_Line (Self.File,   "log *Connection* => "
                           & From.Name & " observes " & To.Name
                           & "   for event kind " & Name_of (for_Kind));
      new_Line (Self.File);
   end log_Connection;





   overriding
   procedure log_Disconnection (Self : in out Item;   From           : in Observer.view;
                                                      To             : in Subject.view;
                                                      for_Kind       : in event.Kind)
   is
      pragma Unreferenced (From, To, for_Kind);
   begin
      put_Line (Self.File,  "log *Disconnection*");   -- tbd: Add more details re From, To and for_Kind.
   end log_Disconnection;





   overriding
   procedure log_Emit (Self : in out Item;   From           : in Subject.view;
                                             To             : in Observer.view;
                                             the_Event      : in Event.item'Class)
   is
   begin
      if Self.Ignored.contains (to_Kind (the_Event'Tag)) then
         return;
      end if;

      put_Line (Self.File,   "log *Emit* => "
                           & From.Name & " sends " & Name_of (Kind_of (the_Event))
                           & "   to "  & To.Name);
   end log_Emit;





   overriding
   procedure log_Relay         (Self : in out Item;   From           : in Observer.view;
                                                      To             : in Observer.view;
                                                      the_Event      : in Event.item'Class)
   is
      pragma Unreferenced (From, To, the_Event);
   begin
      put_Line (Self.File,  "log_Relay");                 -- tbd: Add more details re From, To and the_Event.
   end log_Relay;





   overriding
   procedure log_new_Response (Self : in out Item;   the_Response   : in lace.Response.view;
                                                     of_Observer    : in Observer.item'Class;
                                                     to_Kind        : in event.Kind;
                                                     from_Subject   : in String)
   is
   begin
      put_Line ("'" & the_Response.Name & "'");

      put_Line (Self.File,   "log new *Response* => "
                           & of_Observer.Name
                           & " responds to " & Name_of (to_Kind)
                           & " from "        & from_Subject
                           & "   with "      & the_Response.Name);
   end log_new_Response;





   overriding
   procedure log_rid_Response (Self : in out Item;   the_Response   : in lace.Response.view;
                                                     of_Observer    : in Observer.item'Class;
                                                     to_Kind        : in event.Kind;
                                                     from_Subject   : in String)
   is
   begin
      put_Line (Self.File,   "log rid *Response* => "
                           & of_Observer.Name
                           & " *no longer* responds to " & Name_of (to_Kind)
                           & " from "                    & from_Subject
                           & "   with "                  & the_Response.Name);
   end log_rid_Response;






   overriding
   procedure log_Response (Self : in out Item;   the_Response   : in lace.Response.view;
                                                 of_Observer    : in Observer.view;
                                                 to_Event       : in Event.item'Class;
                                                 from_Subject   : in String)
   is
   begin
      if Self.Ignored.contains (to_Kind (to_Event'Tag)) then
         return;
      end if;

      put_Line (Self.File,   "log *Response* => "
                           & of_Observer.Name
                           & " responds to " & Name_of (to_Kind (to_Event'Tag))
                           & " from "        & from_Subject
                           & "   with "      & the_Response.Name);
   end log_Response;





   overriding
   procedure log (Self : in out Item;   any_Message : in String)
   is
   begin
      put_Line (Self.File,  any_Message);
   end log;





   overriding
   procedure ignore (Self : in out Item;   the_Kind : in event.Kind)
   is
   begin
      Self.Ignored.insert (the_Kind);
   end ignore;



   function Hash (Item : in Event.Kind) return Ada.Containers.Hash_Type is
      Result : Ada.Containers.Hash_Type;
      for Result'Address use Item'Address;
      pragma Assert (Item'Size >= Result'Size, "Defect hash function.");
   begin
      return Result;
   end Hash;

end lace.event.Logger.text;
