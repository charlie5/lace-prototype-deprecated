with
     lace.Event.utility,
     system.RPC,
     ada.unchecked_Conversion;


package body lace.Event.Logger.text
is
   use lace.Event.utility,
       ada.Text_IO;

   --------
   -- Forge
   --

   function to_Logger (Name : in String) return Item
   is
   begin
      return Self : Item
      do
         create (Self.File, out_File, Name & ".log");
      end return;
   end to_Logger;


   overriding
   procedure destruct (Self : in out Item)
   is
   begin
      close (Self.File);
   end destruct;


   -------------
   -- Operations
   --

   overriding
   procedure log_Connection (Self : in out Item;   From     : in Observer.view;
                                                   To       : in Subject .view;
                                                   for_Kind : in Event.Kind)
   is
   begin
      put_Line (Self.File,   "log Connection => "
                           & From.Name & " observes " & To.Name
                           & " for event kind " & Name_of (for_Kind) & ".");
   end log_Connection;


   overriding
   procedure log_Disconnection (Self : in out Item;   From     : in Observer.view;
                                                      To       : in Subject .view;
                                                      for_Kind : in Event.Kind)
   is

      function from_Name return String
      is
         function to_Integer is new ada.unchecked_Conversion (Observer.view,
                                                              long_Integer);
      begin
         return From.Name;
      exception
         when system.RPC.communication_Error
            | storage_Error =>
            return "dead Observer (" & to_Integer (From)'Image & ")";
      end from_Name;

   begin
      put_Line (Self.File,   "log Disconnection => "
                           & from_Name
                           & " no longer observes "
                           & To.Name
                           & " for event kind " & Name_of (for_Kind) & ".");
   end log_Disconnection;


   overriding
   procedure log_Emit (Self : in out Item;   From      : in Subject .view;
                                             To        : in Observer.view;
                                             the_Event : in Event.item'Class)
   is
      function to_Name return String
      is
         function to_Integer is new ada.unchecked_Conversion (lace.Observer.view,
                                                              long_Integer);
      begin
         return To.Name;
      exception
            when system.RPC.communication_Error
               | storage_Error =>
            return "dead Observer (" & to_Integer (To)'Image & ")";
      end to_Name;

   begin
      if Self.Ignored.contains (to_Kind (the_Event'Tag))
      then
         return;
      end if;

      put_Line (Self.File,   "log Emit => "
                           & From.Name & " sends " & Name_of (Kind_of (the_Event))
                           & " to "    & to_Name & ".");
   end log_Emit;


   overriding
   procedure log_Relay (Self : in out Item;   From      : in Observer.view;
                                              To        : in Observer.view;
                                              the_Event : in Event.item'Class)
   is
   begin
      put_Line (Self.File,  "log Relay => "
                           & From.Name & " relays " & Name_of (Kind_of (the_Event))
                           & " to "    & To.Name & ".");
   end log_Relay;


   overriding
   procedure log_new_Response (Self : in out Item;   the_Response : in Response.view;
                                                     of_Observer  : in Observer.item'Class;
                                                     to_Kind      : in Event.Kind;
                                                     from_Subject : in subject_Name)
   is
   begin
      put_Line (Self.File,   "log new Response => "
                           & of_Observer.Name
                           & " responds to " & Name_of (to_Kind)
                           & " from "        & from_Subject
                           & " with "        & the_Response.Name);
   end log_new_Response;


   overriding
   procedure log_rid_Response (Self : in out Item;   the_Response : in Response.view;
                                                     of_Observer  : in Observer.item'Class;
                                                     to_Kind      : in Event.Kind;
                                                     from_Subject : in subject_Name)
   is
   begin
      put_Line (Self.File,   "log rid Response => "
                           & of_Observer.Name
                           & " no longer responds to " & Name_of (to_Kind)
                           & " from "                  & from_Subject
                           & " with "                  & the_Response.Name & ".");
   end log_rid_Response;


   overriding
   procedure log_Response (Self : in out Item;   the_Response : in Response.view;
                                                 of_Observer  : in Observer.view;
                                                 to_Event     : in Event.item'Class;
                                                 from_Subject : in subject_Name)
   is
   begin
      if Self.Ignored.contains (to_Kind (to_Event'Tag))
      then
         return;
      end if;

      put_Line (Self.File,   "log Response => "
                           & of_Observer.Name
                           & " responds to " & Name_of (to_Kind (to_Event'Tag))
                           & " from "        & from_Subject
                           & " with "        & the_Response.Name & ".");
   end log_Response;


   overriding
   procedure log (Self : in out Item;   Message : in String)
   is
   begin
      put_Line (Self.File, Message);
   end log;


   overriding
   procedure ignore (Self : in out Item;   Kind : in Event.Kind)
   is
   begin
      Self.Ignored.insert (Kind);
   end ignore;

end lace.event.Logger.text;
