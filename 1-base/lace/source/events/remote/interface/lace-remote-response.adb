with ada.Tags;



package body lace.remote.Response
is

   function Name (Self : in Item) return String
   is
   begin
      return ada.Tags.expanded_Name (Item'Class (Self)'Tag);
   end Name;

end lace.remote.Response;

