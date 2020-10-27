with
     lace.event_Conversions;

with ada.Text_IO,
     system.Address_Image;

package body lace.Event.utility
is
   --- Event Kinds
   --

   function to_Kind (From : in ada.tags.Tag) return lace.event.Kind
   is
      use lace.event_Conversions;
   begin
      return to_event_Kind (From);
   end to_Kind;


   function Name_of (Kind : in event.Kind) return String
   is
      use lace.event_Conversions,
          ada.Tags;
      use ada.Text_IO;
   begin
      --  return ("KKKKK");
      Put_Line ("in Name_of - 1");
      Put_Line ("Kind => " & system.Address_Image (system.Address (Kind)));
      declare
         the_Tag : Tag := to_Tag (Kind);
      begin
         Put_Line ("in Name_of - 2");
         declare
            Name : String := expanded_Name (the_Tag);
         begin
            Put_Line ("in Name_of - 3: " & Name);
            return Name;
         end;
      end;

      return expanded_Name (to_Tag (Kind));
   end Name_of;


   --- Events
   --

   function Kind_of (the_Event : in Event.item'Class) return event.Kind
   is
   begin
      return to_Kind (the_Event'Tag);
   end Kind_of;


   function Name_of (the_Event : in Event.item'Class) return String
   is
   begin
      return Name_of (Kind_of (the_Event));
   end Name_of;


end lace.Event.utility;
