package body gel.Mouse
is

   --------------
   --- Attributes
   --

   -- Nil.


   ---------------
   ---  Operations
   --

   procedure emit_button_press_Event (Self : in out Item'Class;   Button    : in mouse.button_Id;
                                                                  Modifiers : in keyboard.modifier_Set;
                                                                  Site      : in mouse.Site)
   is
   begin
      self.emit (button_press_Event' (Button, Modifiers, Site));
   end emit_button_press_Event;




   procedure emit_button_release_Event (Self : in out Item'Class;   Button    : in mouse.button_Id;
                                                                    Modifiers : in keyboard.modifier_Set;
                                                                    Site      : in mouse.Site)
   is
   begin
      self.emit (button_release_Event' (Button, Modifiers, Site));
   end emit_button_release_Event;



   procedure emit_motion_Event (Self : in out Item'Class;   Site : in mouse.Site)
   is
   begin
      self.emit (motion_Event' (site => Site));
   end emit_motion_Event;


end gel.Mouse;
