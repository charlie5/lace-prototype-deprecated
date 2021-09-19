with
     gel.Keyboard,

     lace.Event,
     lace.Subject;

package gel.Mouse with remote_Types
--
-- Provides an interface to a mouse.
--
is
   type Item is  limited interface
             and lace.Subject.item;

   type View is access all Item'class;


   ----------
   --- Events
   --

   type Button_Id is range 1 .. 5;
   type Site      is new math.Integers (1 .. 2);      -- Window pixel (x,y) site.

   type button_press_Event is new lace.Event.item with
      record
         Button       : button_Id;
         modifier_Set : keyboard.modifier_Set;
         Site         : mouse.Site;
      end record;

   type button_release_Event is new lace.Event.item with
      record
         Button       : button_Id;
         modifier_Set : keyboard.modifier_Set;
         Site         : mouse.Site;
      end record;

   type motion_Event is new lace.Event.item with
      record
         Site : mouse.Site;
      end record;


   --------------
   --- Attributes
   --

   -- Nil.


   --------------
   --- Operations
   --

   procedure emit_button_press_Event   (Self : in out Item'Class;   Button    : in mouse.button_Id;
                                                                    Modifiers : in keyboard.modifier_Set;
                                                                    Site      : in mouse.Site);

   procedure emit_button_release_Event (Self : in out Item'Class;   Button    : in mouse.button_Id;
                                                                    Modifiers : in keyboard.modifier_Set;
                                                                    Site      : in mouse.Site);

   procedure emit_motion_Event         (Self : in out Item'Class;   Site      : in mouse.Site);


end gel.Mouse;
