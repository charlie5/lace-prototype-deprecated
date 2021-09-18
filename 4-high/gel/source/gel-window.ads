with
     gel.Keyboard.local,
     gel.Mouse.local,
     openGL.Surface,
     lace.Subject_and_deferred_Observer;

private
with
     ada.Calendar;

package gel.Window
--
-- Models a UI Window.
--
is
   type Item is limited new lace.Subject_and_deferred_Observer.item with private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   package Forge
   is
      function new_Window (Name   : in String;
                           Width  : in Positive;
                           Height : in Positive) return View;
   end Forge;

   overriding
   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);


   --------------
   --- Exceptions
   --

   Error : exception;


   --------------
   --- Attributes
   --

   function is_Open          (Self : in     Item) return Boolean;
   function is_Exposed       (Self : in     Item) return Boolean;

   function Keyboard         (Self : access Item) return access gel.Keyboard.item'class;
   function Mouse            (Self : access Item) return access gel.Mouse.item'class;

   function Width            (Self : in     Item) return Positive;
   function Height           (Self : in     Item) return Positive;

   function is_being_Resized (Self : in Item'Class) return Boolean;

   function Surface          (Self : in Item) return openGL.Surface.view;


   --------------
   --- Operations
   --

   procedure emit_Events  (Self : in out Item)   is null;
   procedure enable_GL    (Self : in     Item)   is null;
   procedure disable_GL   (Self : in     Item)   is null;
   procedure swap_GL      (Self : in out Item)   is null;


   ----------
   --  Events
   --

   procedure emit_enter_Event             (Self : in out Item'Class);
   procedure emit_leave_Event             (Self : in out Item'Class);

   procedure emit_focus_in_Event          (Self : in out Item'Class);
   procedure emit_focus_out_Event         (Self : in out Item'Class);

   procedure emit_keymap_notify_Event     (Self : in out Item'Class);
   procedure emit_Expose_Event            (Self : in out Item'Class);
   procedure emit_graphics_Exposure_Event (Self : in out Item'Class);
   procedure emit_no_Exposure_Event       (Self : in out Item'Class);
   procedure emit_visibility_Notify_Event (Self : in out Item'Class);
   procedure emit_create_Notify_Event     (Self : in out Item'Class);
   procedure emit_destroy_Notify_Event    (Self : in out Item'Class);
   procedure emit_unmap_Notify_Event      (Self : in out Item'Class);
   procedure emit_map_Notify_Event        (Self : in out Item'Class);
   procedure emit_map_Request_Event       (Self : in out Item'Class);
   procedure emit_reparent_Notify_Event   (Self : in out Item'Class);
   procedure emit_configure_Notify_Event  (Self : in out Item'Class);
   procedure emit_configure_Request_Event (Self : in out Item'Class);
   procedure emit_gravity_Notify_Event    (Self : in out Item'Class);
   procedure emit_resize_Request_Event    (Self : in out Item'Class;   Width, Height : in Positive);
   procedure emit_circulate_Notify_Event  (Self : in out Item'Class);
   procedure emit_circulate_Request_Event (Self : in out Item'Class);
   procedure emit_property_Notify_Event   (Self : in out Item'Class);
   procedure emit_selection_Clear_Event   (Self : in out Item'Class);
   procedure emit_selection_Request_Event (Self : in out Item'Class);
   procedure emit_selection_Notify_Event  (Self : in out Item'Class);
   procedure emit_colormap_Notify_Event   (Self : in out Item'Class);
   procedure emit_client_Message_Event    (Self : in out Item'Class);
   procedure emit_mapping_Notify_Event    (Self : in out Item'Class);



private

   type String_view is access all String;


   type Item is limited new lace.Subject_and_deferred_Observer.item with
      record
         Width      : Positive;
         Height     : Positive;

         Surface    : openGL.Surface.view := new openGL.Surface.item;

         Keyboard   : gel.Keyboard.local.view;
         Mouse      : gel.Mouse   .local.view;

         is_Open    : Boolean := True;
         is_Exposed : Boolean := True;

         last_resize_Time : ada.Calendar.Time;
      end record;


   procedure Size_is (Self : in out Item;   Width, Height : in Positive);


   package private_Forge
   is
      function to_Window (Name   : in String;
                          Width  : in Positive;
                          Height : in Positive) return Item;
   end private_Forge;


   type create_Window_Function is access function (Name   : in String;
                                                   Width  : in Positive;
                                                   Height : in Positive) return View;

   procedure use_create_Window (create_Window : in create_Window_Function);


end gel.Window;
