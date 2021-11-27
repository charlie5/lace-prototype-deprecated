with
     gel.Events,
     ada.unchecked_Deallocation;

package body gel.Window
is
   -----------
   --- Utility
   --

   procedure free is new ada.unchecked_Deallocation (String, String_view);
   pragma Unreferenced (free);


   ----------
   --- Forge
   --

   procedure define  (Self : in out Item;   Width  : in Positive;
                                            Height : in Positive)
   is
   begin
      Self.last_resize_Time := ada.Calendar.Clock;

      Self.Width    := Width;
      Self.Height   := Height;

      Self.Keyboard := gel.Keyboard.local.Forge.new_Keyboard (of_name => Self.Name & "." & "Keyboard");
      Self.Mouse    := gel.Mouse   .local.Forge.new_Mouse    (of_name => Self.Name & "." & "Mouse");
   end define;



   window_Creator : create_Window_Function;

   package body Forge
   is
      function new_Window (Name   : in String;
                           Width  : in Positive;
                           Height : in Positive) return View
      is
      begin
         if window_Creator = null
         then
            raise Error with "'window_Creator' has not been set.";
         end if;

         return window_Creator (Name, Width, Height);
      end new_Window;

   end Forge;



   overriding
   procedure destroy (Self : in out Item)
   is
      use lace.Subject_and_deferred_Observer,
          gel.Keyboard.local,
          gel.Mouse   .local;

      procedure deallocate is new ada.unchecked_Deallocation (openGL.Surface.item'Class, openGL.Surface.View);
   begin
      destroy (lace.Subject_and_deferred_Observer.item (Self));   -- Destroy base class.

      free (Self.Keyboard);
      free (Self.Mouse);

      deallocate (Self.Surface);
   end destroy;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;



   procedure use_create_Window (create_Window : in create_Window_Function)
   is
   begin
      if window_Creator /= null
      then
         raise Error with "'window_Creator' has already been set.";
      end if;

      window_Creator := create_Window;
   end use_create_Window;



   package body private_Forge
   is
      function to_Window (Name   : in String;
                          Width  : in Positive;
                          Height : in Positive) return Item
      is
      begin
         return Self : Item := (lace.Subject_and_deferred_Observer.Forge.to_Subject_and_Observer (Name)
                                with others => <>)
         do
            Self.define (Width, Height);
         end return;
      end to_Window;
   end private_Forge;


   --------------
   --- Attributes
   --

   function Surface (Self : in Item) return openGL.Surface.view
   is
   begin
      return Self.Surface;
   end Surface;



   function Keyboard (Self : access Item) return access gel.Keyboard.item'class
   is
   begin
      return Self.Keyboard;
   end Keyboard;



   function Mouse (Self : access Item) return access gel.Mouse.item'class
   is
   begin
      return Self.Mouse;
   end Mouse;



   function is_Open  (Self : in Item) return Boolean
   is
   begin
      return Self.is_Open;
   end is_Open;



   function is_Exposed (Self : in Item) return Boolean
   is
   begin
      return Self.is_Exposed;
   end is_Exposed;



   function Width (Self : in Item) return Positive
   is
   begin
      return Self.Width;
   end Width;



   function Height (Self : in Item) return Positive
   is
   begin
      return Self.Height;
   end Height;



   function is_being_Resized (Self : in Item'Class) return Boolean
   is
      use ada.Calendar;
   begin
      return ada.Calendar.Clock - Self.last_resize_Time < 0.1;
   end is_being_Resized;



   procedure Size_is (Self : in out Item;   Width, Height : in Positive)
   is
   begin
      Self.last_resize_Time := Ada.Calendar.Clock;

      Self.Width  := Width;
      Self.Height := Height;

      --  Generate a 'resize' event.
      --
      Self.emit (gel.Events.window_resize_Request' (Width, Height));
   end Size_is;


   ---------------
   ---  Operations
   --

   procedure flush (Self : in Item)
   is
   begin
      null;
   end flush;
   pragma Unreferenced (flush);



   procedure sync (Self : in Item)
   is
   begin
      null;
   end sync;
   pragma Unreferenced (sync);


   ----------
   --- Events
   --

   procedure emit_enter_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_Enter;
   begin
      Self.emit (the_Event);
   end emit_enter_Event;



   procedure emit_leave_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_Leave;
   begin
      Self.emit (the_Event);
   end emit_leave_Event;



   procedure emit_focus_in_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_Leave;
   begin
      Self.emit (the_Event);
   end emit_focus_in_Event;



   procedure emit_focus_out_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_Leave;
   begin
      Self.emit (the_Event);
   end emit_focus_out_Event;



   procedure emit_keymap_notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_keymap_Notify;
   begin
      Self.emit (the_Event);
   end emit_keymap_notify_Event;



   procedure emit_Expose_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_Expose;
   begin
      Self.emit (the_Event);
   end emit_Expose_Event;



   procedure emit_graphics_Exposure_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_graphics_Exposure;
   begin
      Self.emit (the_Event);
   end emit_graphics_Exposure_Event;



   procedure emit_no_Exposure_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_no_Exposure;
   begin
      Self.emit (the_Event);
   end emit_no_Exposure_Event;



   procedure emit_visibility_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_visibility_Notify;
   begin
      Self.emit (the_Event);
   end emit_visibility_Notify_Event;



   procedure emit_create_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_create_Notify;
   begin
      Self.emit (the_Event);
   end emit_create_Notify_Event;



   procedure emit_destroy_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_destroy_Notify;
   begin
      Self.emit (the_Event);
   end emit_destroy_Notify_Event;



   procedure emit_unmap_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_unmap_Notify;
   begin
      Self.emit (the_Event);
   end emit_unmap_Notify_Event;



   procedure emit_map_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_map_Notify;
   begin
      Self.emit (the_Event);
   end emit_map_Notify_Event;



   procedure emit_map_Request_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_map_Request;
   begin
      Self.emit (the_Event);
   end emit_map_Request_Event;



   procedure emit_reparent_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_reparent_Notify;
   begin
      Self.emit (the_Event);
   end emit_reparent_Notify_Event;



   procedure emit_configure_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_configure_Notify;
   begin
      Self.emit (the_Event);
   end emit_configure_Notify_Event;



   procedure emit_configure_Request_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_configure_Request;
   begin
      Self.emit (the_Event);
   end emit_configure_Request_Event;



   procedure emit_gravity_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_gravity_Notify;
   begin
      Self.emit (the_Event);
   end emit_gravity_Notify_Event;



   procedure emit_resize_Request_Event (Self : in out Item'Class;   Width, Height : in Positive)
   is
      the_Event : constant gel.Events.window_resize_Request := (Width, Height);
   begin
      Self.emit (the_Event);
   end emit_resize_Request_Event;



   procedure emit_circulate_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_circulate_Notify;
   begin
      Self.emit (the_Event);
   end emit_circulate_Notify_Event;



   procedure emit_circulate_Request_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_circulate_Request;
   begin
      Self.emit (the_Event);
   end emit_circulate_Request_Event;



   procedure emit_property_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_property_Notify;
   begin
      Self.emit (the_Event);
   end emit_property_Notify_Event;



   procedure emit_selection_Clear_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_selection_Clear;
   begin
      Self.emit (the_Event);
   end emit_selection_Clear_Event;



   procedure emit_selection_Request_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_selection_Request;
   begin
      Self.emit (the_Event);
   end emit_selection_Request_Event;



   procedure emit_selection_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_selection_Notify;
   begin
      Self.emit (the_Event);
   end emit_selection_Notify_Event;



   procedure emit_colormap_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_colormap_Notify;
   begin
      Self.emit (the_Event);
   end emit_colormap_Notify_Event;



   procedure emit_client_Message_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_client_Message;
   begin
      Self.emit (the_Event);
   end emit_client_Message_Event;



   procedure emit_mapping_Notify_Event (Self : in out Item'Class)
   is
      the_Event : gel.Events.window_mapping_Notify;
   begin
      Self.emit (the_Event);
   end emit_mapping_Notify_Event;


end gel.Window;
