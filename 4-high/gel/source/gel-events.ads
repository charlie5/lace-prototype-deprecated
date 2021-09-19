with
     gel.remote.World,
     gel.Mouse,
     lace.Event;

package gel.Events with remote_Types
--
-- Provides events for GEL.
--
is


   type window_Enter             is new lace.Event.item with null record;
   type window_Leave             is new lace.Event.item with null record;

   type window_Focus_In          is new lace.Event.item with null record;
   type window_Focus_Out         is new lace.Event.item with null record;

   type window_keymap_Notify     is new lace.Event.item with null record;

   type window_Expose            is new lace.Event.item with null record;
   type window_graphics_Exposure is new lace.Event.item with null record;
   type window_no_Exposure       is new lace.Event.item with null record;

   type window_visibility_Notify is new lace.Event.item with null record;
   type window_create_Notify     is new lace.Event.item with null record;
   type window_destroy_Notify    is new lace.Event.item with null record;
   type window_unmap_Notify      is new lace.Event.item with null record;
   type window_map_Notify        is new lace.Event.item with null record;
   type window_map_Request       is new lace.Event.item with null record;
   type window_reparent_Notify   is new lace.Event.item with null record;
   type window_configure_Notify  is new lace.Event.item with null record;
   type window_configure_Request is new lace.Event.item with null record;
   type window_gravity_Notify    is new lace.Event.item with null record;
   type window_circulate_Notify  is new lace.Event.item with null record;
   type window_circulate_Request is new lace.Event.item with null record;
   type window_property_Notify   is new lace.Event.item with null record;
   type window_selection_Clear   is new lace.Event.item with null record;
   type window_selection_Request is new lace.Event.item with null record;
   type window_selection_Notify  is new lace.Event.item with null record;
   type window_colormap_Notify   is new lace.Event.item with null record;
   type window_client_Message    is new lace.Event.item with null record;
   type window_mapping_Notify    is new lace.Event.item with null record;

   type window_resize_Request    is new lace.Event.item with
      record
         Width, Height : Positive;
      end record;


   type new_sprite_Event is new lace.Event.item with
      record
         Pair : gel.remote.World.sprite_model_Pair;
      end record;


   type new_sprite_added_to_world_Event is new lace.Event.item with
      record
         Sprite_Id : gel.sprite_Id;
         World_Id  : gel. world_Id;
      end record;


   type my_new_sprite_added_to_world_Event is new lace.Event.item with
      record
         Pair : gel.remote.World.sprite_model_Pair;
      end record;


   type sprite_click_down_Event is new lace.Event.item with
      record
         mouse_Button : gel.Mouse.Button_Id;
         world_Site   : math.Vector_3;
      end record;


   type sprite_click_up_Event is new lace.Event.item with
      record
         mouse_Button : gel.Mouse.Button_Id;
         world_Site   : math.Vector_3;
      end record;


end gel.Events;
