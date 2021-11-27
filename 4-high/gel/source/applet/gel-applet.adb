with
     gel.World.simple,
     gel.Dolly.simple,
     gel.Dolly.following,
     gel.Camera.forge,
     gel.Joint,
     gel.Events,

     openGL.Palette,
     openGL.Renderer.lean.forge,

     lace.Any,
     lace.Event.utility,

     ada.unchecked_Conversion,
     ada.unchecked_Deallocation,
     ada.Text_IO;

use ada.Text_IO;


package body gel.Applet
is
   use lace.Event.utility;


   procedure my_context_Setter
   is
   begin
      global_Window.enable_GL;
   end my_context_Setter;


   procedure my_Swapper
   is
   begin
      global_Window.swap_GL;
   end my_Swapper;


   overriding
   procedure respond (Self : in out add_new_Sprite;   to_Event : in lace.Event.item'Class)
   is
      the_Event : constant gel.events.new_sprite_added_to_world_Event
        := gel.events.new_sprite_added_to_world_Event (to_Event);

      the_Sprite : gel.Sprite.view;

   begin
      the_Sprite := Self.Applet.World (the_Event.World_Id).fetch_Sprite (the_event.Sprite_Id);

      the_Sprite.is_Visible (True);
      Self.Applet.add (the_Sprite);

   exception
      when constraint_Error =>
         put_Line ("Exception in 'add_new_Sprite' response.");
   end respond;


   overriding
   function Name (Self : in add_new_Sprite) return String
   is
      pragma unreferenced (Self);
   begin
      return "add_new_Sprite";
   end Name;



   procedure define (Self : in View;   use_Window : in gel.Window.view)
   is
   begin
      Self.Window   := use_Window;
      global_Window := use_Window;

      -- Add window resize event repsonse.
      --
      Self.local_Subject_and_Observer.add (Self.resize_Response'unchecked_Access,
                                           to_Kind (gel.events.window_resize_Request'Tag),
                                           use_Window.Name);

      Self.Window.register (lace.Observer.view (Self.local_Subject_and_Observer),
                            to_Kind (gel.events.window_resize_Request'Tag));

      Self.resize_Response.Applet := Self;

      -- Setup the renderer.
      --
      Self.Renderer := openGL.Renderer.lean.forge.new_Renderer;

      Self.Renderer.Background_is (openGL.Palette.Grey);
      Self.Renderer.Swapper_is    (my_Swapper'unrestricted_Access);

      Self.Window.disable_GL;

      Self.Renderer.Context_Setter_is (my_context_Setter'unrestricted_Access);
      Self.Renderer.start_Engine;

      Self.Renderer.add_Font (Self.       Font);
      Self.Renderer.add_Font (Self.titles_Font);

      -- Set up the keyboard events.
      --
      Self.Keyboard := Self.Window.Keyboard;

      Self.Mouse                          := Self.Window.Mouse;
      Self.button_press_Response  .Applet := Self;
      Self.button_release_Response.Applet := Self;
      Self.mouse_motion_Response  .Applet := Self;

      -- Add the new sprite event response.
      --
      the_add_new_sprite_Response.Applet := Self;

   end define;


   overriding
   procedure destroy (Self : in out Item)
   is
      use world_Vectors,
          gel.Dolly,
          openGL.Renderer.lean,
          gel.Window,
          gel.World;

      procedure free is new ada.unchecked_Deallocation (world_Info, world_Info_view);

      Cursor     : world_Vectors.Cursor := Self.Worlds.First;

      world_Info : world_Info_view;
      the_World  : gel.World.view;

   begin
      while has_Element (Cursor)
      loop
         world_Info := Element (Cursor);

         -- Free the world.
         --
         the_World := world_Info.World;
         the_World.destroy;
         free (the_World);

         -- Free the cameras.
         --
         declare
            use gel.Camera;

            the_Cameras : camera_Vector renames world_Info.Cameras;
            the_Camera  : gel.Camera.view;
         begin
            for i in 1 .. Integer (the_Cameras.Length)
            loop
               the_Camera := the_Cameras.Element (i);
               free (the_Camera);
            end loop;
         end;

         free (world_Info);
         next (Cursor);
      end loop;

      free (Self.Dolly);
      free (Self.Renderer);
      free (Self.Window);

      Self.local_Subject_and_Observer.destroy;
      lace.Subject_and_deferred_Observer.item (Self).destroy;     -- Destroy base class.
   end destroy;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Applet.item'Class, Applet.view);
   begin
      Self.destroy;
      deallocate (Self);
   end free;


   ---------
   --- Forge
   --

   package body Forge
   is

      function to_Applet (Name       : in String;
                          use_Window : in gel.Window.view) return Item
      is
         use lace.Subject_and_deferred_Observer.Forge;
      begin
         return Self : Item := (to_Subject_and_Observer (Name) with
                                local_Subject_and_Observer => new_Subject_and_Observer (Name),
                                others                     => <>)
         do
            define (Self'unchecked_Access, use_Window);
         end return;
      end to_Applet;



      function new_Applet (Name       : in String;
                           use_Window : in gel.Window.view) return View
      is
      begin
         return new Item' (to_Applet (Name, use_Window));
      end new_Applet;

   end Forge;


   --------------
   --- Attributes
   --

   procedure add (Self : in out Item;   the_World : in world_Info_view)
   is
   begin
      Self.Worlds.append (the_World);
   end add;



   function new_World (Self : access Item;   Name       : in String;
                                             space_Kind : in physics.space_Kind) return gel.World.view
   is
   begin
      Self.add_new_World (Name, space_Kind);
      return Self.Worlds.last_Element.World;
   end new_World;



   procedure add_new_World (Self : in out Item;   Name       : in String;
                                                  space_Kind : in physics.space_Kind)
   is
      use type ada.Containers.Count_type;

      the_world_Info : constant world_Info_view  := new world_Info;
      the_Camera     : constant gel.Camera.view  := gel.Camera.forge.new_Camera;
   begin
      the_world_Info.World := gel.World.simple.forge.new_World (Name,
                                                                world_Id (Self.Worlds.Length + 1),
                                                                space_Kind,
                                                                Self.Renderer).all'Access;

      the_Camera.Viewport_is (Self.Window.Width, Self.Window.Height);
      the_Camera.Renderer_is (Self.Renderer);
      the_Camera.Site_is     ((0.0, 5.0, 50.0));

      the_world_Info.Cameras.append (the_Camera);

      Self.Worlds.append (the_world_Info);

      Self.local_Subject_and_Observer.add (the_add_new_sprite_Response'Access,
                                           to_Kind (gel.Events.new_sprite_added_to_world_Event'Tag),
                                           the_world_Info.World.Name);
      the_world_Info.World.start;

      Self.add (the_world_Info);
   end add_new_World;



   function is_Open (Self : in Item) return Boolean
   is
   begin
      return    Self.Window.is_Open
        and not Self.quit_Requested;
   end is_Open;



   function Window (Self : in Item) return gel.Window.view
   is
   begin
      return Self.Window;
   end Window;



   function world_Count (Self : in Item) return Natural
   is
   begin
      return Natural (Self.Worlds.Length);
   end world_Count;



   function Worlds (Self : in Item) return gel.World.views
   is
      the_Worlds : gel.World.views (1 .. Natural (Self.Worlds.Length));
   begin
      for i in the_Worlds'Range
      loop
         the_Worlds (i) := Self.Worlds.Element (i).World;
      end loop;

      return the_Worlds;
   end Worlds;



   function World (Self : in Item;   Id : in world_Id := 1) return gel.World.view
   is
   begin
      return Self.Worlds.Element (Integer (Id)).World;
   end World;



   function World_as_iFace (Self : in Item;   Id : in world_Id := 1) return gel.remote.World.view
   is
   begin
      return remote.World.view (Self.Worlds.Element (Integer (Id)).World);
   end World_as_iFace;



   function Camera (Self : in Item;   world_Id  : in gel.world_Id  := 1;
                                      camera_Id : in gel.camera_Id := 1) return gel.Camera.view
   is
      w : constant Integer := Integer ( world_Id);
      c : constant Integer := Integer (camera_Id);
   begin
      return Self.Worlds.Element (w).Cameras.Element (c);
   end Camera;



   function Font (Self : in Item) return opengl.Font.font_Id
   is
   begin
      return Self.Font;
   end Font;



   function titles_Font (Self : in Item) return opengl.Font.font_Id
   is
   begin
      return Self.titles_Font;
   end titles_Font;



   function Renderer (Self : in Item) return openGL.Renderer.lean.view
   is
   begin
      return Self.Renderer;
   end Renderer;



   function Keyboard (Self : in Item) return access gel.Keyboard.item'Class
   is
   begin
      return Self.Keyboard;
   end Keyboard;



   function Mouse (Self : in Item) return access gel.Mouse.item'Class
   is
   begin
      return Self.Mouse;
   end Mouse;



   function Dolly (Self : access Item) return gel.Dolly.view
   is
   begin
      return Self.Dolly;
   end Dolly;



   function last_Keypress (Self : access Item) return gel.Keyboard.Key
   is
      the_last_Keypress : constant gel.Keyboard.Key := Self.last_pressed_Key;
   begin
      Self.last_pressed_Key := gel.Keyboard.Nil;
      return the_last_Keypress;
   end last_Keypress;


   --------------
   --- Operations
   --

   procedure evolve_all_Worlds (Self : in out Item;   By : in Duration)
   is
      use world_Vectors;
      world_Cursor : world_Vectors.Cursor := Self.Worlds.First;

   begin
      while has_Element (world_Cursor)
      loop
         declare
            the_world_Info : world_Info renames Element (world_Cursor).all;
         begin
            the_world_Info.World.evolve;
         end;

         next (world_Cursor);
      end loop;
   end evolve_all_Worlds;



   procedure freshen (Self : in out Item)
   is
      use type gel.Dolly.view;

      Window_is_active : Boolean;

   begin
      Self.Window.emit_Events;
      Self.Window.swap_GL;

      Self                           .respond;
      Self.local_Subject_and_Observer.respond;
      Self.Window                    .respond;

      if Self.Dolly /= null then
         Self.Dolly.freshen;
      end if;

      Window_is_active :=              Self.Window.is_Open
                          and then     Self.Window.is_Exposed
                          and then not Self.Window.is_being_Resized;
      declare
         use world_Vectors;
         world_Cursor     : world_Vectors.Cursor        := Self.Worlds.First;
         all_Cameras      : gel.Camera.views (1 .. 1000);
         all_cameras_Last : Natural                     := 0;

      begin
         while has_Element (world_Cursor)
         loop
            declare
               use camera_Vectors;
               the_world_Info : world_Info       renames Element (world_Cursor).all;
               camera_Cursor  : camera_Vectors.Cursor := the_world_Info.Cameras.First;
            begin
               --  the_world_Info.World.wait_on_evolve;
               the_world_Info.World.evolve;

               if Window_is_active
               then
                  while has_Element (camera_Cursor)
                  loop
                     all_cameras_Last               := all_cameras_Last + 1;
                     all_Cameras (all_cameras_Last) := Element (camera_Cursor);
                     Element (camera_Cursor).render (the_world_Info.World,
                                                     To => Self.Window.Surface);
                     next (camera_Cursor);
                  end loop;
               end if;
            end;

            next (world_Cursor);
         end loop;

         loop
            declare
               culls_Completed : Boolean := True;
            begin
               for i in 1 .. all_cameras_Last
               loop
                  culls_Completed :=     culls_Completed
                                     and all_Cameras (i).cull_Completed;
               end loop;

               exit when culls_Completed;
               delay Duration'Small;
            end;
         end loop;
      end;

      Self.Renderer.render;
   end freshen;



   procedure add (Self : in out Item;   the_Sprite : in gel.Sprite.view)
   is
      child_Joints : constant gel.Joint.views := the_Sprite.child_Joints;
   begin
      --  Add children and their joints.
      --
      for i in child_Joints'Range
      loop
         Self          .add (the_Sprite.child_Joints (i).Sprite_B);
         Self.World (1).add (the_Sprite.child_Joints (i));
      end loop;
   end add;



   procedure add (Self : in out Item;   the_Sprite : in gel.Sprite.view;
                                        at_site    : in Vector_3)
   is
   begin
      the_Sprite.Site_is (at_site);
      Self.add (the_Sprite);
   end add;



   procedure take_Screenshot (Self : in out Item;   Filename : in String)
   is
   begin
      Self.Renderer.Screenshot (Filename);
   end take_Screenshot;



   procedure request_Quit (Self : in out Item)
   is
   begin
      Self.quit_Requested := True;
   end request_Quit;



   procedure toggle_video_Capture (Self : in out Item'Class)
   is
   begin
      raise Error with "TODO";
   end toggle_video_Capture;


   ----------------------
   --  Keyboard Responses
   --

   overriding
   procedure respond (Self : in out key_press_Response;   to_Event : in lace.Event.item'Class)
   is
      use gel.Keyboard,
          gel.Dolly;

      the_Event     :          gel.Keyboard.key_press_Event renames gel.Keyboard.key_press_Event (to_Event);
      the_Dolly     : constant gel.Dolly.view               :=      Self.Applet.Dolly;

      the_Key       : constant gel.keyboard.Key             :=      the_Event.modified_Key.Key;
      the_Modifiers : constant gel.Keyboard.modifier_Set    :=      the_Event.modified_Key.modifier_Set;

   begin
      Self.Applet.last_pressed_Key := the_Event.modified_Key.Key;

      if the_Key = ESCAPE
      then
         Self.Applet.quit_Requested := True;
      end if;

      if the_Dolly /= null
      then
         if the_Modifiers (lShift)
         then   the_Dolly.speed_Multiplier_is (6.0);
         else   the_Dolly.speed_Multiplier_is (1.0);
         end if;


         if the_Modifiers (lCtrl)
         then
            if    the_Key = up       then   the_Dolly.is_spinning (Forward);
            elsif the_Key = down     then   the_Dolly.is_spinning (Backward);
            elsif the_Key = left     then   the_Dolly.is_spinning (Left);
            elsif the_Key = right    then   the_Dolly.is_spinning (Right);
            elsif the_Key = pageUp   then   the_Dolly.is_spinning (Up);
            elsif the_Key = pageDown then   the_Dolly.is_spinning (Down);
            end if;

         elsif the_Modifiers (lAlt)
         then
            if    the_Key = up       then   the_Dolly.is_orbiting (Forward);
            elsif the_Key = down     then   the_Dolly.is_orbiting (Backward);
            elsif the_Key = left     then   the_Dolly.is_orbiting (Left);
            elsif the_Key = right    then   the_Dolly.is_orbiting (Right);
            elsif the_Key = pageUp   then   the_Dolly.is_orbiting (Up);
            elsif the_Key = pageDown then   the_Dolly.is_orbiting (Down);
            end if;

         else
            if    the_Key = up       then   the_Dolly.is_moving (Forward);
            elsif the_Key = down     then   the_Dolly.is_moving (Backward);
            elsif the_Key = left     then   the_Dolly.is_moving (Left);
            elsif the_Key = right    then   the_Dolly.is_moving (Right);
            elsif the_Key = pageUp   then   the_Dolly.is_moving (Up);
            elsif the_Key = pageDown then   the_Dolly.is_moving (Down);
            elsif the_Key = F11      then   Self.Applet.take_Screenshot ("./screenshot.bmp");
            elsif the_Key = F12      then   Self.Applet.toggle_video_Capture;
            end if;
         end if;
      end if;

      if    the_Modifiers (lCtrl)
      then
         null;

      elsif the_Modifiers (lAlt)
      then
         null;

      else
         if    the_Key = F11 then   Self.Applet.take_Screenshot  ("./screenshot.bmp");
         elsif the_Key = F12 then   Self.Applet.toggle_video_Capture;
         end if;
      end if;

   end respond;


   overriding
   procedure respond (Self : in out key_release_Response;   to_Event : in lace.Event.Item'Class)
   is
      use gel.Keyboard, gel.Dolly;

      the_Event     :          gel.Keyboard.key_release_Event renames gel.Keyboard.key_release_Event (to_Event);
      the_Dolly     :          gel.Dolly.view                 renames Self.Applet.Dolly;

      the_Key       : constant gel.keyboard.Key               :=      the_Event.modified_Key.Key;
      the_Modifiers : constant gel.Keyboard.modifier_Set      :=      the_Event.modified_Key.modifier_Set;
      pragma Unreferenced (the_Modifiers);
   begin
      if the_Dolly = null
      then
         return;
      end if;

      if the_Key = up
      then
         the_Dolly.is_moving   (Forward,  False);
         the_Dolly.is_spinning (Forward,  False);
         the_Dolly.is_orbiting (Forward,  False);

      elsif the_Key = down
      then
         the_Dolly.is_moving   (Backward,  False);
         the_Dolly.is_spinning (Backward,  False);
         the_Dolly.is_orbiting (Backward,  False);

      elsif the_Key = left
      then
         the_Dolly.is_moving   (Left,  False);
         the_Dolly.is_spinning (Left,  False);
         the_Dolly.is_orbiting (Left,  False);

      elsif the_Key = right
      then
         the_Dolly.is_moving   (Right,  False);
         the_Dolly.is_spinning (Right,  False);
         the_Dolly.is_orbiting (Right,  False);

      elsif the_Key = pageUp
      then
         the_Dolly.is_moving   (Up,  False);
         the_Dolly.is_spinning (Up,  False);
         the_Dolly.is_orbiting (Up,  False);

      elsif the_Key = pageDown
      then
         the_Dolly.is_moving   (Down,  False);
         the_Dolly.is_spinning (Down,  False);
         the_Dolly.is_orbiting (Down,  False);
      end if;

   end respond;



   procedure Dolly_is (Self : access Item;   Now : in gel.Dolly.view)
   is
   begin
      Self.Dolly := Now;
   end Dolly_is;



   procedure enable_simple_Dolly (Self : access Item;   in_World : in world_Id)
   is
   begin
      Self.Dolly := new gel.Dolly.simple.item;
      Self.Dolly.add_Camera (Self.Camera (in_World, 1));

      Self.key_press_Response  .Applet := gel.Applet.view (Self);
      Self.key_release_Response.Applet := gel.Applet.view (Self);

      lace.Event.utility.connect (lace.Observer.view (Self.local_Subject_and_Observer),
                                  lace.Subject .view (Self.Keyboard),
                                  Self.key_press_Response'unchecked_Access,
                                  to_Kind (gel.Keyboard.key_press_Event'Tag));

      lace.Event.utility.connect (lace.Observer.view (Self.local_Subject_and_Observer),
                                  lace.Subject .view (Self.Keyboard),
                                  Self.key_release_Response'unchecked_Access,
                                  to_Kind (gel.Keyboard.key_release_Event'Tag));
   end enable_simple_Dolly;



   procedure enable_following_Dolly (Self : access Item;   Follow : in gel.Sprite.view)
   is
      the_Dolly : constant gel.Dolly.following.view := new gel.Dolly.following.item;
   begin
      the_Dolly.follow (the_Sprite => Follow);

      Self.Dolly := the_Dolly.all'Access;
      Self.Dolly.add_Camera (Self.Camera (1, 1));
   end enable_following_Dolly;


   --------------------------
   --- Mouse Button Responses
   --

   type button_press_raycast_Context is new lace.Any.limited_item with
      record
         is_Motion : Boolean;
         is_Press  : Boolean;
         button_Id : gel.mouse.Button_Id;
      end record;

   type button_press_raycast_Context_view is access all button_press_raycast_Context'Class;


   overriding
   procedure respond (Self : in out mouse_click_raycast_Response;   to_Event : in lace.Event.item'Class)
   is
      use gel.World;

      the_Event   :          raycast_collision_Event           := raycast_collision_Event (to_Event);
      the_Context : constant button_press_raycast_Context_view := button_press_raycast_Context_view (the_Event.Context);
   begin
      if the_Context.is_Motion
      then
         null;

      else
         if the_Context.is_Press
         then
            declare
               collide_Event : constant gel.events.sprite_click_down_Event := (mouse_Button => the_Context.button_Id,
                                                                               world_Site   => the_Event.Site_world);
            begin
               the_Event.near_Sprite.receive (collide_Event, Self.Applet.Name);
            end;

         else   -- Is a button release.
            declare
               collide_Event : constant gel.events.sprite_click_up_Event := (mouse_Button => the_Context.button_Id,
                                                                             world_Site   => the_Event.Site_world);
            begin
               the_Event.near_Sprite.receive (collide_Event, Self.Applet.Name);
            end;
         end if;
      end if;

      the_Event.destruct;
   end respond;


   type mouse_button_collision_Event is new gel.World.raycast_collision_Event with null record;


   overriding
   procedure respond (Self : in out button_press_Response;   to_Event : in lace.Event.item'Class)
   is
      use world_Vectors,
          gel.Mouse;

      the_Event       : gel.mouse.button_press_Event renames gel.Mouse.button_press_Event (to_Event);
      Cursor          : world_Vectors.Cursor         :=      Self.Applet.Worlds.First;
      the_world_Info  : world_Info_view;

   begin
      while has_Element (Cursor)
      loop
         the_world_Info := Element (Cursor);

         declare
            use gel.World;

            the_Camera : constant gel.Camera.view := the_world_Info.Cameras.first_Element;

            Site_window_space : constant Vector_3 := (Real (the_Event.Site (1)),  Real (the_Event.Site (2)),  1.0);
            Site_world_space  : constant Vector_3 := the_Camera.to_world_Site (Site_window_space);

            the_Context : constant access button_press_raycast_Context := new button_press_raycast_Context;
            event_Kind  :                 mouse_button_collision_Event;

         begin
            the_Context.is_Motion := False;
            the_Context.is_Press  := True;
            the_Context.button_Id := the_Event.Button;

            the_world_Info.World.cast_Ray (From     => the_Camera.Site,
                                           To       => Site_world_space,
                                           Observer => lace.Observer.view (Self.Applet.local_Subject_and_Observer),
                                           Context  => the_Context,
                                           event_Kind => event_Kind);
         end;

         next (Cursor);
      end loop;
   end respond;



   overriding
   procedure respond (Self : in out button_release_Response;   to_Event : in lace.Event.item'Class)
   is
      use world_Vectors,
          gel.Mouse;

      the_Event      : gel.Mouse.button_release_Event renames gel.Mouse.button_release_Event (to_Event);
      Cursor         : world_Vectors.Cursor           :=      Self.Applet.Worlds.First;
      the_world_Info : world_Info_view;

   begin
      while has_Element (Cursor)
      loop
         the_world_Info := Element (Cursor);

         declare
            the_Camera : constant gel.Camera.view := the_world_Info.Cameras.first_Element;

            Site_window_space : constant Vector_3 := (Real (the_Event.Site (1)),  Real (the_Event.Site (2)),  1.0);
            Site_world_space  : constant Vector_3 := the_Camera.to_world_Site (Site_window_space);

            the_Context : constant access button_press_raycast_Context := new button_press_raycast_Context;
            event_Kind  :                 mouse_button_collision_Event;

         begin
            the_Context.is_Motion := False;
            the_Context.is_Press  := False;
            the_Context.button_Id := the_Event.Button;

            the_world_Info.World.cast_Ray (From     => the_Camera.Site,
                                           To       => Site_world_space,
                                           Observer => lace.Observer.view (Self.Applet.local_Subject_and_Observer),
                                           Context  => the_Context,
                                           event_Kind => event_Kind);
         end;

         next (Cursor);
      end loop;
   end respond;



   overriding
   procedure respond (Self : in out mouse_motion_Response;   to_Event : in lace.Event.Item'Class)
   is
      use world_Vectors;

      the_Event      : gel.mouse.motion_Event renames gel.mouse.motion_Event (to_Event);
      Cursor         : world_Vectors.Cursor   :=      Self.Applet.Worlds.First;
      the_world_Info : world_Info_view;

   begin
      while has_Element (Cursor)
      loop
         the_world_Info := Element (Cursor);

         declare
            the_Camera : constant gel.Camera.view := the_world_Info.Cameras.first_Element;

            Site_window_space : constant Vector_3 := (Real (the_Event.Site (1)),  Real (the_Event.Site (2)),  1.0);
            Site_world_space  : constant Vector_3 := the_Camera.to_world_Site (Site_window_space);
            pragma Unreferenced (Site_world_space);

            the_Context : constant access button_press_raycast_Context := new button_press_raycast_Context;

         begin
            the_Context.is_Motion := True;
         end;

         next (Cursor);
      end loop;

   end respond;


   --------------------------
   --- Window Resize Response
   --

   overriding
   procedure respond (Self : in out resize_event_Response;   to_Event : in lace.Event.item'Class)
   is
      pragma unreferenced (to_Event);
      use world_Vectors;

      Cursor         : world_Vectors.Cursor := Self.Applet.Worlds.First;
      the_world_Info : world_Info_view;
   begin
      while has_Element (Cursor)
      loop
         the_world_Info := Element (Cursor);

         declare
            the_Camera : constant gel.Camera.view := the_world_Info.Cameras.first_Element;
         begin
            the_Camera.Viewport_is (Self.Applet.Window.Width,
                                    Self.Applet.Window.Height);
         end;

         next (Cursor);
      end loop;
   end respond;


   ---------
   --- Mouse
   --

   procedure enable_Mouse (Self : access Item;   detect_Motion : in Boolean)
   is
   begin
      Self.local_Subject_and_Observer.add (Self.button_press_Response'unchecked_Access,
                                           to_Kind (gel.Mouse.button_press_Event'Tag),
                                           Self.Mouse.Name);

      Self.local_Subject_and_Observer.add (Self.button_release_Response'unchecked_Access,
                                           to_Kind (gel.Mouse.button_release_Event'Tag),
                                           Self.Mouse.Name);

      Self.Mouse.register (lace.Observer.view (Self.local_Subject_and_Observer),  to_Kind (gel.Mouse.button_press_Event  'Tag));
      Self.Mouse.register (lace.Observer.view (Self.local_Subject_and_Observer),  to_Kind (gel.Mouse.button_release_Event'Tag));

      if detect_Motion
      then
         lace.Event.Utility.connect (lace.Observer.view (Self.local_Subject_and_Observer),
                                     lace.Subject.view  (Self.Mouse),
                                     Self.mouse_motion_Response'unchecked_Access,
                                     to_Kind (gel.Mouse.motion_Event'Tag));
      end if;

      Self.mouse_click_raycast_Response.Applet := Self.all'unchecked_Access;

      declare
         use world_Vectors;

         Cursor         : world_Vectors.Cursor := Self.Worlds.First;
         the_world_Info : world_Info_view;
      begin
         while has_Element (Cursor)
         loop
            the_world_Info := Element (Cursor);

            Self.local_Subject_and_Observer.add (the_Response => Self.mouse_click_raycast_Response'unchecked_Access,
                                                 to_Kind      => lace.event.Utility.to_Kind (mouse_button_collision_Event'Tag),
                                                 from_Subject => the_world_Info.World.Name);
            next (Cursor);
         end loop;
      end;

   end enable_Mouse;


   ----------------
   --- Local Events
   --

   function local_Subject_and_Observer (Self : access Item) return lace.Subject_and_deferred_Observer.view
   is
   begin
      return Self.local_Subject_and_Observer;
   end local_Subject_and_Observer;


   function local_Subject (Self : access Item) return lace.Subject.view
   is
   begin
      return lace.Subject.view (Self.local_Subject_and_Observer);
   end local_Subject;


   function local_Observer (Self : access Item) return lace.Observer.view
   is
   begin
      return lace.Observer.view (Self.local_Subject_and_Observer);
   end local_Observer;


end gel.Applet;
