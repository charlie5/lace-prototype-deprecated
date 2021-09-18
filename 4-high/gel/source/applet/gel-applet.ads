with
     gel.remote.World,
     gel.World,
     gel.Camera,
     gel.Keyboard,
     gel.Mouse,
     gel.Sprite,
     gel.Dolly,
     gel.Window,

     openGL.Renderer.lean,
     opengl.Font,

     lace.Event,
     lace.Response,
     lace.Subject,
     lace.Observer,
     lace.Subject_and_deferred_Observer,

     ada.Containers.Vectors;

package gel.Applet
--
--  Provides an application model, configured with a single window.
--
is
   type Item is limited new lace.Subject_and_deferred_Observer.item with private;
   type View is access all Item'Class;


   ----------
   --- Forge
   --

   package Forge
   is
      function  to_Applet (Name       : in String;
                           use_Window : in gel.Window.view) return Item;

      function new_Applet (Name       : in String;
                           use_Window : in gel.Window.view) return View;
   end Forge;

   overriding
   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);


   ---------------
   --- Attributes
   --

   function is_Open        (Self : in     Item) return        Boolean;

   function Window         (Self : in     Item) return        gel.Window.view;
   function Renderer       (Self : in     Item) return        openGL.Renderer.lean.view;
   function Keyboard       (Self : in     Item) return access gel.Keyboard.item'Class;
   function Mouse          (Self : in     Item) return access gel.Mouse   .item'Class;
   function Dolly          (Self : access Item) return        gel.Dolly.view;

   function last_Keypress  (Self : access Item) return        gel.Keyboard.Key;


   function world_Count    (Self : in     Item) return Natural;
   function Worlds         (Self : in     Item) return gel.World.views;
   function World          (Self : in     Item;   Id : in world_Id := 1) return gel.World.view;
   function World_as_iFace (Self : in     Item;   Id : in world_Id := 1) return gel.remote.World.view;

   function Camera         (Self : in     Item;   world_Id  : in gel.world_Id  := 1;
                                                  camera_Id : in gel.camera_Id := 1) return gel.Camera.view;

   function        Font    (Self : in     Item) return opengl.Font.font_Id;
   function titles_Font    (Self : in     Item) return opengl.Font.font_Id;


   ---------------------------------
   --- Add a new world and camera(s)
   --

   use type gel.Camera.view;
   package camera_Vectors is new ada.Containers.Vectors (Positive, gel.Camera.view);
   subtype camera_Vector  is     camera_Vectors.Vector;

   type world_Info is
      record
         World   : gel.World.view;
         Cameras : camera_Vector;
      end record;

   type world_Info_view is access all world_Info;

   procedure add (Self : in out Item;   the_World : in world_Info_view);

   procedure add_new_World (Self : in out Item;   Name       : in String;
                                                  space_Kind : in physics.space_Kind);

   function  new_World     (Self : access Item;   Name       : in String;
                                                  space_Kind : in physics.space_Kind) return gel.World.view;

   ---------------
   --- Operations
   --

   use Math;

   procedure evolve_all_Worlds (Self : in out Item;   By : in Duration);

   procedure add (Self : in out Item;   the_Sprite : in gel.Sprite.view);
   procedure add (Self : in out Item;   the_Sprite : in gel.Sprite.view;
                                        at_site    : in Vector_3);

   procedure Dolly_is               (Self : access Item;   Now      : in gel.Dolly.view);
   procedure enable_simple_Dolly    (Self : access Item;   in_World : in world_Id);
   procedure enable_following_Dolly (Self : access Item;   Follow   : in gel.Sprite.view);

   procedure enable_Mouse (Self : access Item;   detect_Motion : in Boolean);

   procedure prepare (Self : access Item) is null;
   procedure freshen (Self : in out Item);
   --
   --  processes window events and then redraws the window.

   procedure take_Screenshot (Self : in out Item;   Filename : in String);
   procedure request_Quit    (Self : in out Item);

   ----------
   --- Events
   --

   function local_Subject_and_Observer
                           (Self : access Item) return lace.Subject_and_deferred_Observer.view;
   function local_Subject  (Self : access Item) return lace.Subject.view;
   function local_Observer (Self : access Item) return lace.Observer.view;



private

   use type Sprite.view;
   package sprite_Vectors is new ada.containers.Vectors (Positive, Sprite.view);


   -------------------
   --- Event Responses
   --

   type applet_event_Response is abstract new lace.Response.item with
      record
         Applet : gel.Applet.view;
      end record;


   -- 'add_new_Sprite' Response
   --

   type add_new_Sprite is new applet_event_Response with null record;

   overriding
   function  Name    (Self : in     add_new_Sprite) return String;
   overriding
   procedure respond (Self : in out add_new_Sprite;   to_Event : in lace.Event.item'Class);

   the_add_new_sprite_Response : aliased add_new_Sprite;


   -- 'Keyboard' Responses
   --
   type key_press_Response is new applet_event_Response with null record;
   overriding
   procedure respond (Self : in out key_press_Response;   to_Event : in lace.Event.item'Class);


   type key_release_Response is new applet_event_Response with null record;
   overriding
   procedure respond (Self : in out key_release_Response; to_Event : in lace.Event.item'Class);


   -- 'Mouse' Responses
   --
   type button_press_Response is new applet_event_Response with null record;
   overriding
   procedure respond (Self : in out button_press_Response;     to_Event : in lace.Event.item'Class);

   type button_release_Response is new applet_event_Response with null record;
   overriding
   procedure respond (Self : in out button_release_Response;   to_Event : in lace.Event.item'Class);


   type mouse_motion_Response is new applet_event_Response with null record;
   overriding
   procedure respond (Self : in out mouse_motion_Response;     to_Event : in lace.Event.item'Class);


   type mouse_click_raycast_Response is new lace.Response.item with
      record
         Applet : gel.Applet.view;
      end record;

   overriding
   procedure respond (Self : in out mouse_click_raycast_Response;   to_Event : in lace.Event.item'Class);

   type mouse_click_raycast_Response_view is access all mouse_click_raycast_Response'Class;


   -- 'Screen' Resize Response
   --
   type resize_event_Response is new applet_event_Response with null record;
   overriding
   procedure respond (Self : in out resize_event_Response;   to_Event : in lace.Event.Item'Class);


   ----------------
   --- world_Vector
   --
   use type gel.World.view;
   package world_Vectors is new ada.Containers.Vectors (Positive, world_Info_view);
   subtype world_Vector  is world_Vectors.Vector;


   --------------
   -- Applet Item
   --
   type Item is limited new lace.Subject_and_deferred_Observer.item with
      record
         local_Subject_and_Observer : lace.Subject_and_deferred_Observer.view := new lace.Subject_and_deferred_Observer.item;

         Worlds : World_Vector;

         Window          :         gel.Window.view;
         resize_Response : aliased applet.resize_event_Response;

         Keyboard             : access  gel.Keyboard.item'Class;
         key_press_Response   : aliased applet.key_press_Response;
         key_release_Response : aliased applet.key_release_Response;

         Mouse                        : access  gel.Mouse.item'Class;
         button_press_Response        : aliased applet.button_press_Response;
         button_release_Response      : aliased applet.button_release_Response;
         mouse_motion_Response        : aliased applet.mouse_motion_Response;
         mouse_click_raycast_Response : aliased applet.mouse_click_raycast_Response;

         Renderer           : openGL.Renderer.lean.view;
         Font               : opengl.Font.font_Id := (openGL.to_Asset ("assets/opengl/font/LiberationMono-Regular.ttf"), 30);
         titles_Font        : opengl.Font.font_Id := (openGL.to_Asset ("assets/opengl/font/LiberationMono-Regular.ttf"), 40);
         is_capturing_Video : Boolean             := False;
         Dolly              : gel.Dolly.view;

         last_pressed_Key   : gel.Keyboard.Key := gel.Keyboard.Nil;
         key_Focus          : gel.Sprite.view;

         quit_Requested     : Boolean := False;
      end record;

   global_Window : gel.Window.view;


end gel.Applet;
