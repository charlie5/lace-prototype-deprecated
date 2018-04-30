with
     mmi.remote.World,
     mmi.World,
     mmi.Camera,
     mmi.Keyboard,
     mmi.Mouse,
     mmi.Sprite,
     mmi.Dolly,
     mmi.Window,

     physics.Forge,

     openGL.Renderer.lean,
     opengl.Font, -- .texture,

     lace.Event,
     lace.       Response,
     lace.       Subject,
     lace.       Observer,
     lace.       Subject_and_deferred_Observer,
     lace.remote.Subject_and_deferred_Observer,

     ada.containers.Vectors;


package mmi.Applet
--
--  Provides an application model, configured with a single window.
--
is

   type Item is limited new lace.remote.Subject_and_deferred_Observer.item with private;
   type View is access all Item'Class;


   ----------
   --- Forge
   --

   package Forge
   is
      function  to_Applet (Name       : in String;
                           use_Window : in mmi.Window.view) return Item;

      function new_Applet (Name       : in String;
                           use_Window : in mmi.Window.view) return View;
   end Forge;


   overriding
   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);



   ---------------
   --- Attributes
   --

   function is_Open        (Self : in     Item) return        Boolean;

   function Window         (Self : in     Item) return        mmi.Window.view;
   function Renderer       (Self : in     Item) return        openGL.Renderer.lean.view;
   function Keyboard       (Self : in     Item) return access mmi.Keyboard.item'class;
   function Mouse          (Self : in     Item) return access mmi.Mouse.item'class;
   function Dolly          (Self : access Item) return        mmi.Dolly.view;

   function last_Keypress  (Self : access Item) return        mmi.Keyboard.Key;


   function world_Count    (Self : in     Item)                            return Natural;
   function Worlds         (Self : in     Item)                            return mmi.World.views;
   function World          (Self : in     Item;   Id        : in world_Id := 1) return mmi.World.view;
   function World_as_iFace (Self : in     Item;   Id        : in world_Id := 1) return mmi.remote.World.view;


   function Camera         (Self : in     Item;   world_Id  : in mmi.world_Id  := 1;
                                                  camera_Id : in mmi.camera_Id := 1) return mmi.Camera.view;


   function        Font    (Self : in     Item) return opengl.Font.font_Id;
   function titles_Font    (Self : in     Item) return opengl.Font.font_Id;



   --- Add a new world and camera(s).
   --

   use type mmi.Camera.view;
   package camera_Vectors is new ada.Containers.Vectors (Positive, mmi.Camera.view);
   subtype camera_Vector  is     camera_Vectors.Vector;


   type world_Info is
      record
         World   : mmi.World.view;
         Cameras : camera_Vector;
      end record;

   type world_Info_view is access all world_Info;


   procedure add (Self : in out Item;   the_World : in world_Info_view);

   procedure add_new_World (Self : in out Item;   Name       : in String;
                                                  space_Kind : in physics.space_Kind);

   function  new_World (Self : access Item;   Name       : in String;
                                              space_Kind : in physics.space_Kind) return mmi.World.view;



   ---------------
   --- Operations
   --

   procedure evolve_all_Worlds      (Self : in out Item;   By : in Duration);

   procedure add                    (Self : in out Item;   the_Sprite    : in mmi.Sprite.view);
   procedure add                    (Self : in out Item;   the_Sprite    : in mmi.Sprite.view;
                                                           at_site       : in math.Vector_3);


   procedure Dolly_is               (Self : access Item;   Now           : in mmi.Dolly.view);
   procedure enable_simple_Dolly    (Self : access Item;   in_World      : in world_Id);
   procedure enable_following_Dolly (Self : access Item;   Follow        : in mmi.Sprite.view);



   procedure enable_Mouse           (Self : access Item;   detect_Motion : in Boolean);


   procedure prepare                (Self : access Item) is null;
   procedure freshen                (Self : in out Item);
   --
   --  processes window events and then redraws the window.


   procedure take_Screenshot        (Self : in out Item;   Filename      : in String);
   procedure request_Quit           (Self : in out Item);



   --- Events
   --

   function local_Subject_and_Observer (Self : access Item) return lace.Subject_and_deferred_Observer.view;
   function local_Subject              (Self : access Item) return lace.Subject.view;
   function local_Observer             (Self : access Item) return lace.Observer.view;




private

   use type Sprite.view;
   package sprite_Vectors is new ada.containers.Vectors (Positive, Sprite.view);


   --------------------
   --- Event Responses
   --

   type applet_event_Response is abstract new lace.Response.item with
      record
         Applet : mmi.Applet.view;
      end record;


   --- add_new_Sprite response
   --

   type add_new_Sprite is new applet_event_Response with null record;

   overriding
   function  Name    (Self : in     add_new_Sprite) return String;
   overriding
   procedure respond (Self : in out add_new_Sprite;   to_Event : in lace.Event.Item'Class);

   the_add_new_sprite_Response : aliased add_new_Sprite;



   --  keyboard responses
   --
   type key_press_Response is new applet_event_Response with null record;
   overriding
   procedure respond (Self : in out key_press_Response;        to_Event : in lace.Event.Item'Class);


   type key_release_Response is new applet_event_Response with null record;
   overriding
   procedure respond (Self : in out key_release_Response;      to_Event : in lace.Event.Item'Class);



   --  mouse responses
   --
   type button_press_Response is new applet_event_Response with null record;
   overriding
   procedure respond (Self : in out button_press_Response;     to_Event : in lace.Event.Item'Class);

   type button_release_Response is new applet_event_Response with null record;
   overriding
   procedure respond (Self : in out button_release_Response;   to_Event : in lace.Event.Item'Class);


   type mouse_motion_Response is new applet_event_Response with null record;
   overriding
   procedure respond (Self : in out mouse_motion_Response;     to_Event : in lace.Event.Item'Class);


   type mouse_click_raycast_Response is new lace.Response.item with
      record
         Applet : mmi.Applet.view;
      end record;

   overriding
   procedure respond (Self : in out mouse_click_raycast_Response;   to_Event : in lace.Event.Item'Class);

   type mouse_click_raycast_Response_view is access all mouse_click_raycast_Response'Class;




   --  Screen Resize.
   --
   type resize_event_Response is new applet_event_Response with null record;
   overriding
   procedure respond (Self : in out resize_event_Response;   to_Event : in lace.Event.Item'Class);




   -- world_Vector
   --

   use type mmi.World.view;
   package world_Vectors is new ada.Containers.Vectors (Positive, world_Info_view);
   subtype world_Vector  is world_Vectors.Vector;




   -- Applet Item
   --
   type Item is limited new lace.remote.Subject_and_deferred_Observer.item with
      record
         local_Subject_and_Observer   :         lace.Subject_and_deferred_Observer.view := new lace.Subject_and_deferred_Observer.item;

         Window                       :         mmi.Window.view;

         resize_Response              : aliased applet.resize_event_Response;

         Worlds                       :         World_Vector;

         Keyboard                     : access  mmi.Keyboard.item'Class;
         key_press_Response           : aliased applet.key_press_Response;
         key_release_Response         : aliased applet.key_release_Response;

         Dolly                        : aliased mmi.Dolly.view;

         Mouse                        : access  mmi.Mouse.item'Class;
         button_press_Response        : aliased applet.button_press_Response;
         button_release_Response      : aliased applet.button_release_Response;
         mouse_motion_Response        : aliased applet.mouse_motion_Response;
         mouse_click_raycast_Response : aliased applet.mouse_click_raycast_Response;

         Renderer                     :         openGL.Renderer.lean.view;

         Font                         :         opengl.Font.font_Id := (openGL.to_Asset ("assets/opengl/font/LiberationMono-Regular.ttf"),  30);
         titles_Font                  :         opengl.Font.font_Id := (openGL.to_Asset ("assets/opengl/font/LiberationMono-Regular.ttf"),  40);

         is_capturing_Video           :         Boolean             := False;

         last_pressed_Key             :         mmi.Keyboard.Key    := mmi.Keyboard.Nil;
         key_Focus                    :         mmi.Sprite.view;

         quit_Requested               :         Boolean             := False;
      end record;

   global_Window : mmi.Window.view;


end mmi.Applet;
