
with mmi.World;
with mmi.Event;
with mmi.Window;
with mmi.surface_Profile;
with mmi.Surface;

--limited with lumen.Display;

private with sdl_c.SDL_Surface;


package mmi.Screen.sdl
--
--
--
is
--     pragma pure;

   type Item  is new mmi.Screen.item with private;
   type View  is access all Item'Class;

   type Items is array (Positive range <>) of aliased Item;
   type Views is array (Positive range <>) of View;




   -- define/destroy
   --

   procedure define  (Self : in out Item);
   procedure destroy (Self : in out Item);



   -- attributes
   --

--     type Display_view is access all lumen.Display.item'class;
--
--     function Display (Self : in Item) return Display_view;


   function  Surface (Self : in Item) return mmi.Surface.view;
--     function World   (Self : in Item) return lumen.World.view;

   function  Width  (Self : in Item) return Positive;
   function  Height (Self : in Item) return Positive;

   procedure Size_is (Self : in out Item;   Width, Height : in Positive);



   -- operations
   --




   -- factory
   --

--     function new_Window (Self : access Item;   enable_Events   : in mmi.event.Kinds;
--                                                surface_profile : in mmi.surface_Profile.item'Class) return access mmi.Window.item'class   is abstract;




private



   type Item  is new mmi.Screen.item with
      record
         Surface :  sdl_c.SDL_Surface.Pointer;

         Width   : Positive;
         Height  : Positive;

--           World   :  lumen.World.view;
      end record;






end mmi.Screen.sdl;
