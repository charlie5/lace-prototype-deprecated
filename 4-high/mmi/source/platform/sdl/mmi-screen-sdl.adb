
with mmi.World.forge;

with SDL_c.binding;
with sdl_c.SDL_Surface;

with interfaces.C;



package body mmi.Screen.sdl
--
--
--
is

   use SDL_c,      SDL_c.binding;
   use Interfaces, Interfaces.C;





   -- general
   --

   procedure set_video_Mode (Self : in out Item)
   is
      use type sdl_c.SDL_Surface.Pointer;
   begin
      self.Surface := SDL_SetVideoMode (c.int (self.Width),
                                        c.int (self.Height),
                                        32,
                                        SDL_OPENGL or SDL_RESIZABLE); -- or SDL_FULLSCREEN);

      if self.Surface = null then   raise mmi.Error with "unable to SetVideoMode";   end if;
   end;




   -- define/destroy
   --


   procedure define  (Self : in out Item)
   is
      use sdl_c.SDL_Surface.Pointers;
      status : C.int;
   begin
      status := SDL_GL_SetAttribute (SDL_GL_RED_SIZE,     8);
      status := SDL_GL_SetAttribute (SDL_GL_GREEN_SIZE,   8);
      status := SDL_GL_SetAttribute (SDL_GL_BLUE_SIZE,    8);
      status := SDL_GL_SetAttribute (SDL_GL_DEPTH_SIZE,  16);
      status := SDL_GL_SetAttribute (SDL_GL_DOUBLEBUFFER, 1);

      self.Size_is (800, 800);

      define (mmi.Screen.item (Self));
   end;




   procedure destroy (Self : in out Item)
   is
   begin
      SDL_quit;
   end;




   -- attributes
   --

--     function World  (Self : in Item) return lumen.World.view
--     is
--     begin
--        return self.World;
--     end;




   function Surface (Self : in Item) return mmi.Surface.view
   is
   begin
      return null; --self.Surface;
   end;




--     function Display (Self : in Item) return Display_view;


   function Width  (Self : in Item) return Positive
   is
   begin
      return self.Width;
   end;



   function Height (Self : in Item) return Positive
   is
   begin
      return self.Height;
   end;



   procedure Size_is (Self : in out Item;   Width, Height : in Positive)
   is
   begin
      self.Width  := Width;
      self.Height := Height;

      set_video_Mode (Self);


      -- generate an 'resized' event
      --
      self.notify_Observers (resize_Event'(others => <>));

   end;



   -- operations
   --


end mmi.Screen.sdl;
