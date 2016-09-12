--  with
--       glx.Pointers;

with
     OSMesa_C.Binding,
     System;

package body openGL.Context
is

   procedure define (Self : in out Item;   the_Display         : access openGL.Display.item'Class;
                                           the_surface_Profile : in     openGL.surface_Profile.item'Class)

   is
      pragma Unreferenced (the_surface_Profile);
--        use Glx,
--            glx.Pointers;

      use OSMesa_C.Binding;
      use type System.Address;

   begin
      Self.Context := OSMesaCreateContext (format    => GL.GL_RGBA,   -- OSMESA_RGBA,
                                           sharelist => system.Null_Address);
      if Self.Context = System.Null_Address
      then
         raise Program_Error with "no openGL context";
      end if;

      Self.Display := the_Display;
   end define;



   procedure make_Current (Self : in Item;   read_Surface  : in openGL.Surface.item;
                                             write_Surface : in openGL.Surface.item)
   is
      pragma Unreferenced (write_Surface);

--        Success : glx.Bool;     pragma Unreferenced (Success);

   begin
      null;
   end make_Current;



--     function glx_Context_debug (Self : in Item'Class) return GLX.GLXContext.item
--     is
--     begin
--        return self.glx_Context;
--     end glx_Context_debug;


end openGL.Context;
