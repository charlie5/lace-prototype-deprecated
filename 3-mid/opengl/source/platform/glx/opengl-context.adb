with
     glx.Pointers;

package body openGL.Context     -- TODO: Finish this package.
is

   procedure define (Self : in out Item;   Profile : in openGL.surface_Profile.item'Class)

   is
      pragma Unreferenced (Profile);
      use GlX,
          glx.Pointers;
   begin
      if Self.glx_Context = null
      then
         raise Program_Error with "No openGL context";
      end if;
   end define;



   procedure make_Current (Self : in Item;   read_Surface  : in Surface.item;
                                             write_Surface : in Surface.item)
   is
      pragma Unreferenced (write_Surface);
      Success : glx.Bool with Unreferenced;
   begin
      null;
   end make_Current;



   function glx_Context_debug (Self : in Item'Class) return glx.Context.item
   is
   begin
      return Self.glx_Context;
   end glx_Context_debug;


end openGL.Context;
