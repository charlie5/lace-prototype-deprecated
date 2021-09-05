with
     openGL.surface_Profile,
     openGL.Surface,
     glx.Context;

package openGL.Context
--
--  Models an openGL (GLX) context.
--
is
   type Item is tagged private;
   type View is access all Item'Class;


   procedure define       (Self : in out Item;   Profile       : in surface_Profile.item'Class);

   procedure make_Current (Self : in     Item;   read_Surface  : in Surface.item;
                                                 write_Surface : in Surface.item);

   function  glx_Context_debug (Self : in Item'Class) return glx.Context.item;     -- For debugging.



private

   type Item is tagged
      record
         glx_Context : aliased glx.Context.item;
      end record;

end openGL.Context;
