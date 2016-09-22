with
     openGL.surface_Profile,
     openGL.Surface,

     Glx.GLXContext;


package openGL.Context
--
--  Models an openGL (GLX) context.
--
is

   type Item is tagged private;
   type View is access all Item'Class;


   procedure define       (Self : in out Item;   the_surface_Profile : in     openGL.surface_Profile.item'Class);


   procedure make_Current (Self : in Item;   read_Surface  : in openGL.Surface.item;
                                             write_Surface : in openGL.Surface.item);


   function glx_Context_debug (Self : in Item'Class) return GLX.GLXContext.item;     -- For debug.



private

   type Item is tagged
      record
         glx_Context : aliased GLX.GLXContext.item;
      end record;

end openGL.Context;
