with
     openGL.surface_Profile;

private
with
     Glx;

limited private
with
     openGL.Context;

package openGL.Surface
--
--  Models an openGL surface.
--
is
   type Item  is tagged private;
   type Items is array (Positive range <>) of aliased Item;

   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;


   procedure define (Self : in out Item;   Profile   : in surface_Profile.item'Class;
                                           Window_Id : in Natural);

   --  Operations
   --
   procedure swap_Buffers (Self : in Item);



private

   type Item is tagged
      record
         glx_Surface :        glx.Drawable;
         Context     : access openGL.Context.item'Class;
      end record;

end openGL.Surface;
