with
     openGL.Texture;

private
with
     ada.unchecked_Conversion;

package openGL.Primitive
--
--  Provides a base class for openGL primitives.
--
is
   type    Item  is abstract tagged limited private;
   subtype Class is Item'Class;

   type    View  is access all Item'class;
   type    Views is array (Index_t range <>) of View;


   ----------
   --  Facets
   --
   type facet_Kind is (Points,
                       Lines,     line_Loop,      line_Strip,
                       Triangles, triangle_Strip, triangle_Fan);

   ---------
   --  Forge
   --

   procedure define  (Self : in out Item;   Kind    : in facet_Kind);
   procedure destroy (Self : in out Item) is abstract;
   procedure free    (Self : in out View);


   --------------
   --  Attributes
   --

   function  Texture        (Self : in     Item)     return openGL.Texture.Object;
   procedure Texture_is     (Self : in out Item;   Now : in openGL.Texture.Object);

   procedure Bounds_are     (Self : in out Item;   Now : in openGL.Bounds);
   function  Bounds         (self : in     Item)     return openGL.Bounds;
   --
   -- Returns the bounds in object space.

   procedure is_Transparent (Self : in out Item;   Now : in Boolean := True);
   function  is_Transparent (Self : in     Item)     return Boolean;


   ---------------
   --- Operations
   --

   procedure render (Self : in out Item);

   unused_line_Width : constant := -1.0;



private

   type Item is abstract tagged limited
      record
         facet_Kind     : primitive.facet_Kind;
         Texture        : openGL.Texture.Object := openGL.Texture.null_Object;
         is_Transparent : Boolean;
         Bounds         : openGL.Bounds;
         line_Width     : Real := unused_line_Width;
      end record;


   ----------
   --  Facets
   --

   function Thin (Self : in facet_Kind) return gl.GLenum;

   for facet_Kind use (Points         => gl.GL_POINTS,
                       Lines          => gl.GL_LINES,
                       line_Loop      => gl.GL_LINE_LOOP,
                       line_Strip     => gl.GL_LINE_STRIP,
                       Triangles      => gl.GL_TRIANGLES,
                       triangle_Strip => gl.GL_TRIANGLE_STRIP,
                       triangle_Fan   => gl.GL_TRIANGLE_FAN);

   for facet_Kind'Size use gl.GLenum'Size;

   function Convert is new ada.Unchecked_Conversion (facet_Kind, gl.GLenum);

   function Thin (Self : in facet_Kind) return gl.GLenum
                  renames Convert;


end openGL.Primitive;
