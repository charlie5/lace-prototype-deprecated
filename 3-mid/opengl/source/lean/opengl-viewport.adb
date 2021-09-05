with
     GL.Binding,
     openGL.Tasks;


package body openGL.Viewport
is
   use GL;


   function Extent return Extent_2d
   is
      use GL.Binding;

      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      Extent      : array (1 .. 4) of aliased gl.glInt;
   begin
      glGetIntegerv (gl_VIEWPORT,
                     Extent (1)'unchecked_Access);

      return (Integer (Extent (3)),
              Integer (Extent (4)));
   end Extent;



   procedure Extent_is (Now : in Extent_2d)
   is
      use GL.Binding;

      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
   begin
      glViewport (0, 0,
                  GLint (now.Width),
                  GLint (now.Height));
   end Extent_is;


end openGL.Viewport;
