with
     openGL.Tasks,
     GL.Binding,
     interfaces.C.Strings,
     ada.unchecked_Conversion;

package body openGL.Server
is

   function Version return String
   is
      use GL,
          GL.Binding,
          Interfaces;

      check_is_OK : constant Boolean := openGL.Tasks.Check with Unreferenced;

      type GLubyte_Pointer  is access all GLubyte;
      function to_Chars_ptr is new ada.unchecked_Conversion (GLubyte_Pointer,
                                                             c.Strings.Chars_ptr);

      Result : constant String := c.Strings.Value (to_Chars_ptr (glGetString (GL_VERSION)));
   begin
      return Result;
   end Version;



   function Version return a_Version
   is
      use GL,
          GL.Binding;
      Major : aliased glInt;
      Minor : aliased glInt;
   begin
      glGetIntegerv (GL_MAJOR_VERSION, Major'Access);
      glGetIntegerv (GL_MINOR_VERSION, Minor'Access);

      return (Major => Integer (Major),
              Minor => Integer (Minor));
   end Version;


end openGL.Server;
