with
     GL,
     GL.Binding,
     openGL.Tasks,
     interfaces.C;

package body openGL.Renderer
is
   use GL,
       interfaces.C;


   procedure Background_is (Self : in out Item;   Now     : in openGL.Color;
                                                  Opacity : in Opaqueness := 1.0)
   is
   begin
      Self.Background.Primary := +Now;
      Self.Background.Alpha   := to_color_Value (Primary (Opacity));
   end Background_is;



   procedure Background_is (Self : in out Item;   Now : in openGL.lucid_Color)
   is
   begin
      Self.Background := +Now;
   end Background_is;



   procedure clear_Frame (Self : in Item)
   is
      use GL.Binding;
      check_is_OK : constant Boolean := openGL.Tasks.Check with Unreferenced;
   begin
      glClearColor (GLfloat (to_Primary (Self.Background.Primary.Red)),
                    GLfloat (to_Primary (Self.Background.Primary.Green)),
                    GLfloat (to_Primary (Self.Background.Primary.Blue)),
                    GLfloat (to_Primary (Self.Background.Alpha)));

      glClear (   GL_COLOR_BUFFER_BIT
               or GL_DEPTH_BUFFER_BIT);

      glCullFace (GL_BACK);
      glEnable   (GL_CULL_FACE);
   end clear_Frame;


end openGL.Renderer;
