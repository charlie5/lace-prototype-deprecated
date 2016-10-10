with
     GL,
     openGL.Tasks,
     interfaces.C;


package body openGL.Renderer
is

   use GL, interfaces.C;


   procedure Background_is (Self : in out Item;   Now : in openGL.Color)
   is
      pragma Unreferenced (Self);
   begin
      Self.Background := Now;
   end Background_is;



   procedure clear_Frame (Self : in Item)
   is
      check_is_OK : constant Boolean := openGL.Tasks.Check;
   begin
      glClearColor (GLfloat (to_Real (Self.Background.Red)),
                    GLfloat (to_Real (Self.Background.Green)),
                    GLfloat (to_Real (Self.Background.Blue)),
--                      1.0);
                    0.9);

      glClear (   GL_COLOR_BUFFER_BIT
               or GL_DEPTH_BUFFER_BIT);

      glCullFace (GL_BACK);
      glEnable   (GL_CULL_FACE);
   end clear_Frame;


end openGL.Renderer;
