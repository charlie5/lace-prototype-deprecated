with
     openGL.Program,
     openGL.Tasks,
     GL.lean,
     GL.Pointers,
     Interfaces.C.Strings;


package body openGL.Variable.uniform
is
   use GL.lean,
       Interfaces;


   ---------
   --  Forge
   --

   procedure define  (Self : in out Item;   Program : access openGL.Program.item'Class;
                                            Name    : in     String)
   is
      use GL.Pointers, C;

      check_is_OK : constant Boolean               := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      the_Name    : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.new_String (Name);

   begin
      Self.gl_Variable := glGetUniformLocation (Program.gl_Program,
                                                to_GLchar_access (the_Name));
      Interfaces.C.Strings.free (the_Name);

      if Self.gl_Variable = -1
      then
         raise openGL.Error with "unable to get location for uniform named '" & Name & "'";
      end if;
   end define;



   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      null;
   end destroy;




   -----------
   --  Actuals
   --

   --  bool
   --
   procedure Value_is (Self : in bool;   Now : in Boolean)
   is
      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
   begin
      glUniform1i (Self.gl_Variable,
                   Boolean'Pos (Now));
   end Value_is;


   --  int
   --
   procedure Value_is (Self : in int;   Now : in Integer)
   is
      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
   begin
      glUniform1i (Self.gl_Variable,
                   gl.GLint (Now));
   end Value_is;



   --  float
   --
   procedure Value_is (Self : in float;   Now : in openGL.Real)
   is
      check_is_OK : constant Boolean := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
   begin
      glUniform1fv (Self.gl_Variable,
                    1,
                    GLfloat_Address (Now'Address));
   end Value_is;



   --  vec3
   --
   procedure Value_is (Self : in vec3;   Now : in openGL.Vector_3)
   is
      check_is_OK : constant Boolean         := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      the_Vector  : aliased  openGL.Vector_3 := Now;
   begin
      glUniform3fv (Self.gl_Variable,
                    1,
                    the_Vector (1)'Address);
   end Value_is;



   --  vec4
   --
   procedure Value_is (Self : in vec4;   Now : in openGL.Vector_4)
   is
      check_is_OK : constant Boolean         := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      the_Vector  : aliased  openGL.Vector_4 := Now;
   begin
      glUniform4fv (Self.gl_Variable,
                    1,
                    the_Vector (1)'Address);
   end Value_is;



   --  mat3
   --
   procedure Value_is (Self : in mat3;   Now : in openGL.Matrix_3x3)
   is
      use GL;
      check_is_OK : constant Boolean           := openGL.Tasks.Check;     pragma Unreferenced (check_is_OK);
      the_Matrix  : aliased  openGL.Matrix_3x3 := Now;
   begin
      glUniformMatrix3fv (Self.gl_Variable,
                          1,
                          GL_FALSE,
                          the_Matrix (1, 1)'Address);
   end Value_is;



   --  mat4
   --
   procedure Value_is (Self : in mat4;   Now : in openGL.Matrix_4x4)
   is
      use GL;
      check_is_OK : constant Boolean           := openGL.Tasks.Check;   pragma Unreferenced (check_is_OK);
      the_Matrix  : aliased  openGL.Matrix_4x4 := Now;
   begin
      glUniformMatrix4fv (Self.gl_Variable,
                          1,
                          GL_FALSE,
                          the_Matrix (1, 1)'Address);
   end Value_is;


end openGL.Variable.uniform;
