with
     openGL.Program,
     openGL.Tasks,
     openGL.Errors,
     GL.lean,
     GL.Pointers,
     interfaces.C.Strings;

package body openGL.Variable.uniform
is
   use GL.lean,
       Interfaces;

   ---------
   --  Forge
   --

   procedure define (Self : in out Item;   Program : access openGL.Program.item'Class;
                                           Name    : in     String)
   is
      use GL.Pointers, C;
      the_Name : C.Strings.chars_ptr := C.Strings.new_String (Name);

   begin
      Tasks.check;

      Self.gl_Variable := glGetUniformLocation (Program.gl_Program,
                                                to_GLchar_access (the_Name));
      Errors.log;
      C.Strings.free (the_Name);

      if Self.gl_Variable = -1
      then
         raise openGL.Error with "Unable to get location for uniform named '" & Name & "'";
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
   begin
      Tasks.check;
      glUniform1i (Self.gl_Variable,
                   Boolean'Pos (Now));
      Errors.log;
   end Value_is;


   --  int
   --
   procedure Value_is (Self : in int;   Now : in Integer)
   is
   begin
      Tasks.check;
      glUniform1i (Self.gl_Variable,
                   gl.GLint (Now));
      Errors.log;
   end Value_is;


   --  float
   --
   procedure Value_is (Self : in float;   Now : in Real)
   is
   begin
      Tasks.check;
      glUniform1fv (Self.gl_Variable,
                    1,
                    Now'Address);
      Errors.log;
   end Value_is;



   --  vec3
   --
   procedure Value_is (Self : in vec3;   Now : in Vector_3)
   is
   begin
      Tasks.check;
      glUniform3fv (Self.gl_Variable,
                    1,
                    Now (1)'Address);
      Errors.log;
   end Value_is;



   --  vec4
   --
   procedure Value_is (Self : in vec4;   Now : in Vector_4)
   is
   begin
      Tasks.check;
      glUniform4fv (Self.gl_Variable,
                    1,
                    Now (1)'Address);
      Errors.log;
   end Value_is;



   --  mat3
   --
   procedure Value_is (Self : in mat3;   Now : in Matrix_3x3)
   is
   begin
      Tasks.check;
      glUniformMatrix3fv (Self.gl_Variable,
                          1,
                          gl.GL_FALSE,
                          Now (1, 1)'Address);
      Errors.log;
   end Value_is;



   --  mat4
   --
   procedure Value_is (Self : in mat4;   Now : in Matrix_4x4)
   is
   begin
      Tasks.check;
      glUniformMatrix4fv (Self.gl_Variable,
                          1,
                          gl.GL_FALSE,
                          Now (1, 1)'Address);
      Errors.log;
   end Value_is;


end openGL.Variable.uniform;
