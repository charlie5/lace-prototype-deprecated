with
     openGL.Tasks,
     openGL.Errors,
     GL.lean,
     GL.Pointers,

     ada.Characters.latin_1,
     ada.Strings.unbounded,
     ada.Text_IO,
     ada.IO_Exceptions,

     interfaces.C.Strings;

package body openGL.Shader
is
   use GL.lean,
       Interfaces;

   -----------
   --  Utility
   --
   function read_text_File (Filename : in String) return C.char_array;


   ---------
   --  Forge
   --

   procedure define (Self : in out Item;   Kind            : in shader.Kind;
                                           source_Filename : in String)
   is
      use GL.Pointers,
          C.Strings;

      the_Source       : aliased  C.char_array    := read_text_File (source_Filename);
      the_Source_ptr   : aliased
                         constant chars_ptr       := to_chars_ptr (the_Source'unchecked_Access);
      the_Source_Array : aliased  chars_ptr_array := (1 => the_Source_ptr);
   begin
      Tasks.check;

      Self.Kind := Kind;

      if Kind = Vertex
      then   Self.gl_Shader := glCreateShader (GL_VERTEX_SHADER);
      else   Self.gl_Shader := glCreateShader (GL_FRAGMENT_SHADER);
      end if;

      Errors.log;

      glShaderSource (Self.gl_Shader,
                      1,
                      to_GLchar_Pointer_access (the_Source_array'Access),
                      null);
      Errors.log;

      glCompileShader (Self.gl_Shader);
      Errors.log;

      declare
         use type C.int;
         Status : aliased gl.glInt;
      begin
         glGetShaderiv (self.gl_Shader,
                        GL_COMPILE_STATUS,
                        Status'unchecked_Access);
         if Status = 0
         then
            declare
               compile_Log : constant String := Self.shader_info_Log;
            begin
               Self.destroy;
               raise Error with "'" & source_Filename & "' compilation failed ~ " & compile_Log;
            end;
         end if;
      end;
   end define;



   procedure destroy (Self : in out Item)
   is
   begin
      Tasks.check;
      glDeleteShader (self.gl_Shader);
   end destroy;


   --------------
   --  Attributes
   --

   function shader_info_Log (Self : in Item) return String
   is
      use C, GL;

      info_log_Length : aliased  glInt   := 0;
      chars_Written   : aliased  glSizei := 0;
   begin
      Tasks.check;

      glGetShaderiv (Self.gl_Shader,
                     GL_INFO_LOG_LENGTH,
                     info_log_Length'unchecked_Access);

      if info_log_Length = 0
      then
         return "";
      end if;

      declare
         use gl.Pointers;
         info_Log     : aliased  C.char_array        := C.char_array' (1 .. C.size_t (info_log_Length) => <>);
         info_Log_ptr : constant C.Strings.chars_Ptr := C.Strings.to_chars_ptr (info_Log'unchecked_Access);
      begin
         glGetShaderInfoLog (self.gl_Shader,
                             glSizei (info_log_Length),
                             chars_Written'unchecked_Access,
                             to_GLchar_access (info_Log_ptr));

         return C.to_Ada (info_Log);
      end;
   end shader_info_Log;


   ----------
   --  Privvy
   --

   function gl_Shader (Self : in Item) return a_gl_Shader
   is
   begin
      return Self.gl_Shader;
   end gl_Shader;


   -----------
   --  Utility
   --
   NL : constant String := "" & ada.characters.latin_1.LF;


   function read_text_File (Filename : in String) return C.char_array
   is
      use ada.Text_IO,
          ada.Strings.unbounded;

      the_File  : ada.Text_IO.File_type;
      Pad       : unbounded_String;

   begin
      open (the_File, in_File, Filename);

      while not end_of_File (the_File)
      loop
         append (Pad, get_Line (the_File) & NL);
      end loop;

      close (the_File);

      declare
         use type Interfaces.C.size_t;

         the_Data : C.char_array (1 .. C.size_t (Length (Pad)) + 1);
      begin
         for i in 1 .. the_Data'Last - 1
         loop
            the_Data (i) := C.char (Element (Pad, Integer (i)));
         end loop;

         the_Data (the_Data'Last) := C.char'Val (0);

         return the_Data;
      end;

   exception
      when ada.IO_Exceptions.name_Error =>
         raise Error with "Unable to locate shader asset named '" & Filename & "'.";
   end read_text_File;


end openGL.Shader;
