with
     openGL.Tasks,
     openGL.Conversions,

     GL.Pointers,
     GL.lean,

     ada.Characters.latin_1,
     ada.Strings.fixed,

     interfaces.C.Strings;


package body openGL.Program
is
   use gl.lean,
       Interfaces;

   compiling_in_debug_Mode : constant Boolean := True;

   type Shader_view is access all Shader.item'Class;


   --------------
   --  Parameters
   --

   procedure Program_is (Self : in out Parameters;   Now : in openGL.Program.view)
   is
   begin
      Self.Program := Now;
   end Program_is;


   function Program (Self : in Parameters) return openGL.Program.view
   is
   begin
      return Self.Program;
   end Program;


   ---------
   --- Forge
   --

   procedure define (Self : in out Item;   use_vertex_Shader   : in Shader.view;
                                           use_fragment_Shader : in Shader.view)
   is
   begin
      Tasks.check;

      Self.gl_Program := glCreateProgram;

      glAttachShader (Self.gl_Program,    use_vertex_Shader.gl_Shader);
      glAttachShader (Self.gl_Program,  use_fragment_Shader.gl_Shader);

      Self.  vertex_Shader :=   use_vertex_Shader;
      Self.fragment_Shader := use_fragment_Shader;

      glLinkProgram (Self.gl_Program);

      declare
         use type C.int;
         Status : aliased gl.glInt;
      begin
         glGetProgramiv (Self.gl_Program,
                         GL_LINK_STATUS,
                         Status'unchecked_Access);

         if Status = 0
         then
            declare
               link_Log : constant String := Self.ProgramInfoLog;
            begin
               Self.destroy;
               raise Error with "Program link error ~ " & link_Log;
            end;
         end if;
      end;

      if compiling_in_debug_Mode
      then
         glValidateProgram (Self.gl_Program);
      end if;
   end define;



   procedure define (Self : in out Item;   use_vertex_Shader_File   : in String;
                                           use_fragment_Shader_File : in String)
   is
      use openGL.Shader;
      the_vertex_Shader   : constant Shader_view := new openGL.Shader.item;
      the_fragment_Shader : constant Shader_view := new openGL.Shader.item;
   begin
      the_vertex_Shader  .define (openGL.Shader.vertex,     use_vertex_Shader_File);
      the_fragment_Shader.define (openGL.Shader.fragment, use_fragment_Shader_File);

      Self.define (  the_vertex_Shader.all'Access,
                   the_fragment_Shader.all'Access);
   end define;



   procedure destroy (Self : in out Item)
   is
   begin
      Tasks.check;
      glDeleteProgram (Self.gl_Program);
   end destroy;


   --------------
   --  Attributes
   --

   function Attribute (Self : access Item'Class;   Named : in String) return openGL.Attribute.view
   is
   begin
      for Each in 1 .. Self.attribute_Count
      loop
         if Self.Attributes (Each).Name = Named
         then
            return Self.Attributes (Each);
         end if;
      end loop;

      raise Error with "'" & Named & "' is not a valid program attribute.";
   end Attribute;



   function attribute_Location (Self : access Item'Class;   Named : in String) return gl.GLuint
   is
      use      gl.Pointers;
      use type gl.GLint;

      attribute_Name : C.strings.chars_ptr := C.Strings.new_String (Named & ada.characters.Latin_1.NUL);

   begin
      Tasks.check;

      declare
         gl_Location : constant gl.GLint := glGetAttribLocation (Self.gl_Program,
                                                                 to_GLchar_access (attribute_Name));
      begin
         if gl_Location = -1
         then
            raise Error with "Requested attribute '" & Named & "' has no gl location in program.";
         end if;

         C.Strings.free (attribute_Name);

         return gl.GLuint (gl_Location);
      end;
   end attribute_Location;



   function is_defined (Self : in Item'Class) return Boolean
   is
      use type a_gl_Program;
   begin
      return Self.gl_Program /= 0;
   end is_defined;



   function ProgramInfoLog (Self : in Item) return String
   is
      use C, GL;

      info_log_Length : aliased glInt   := 0;
      chars_Written   : aliased glSizei := 0;

   begin
      Tasks.check;

      glGetProgramiv (Self.gl_Program,
                      GL_INFO_LOG_LENGTH,
                      info_log_Length'unchecked_Access);

      if info_log_Length = 0 then
         return "";
      end if;

      declare
         use GL.Pointers;
         info_Log     : aliased  C.char_array        := C.char_array' (1 .. C.size_t (info_log_Length) => <>);
         info_Log_ptr : constant C.strings.chars_ptr := C.strings.to_chars_ptr (info_Log'unchecked_Access);
      begin
         glGetProgramInfoLog (Self.gl_Program,
                              glSizei (info_log_Length),
                              chars_Written'unchecked_Access,
                              to_GLchar_access (info_Log_ptr));
         return C.to_Ada (info_Log);
      end;
   end ProgramInfoLog;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return Variable.uniform.bool
   is
      the_Variable : Variable.uniform.bool;
   begin
      the_Variable.define (Self, Named);
      return the_Variable;
   end uniform_Variable;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return Variable.uniform.int
   is
      the_Variable : Variable.uniform.int;
   begin
      the_Variable.define (Self, Named);
      return the_Variable;
   end uniform_Variable;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return Variable.uniform.float
   is
      the_Variable : Variable.uniform.float;
   begin
      the_Variable.define (Self, Named);
      return the_Variable;
   end uniform_Variable;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return Variable.uniform.vec3
   is
      the_Variable : Variable.uniform.vec3;
   begin
      the_Variable.define (Self, Named);
      return the_Variable;
   end uniform_Variable;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return Variable.uniform.vec4
   is
      the_Variable : Variable.uniform.vec4;
   begin
      the_Variable.define (Self, Named);
      return the_Variable;
   end uniform_Variable;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return Variable.uniform.mat3
   is
      the_Variable : Variable.uniform.mat3;
   begin
      the_Variable.define (Self, Named);
      return the_Variable;
   end uniform_Variable;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return Variable.uniform.mat4
   is
      the_Variable : Variable.uniform.mat4;
   begin
      the_Variable.define (Self, Named);
      return the_Variable;
   end uniform_Variable;


   --------------
   --  Operations
   --

   procedure add (Self : in out Item;   Attribute : in openGL.Attribute.view)
   is
   begin
      Self.attribute_Count                   := Self.attribute_Count + 1;
      Self.Attributes (Self.attribute_Count) := Attribute;
   end add;



   procedure enable (Self : in out Item)
   is
      use type gl.GLuint;
   begin
      Tasks.check;

      if Self.gl_Program = 0
      then
         Item'Class (Self).define;     -- TODO: This appears to do nothing.
      end if;

      glUseProgram (self.gl_Program);
   end enable;



   procedure enable_Attributes (Self : in Item)
   is
   begin
      for Each in 1 .. Self.attribute_Count
      loop
         Self.Attributes (Each).enable;
      end loop;
   end enable_Attributes;



   procedure mvp_Transform_is (Self : in out Item'Class;   Now : in Matrix_4x4)
   is
   begin
      Self.mvp_Transform := Now;
   end mvp_Transform_is;



   procedure camera_Site_is (Self : in out Item'Class;   Now : in Vector_3)
   is
   begin
      Self.camera_Site := Now;
   end camera_Site_is;



   procedure model_Matrix_is (Self : in out Item'Class;   Now : in Matrix_4x4)
   is
   begin
      Self.model_Transform := Now;
   end model_Matrix_is;



   --  procedure camera_Matrix_is (Self : in out Item'Class;   Now : in Matrix_4x4)
   --  is
   --  begin
   --     Self.camera_Matrix := Now;
   --  end camera_Matrix_is;



   procedure inverse_modelview_Matrix_is (Self : in out Item'Class;   Now : in Matrix_3x3)
   is
   begin
      Self.inverse_modelview_Matrix := Now;
   end inverse_modelview_Matrix_is;



   procedure Light_is (Self : in out Item'Class;   light_Id : in Positive;
                                                   Now      : in Light.item)
   is
   begin
      Self.Lights (light_Id) := Now;
   end Light_is;



--     procedure directional_Light_is (Self : in out Item'Class;   light_Id : in Positive;
--                                                                 Now      : in Light.directional.item)
--     is
--     begin
--        Self.directional_Light (light_Id) := Now;
--     end directional_Light_is;
--
--
--
--     procedure diffuse_Light_is (Self : in out Item'Class;   light_Id : in Positive;
--                                                             Now      : in Light.diffuse.item)
--     is
--     begin
--        Self.diffuse_Lights (light_Id) := Now;
--     end diffuse_Light_is;



   procedure Scale_is (Self : in out Item'Class;   Now : in Vector_3)
   is
   begin
      Self.Scale := Now;
   end Scale_is;



   --  procedure Shine_is (Self : in out Item'Class;   Now : in Shine)
   --  is
   --  begin
   --     Self.Shine := Now;
   --  end Shine_is;



   procedure set_mvp_Uniform (Self : in Item)
   is
      the_mvp_Uniform : constant Variable.uniform.mat4 := Self.uniform_Variable ("mvp_Transform");
   begin
      the_mvp_Uniform.Value_is (Self.mvp_Transform);
   end set_mvp_Uniform;



   --  procedure set_Uniforms (Self : in Item)
   --  is
   --     the_model_Uniform  : constant Variable.uniform.mat4 := Self.uniform_Variable ("model");
   --     the_camera_Uniform : constant Variable.uniform.mat4 := Self.uniform_Variable ("camera");
   --  begin
   --     the_model_Uniform .Value_is (Self.model_Matrix);
   --     the_camera_Uniform.Value_is (Self.camera_Matrix);
   --  end set_Uniforms;


   procedure set_Uniforms (Self : in Item)
   is
      use linear_Algebra_3d,
          openGL.Conversions;

      the_scale_Uniform                  : constant Variable.uniform.vec3 := Self.uniform_Variable ("Scale");
      the_light_count_Uniform            : constant Variable.uniform.int  := Self.uniform_Variable ("light_Count");
      the_specular_color_Uniform         : constant Variable.uniform.vec3 := Self.uniform_Variable ("specular_Color");
      the_camera_site_Uniform            : constant Variable.uniform.vec3 := Self.uniform_Variable ("camera_Site");
      the_model_transform_Uniform        : constant Variable.uniform.mat4 := Self.uniform_Variable ("model_Transform");
      the_inverse_model_rotation_Uniform : constant Variable.uniform.mat3 := Self.uniform_Variable ("inverse_model_Rotation");
   begin
      --  openGL.Program.item (Self).set_Uniforms;
      Self.set_mvp_Uniform;


      the_light_count_Uniform           .Value_is (1);
      the_specular_color_Uniform        .Value_is (to_Vector_3 (Self.specular_Color));
      the_scale_Uniform                 .Value_is (Self.Scale);
      the_camera_site_Uniform           .Value_is (Self.camera_Site);
      the_model_transform_Uniform       .Value_is (Self.model_Transform);
      the_inverse_model_rotation_Uniform.Value_is (Inverse (get_Rotation (Self.model_Transform)));

      -- Lights.
      --
      for i in Self.Lights'Range
      loop
         declare
            use Light;

            Light : openGL.Light.item renames Self.Lights (i);

            function light_Name return String
            is
               use ada.Strings,
                   ada.Strings.fixed;
            begin
               return "Lights[" & Trim (Integer'Image (i - 1), Left) & "]";
            end light_Name;

            site_Uniform                : constant Variable.uniform.vec4  := Self.uniform_Variable (light_Name & ".Site");
            color_Uniform               : constant Variable.uniform.vec3  := Self.uniform_Variable (light_Name & ".Color");
            attenuation_Uniform         : constant Variable.uniform.float := Self.uniform_Variable (light_Name & ".Attenuation");
            ambient_coefficient_Uniform : constant Variable.uniform.float := Self.uniform_Variable (light_Name & ".ambient_Coefficient");
            cone_angle_Uniform          : constant Variable.uniform.float := Self.uniform_Variable (light_Name & ".cone_Angle");
            cone_direction_Uniform      : constant Variable.uniform.vec3  := Self.uniform_Variable (light_Name & ".cone_Direction");
         begin
            case Light.Kind
            is
            when Diffused =>   site_Uniform.Value_is (Vector_4 (Light.Site & 1.0));
            when Direct   =>   site_Uniform.Value_is (Vector_4 (Light.Site & 0.0));
            end case;

            color_Uniform              .Value_is (to_Vector_3 (Light.Color));
            attenuation_Uniform        .Value_is (             Light.Attenuation);
            ambient_coefficient_Uniform.Value_is (             Light.ambient_Coefficient);
            cone_angle_Uniform         .Value_is (Real        (Light.cone_Angle));
            cone_direction_Uniform     .Value_is (             Light.cone_Direction);
         end;
      end loop;
   end set_Uniforms;



   procedure specular_Color_is (Self : in out Item;   Now : in Color)
   is
   begin
      Self.specular_Color := Now;
   end specular_Color_is;



   --  Privvy
   --

   function gl_Program (Self : in Item) return a_gl_Program
   is
   begin
      return Self.gl_Program;
   end gl_Program;


end openGL.Program;
