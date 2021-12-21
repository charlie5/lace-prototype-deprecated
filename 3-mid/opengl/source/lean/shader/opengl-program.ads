with
     openGL.Shader,
     openGL.Variable.uniform,
     openGL.Attribute,
     openGL.Light;

private
with
     GL;


package openGL.Program
--
--  Models an openGL program.
--
is
   type Item is tagged limited private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   procedure define  (Self : in out Item) is null;

   procedure define  (Self : in out Item;   use_vertex_Shader   : in Shader.view;
                                            use_fragment_Shader : in Shader.view);

   procedure define  (Self : in out Item;   use_vertex_Shader_File   : in String;
                                            use_fragment_Shader_File : in String);

   procedure destroy (Self : in out Item);


   ----------------------
   --  Program Parameters
   --

   --  These are used by individual visuals which require program Uniforms to vary from visual to visual.
   --  The Parmaters type is extended to contain the required varying data and 'enable' is overridden to
   --  apply the varying data to the programs Uniforms. 'enable' is called as part of the rendering process
   --  just prior to the visuals geometry being rendered.
   --
   --  (See 'gel.Human' for an example of usage.)

   type Parameters      is limited new openGL.Parameters with private;
   type Parameters_view is access all Parameters'Class;

   procedure Program_is (Self : in out Parameters;   Now : in Program.view);
   function  Program    (Self : in     Parameters)     return Program.view;
   procedure enable     (Self : in out Parameters) is null;


   --------------
   --  Attributes
   --

   function is_defined         (Self : in     Item'Class) return Boolean;

   function uniform_Variable   (Self : access Item'Class;   Named : in String) return Variable.uniform.bool;
   function uniform_Variable   (Self : access Item'Class;   Named : in String) return Variable.uniform.int;
   function uniform_Variable   (Self : access Item'Class;   Named : in String) return Variable.uniform.float;
   function uniform_Variable   (Self : access Item'Class;   Named : in String) return Variable.uniform.vec3;
   function uniform_Variable   (Self : access Item'Class;   Named : in String) return Variable.uniform.vec4;
   function uniform_Variable   (Self : access Item'Class;   Named : in String) return Variable.uniform.mat3;
   function uniform_Variable   (Self : access Item'Class;   Named : in String) return Variable.uniform.mat4;

   function Attribute          (Self : access Item'Class;   Named : in String) return Attribute.view;
   function attribute_Location (Self : access Item'Class;   Named : in String) return gl.GLuint;

   function ProgramInfoLog     (Self : in     Item) return String;      -- TODO: Better name.


   --------------
   --  Operations
   --

   procedure add               (Self : in out Item;   Attribute : in openGL.Attribute.view);
   procedure enable            (Self : in out Item);
   procedure enable_Attributes (Self : in     Item);


   ------------
   --  Uniforms
   --

   procedure mvp_Transform_is            (Self : in out Item;   Now : in Matrix_4x4);
   procedure camera_Site_is              (Self : in out Item;   Now : in Vector_3)    is null;
   procedure model_Matrix_is             (Self : in out Item;   Now : in Matrix_4x4)  is null;
   procedure Lights_are                  (Self : in out Item;   Now : in Light.items) is null;
   procedure Scale_is                    (Self : in out Item;   Now : in Vector_3);
   procedure set_Uniforms                (Self : in     Item);


   ----------
   --  Privvy   TODO: move this to privvy child package.
   --

   subtype a_gl_Program is gl.GLuint;
   function  gl_Program (Self : in Item) return a_gl_Program;



private

   type Item is tagged limited
      record
         gl_Program      : gl.GLuint := 0;
         vertex_Shader   : Shader.view;
         fragment_Shader : Shader.view;

         Attributes      : openGL.Attribute.views (1 .. 8);
         attribute_Count : Natural := 0;

         mvp_Transform   : Matrix_4x4;
         Scale           : Vector_3 := (1.0, 1.0, 1.0);
      end record;


   -------------
   -- Parameters
   --

   type Parameters is limited new openGL.Parameters with
      record
         Program : openGL.Program.view;
      end record;


end openGL.Program;
