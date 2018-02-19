with
     openGL.Shader,
     openGL.Variable.uniform,
     openGL.Attribute,
     openGL.Light.directional;

private
with
     GL;


package openGL.Program
--
--  Models an openGL program.
--
is

   type Item  is abstract tagged limited private;
   type View  is access all Item'Class;


   ---------
   --- Forge
   --

   procedure define  (Self : in out Item) is null;

   procedure define  (Self : in out Item;   use_vertex_Shader   : in openGL.Shader.view;
                                            use_fragment_Shader : in openGL.Shader.view);

   procedure define  (Self : in out Item;   use_vertex_Shader_Filename   : in String;
                                            use_fragment_Shader_Filename : in String);

   procedure destroy (Self : in out Item);



   ----------------------
   --  Program Parameters
   --

   --  These are used by individual visuals which require program Uniforms to vary from visual to visual.
   --  The Parmaters type is extended to contain the required varying data and 'enable' is overridden to
   --  apply the varying data to the programs Uniforms. 'enable' is called as part of the rendering process
   --  just prior to the visuals geometry being rendered.
   --
   --  (See 'mmi.Human' for an example of usage.)

   type Parameters      is limited new openGL.Parameters with private;
   type Parameters_view is access all program.Parameters'Class;

   procedure Program_is (Self : in out Parameters;   Now : in openGL.Program.view);
   function  Program    (Self : in     Parameters)     return openGL.Program.view;
   procedure enable     (Self : in out Parameters) is null;



   --------------
   --  Attributes
   --

   function is_defined         (Self : in     Item'Class) return Boolean;

   function uniform_Variable   (Self : access Item'Class;   Named : in String) return openGL.Variable.uniform.int;
   function uniform_Variable   (Self : access Item'Class;   Named : in String) return openGL.Variable.uniform.float;
   function uniform_Variable   (Self : access Item'Class;   Named : in String) return openGL.Variable.uniform.vec3;
   function uniform_Variable   (Self : access Item'Class;   Named : in String) return openGL.Variable.uniform.vec4;
   function uniform_Variable   (Self : access Item'Class;   Named : in String) return openGL.Variable.uniform.mat3;
   function uniform_Variable   (Self : access Item'Class;   Named : in String) return openGL.Variable.uniform.mat4;

   function Attribute          (Self : access Item'Class;   Named : in String) return openGL.Attribute.view;
   function attribute_Location (Self : access Item'Class;   Named : in String) return gl.GLuint;

   function ProgramInfoLog     (Self : in     Item) return String;      -- tbd: rename



   --------------
   --  Operations
   --

   procedure add               (Self : in out Item;   the_Attribute : in openGL.Attribute.view);

   procedure enable            (Self : in out Item);
   procedure enable_Attributes (Self : in     Item);



   ------------
   --  Uniforms
   --

   procedure mvp_Matrix_is               (Self : in out Item'Class;   Now : in Matrix_4x4);
   procedure inverse_modelview_Matrix_is (Self : in out Item'Class;   Now : in Matrix_3x3);

   procedure directional_Light_is        (Self : in out Item'Class;   light_Id : in Positive;
                                                                      Now      : in openGL.Light.directional.item);
   procedure Scale_is                    (Self : in out Item'Class;   Now      : in Vector_3);
   procedure Shine_is                    (Self : in out Item'Class;   Now      : in openGL.Shine);

   procedure set_Uniforms                (Self : in     Item)   is abstract;



   ----------
   --  Privvy   tbd: move this to privvy child package.
   --

   subtype a_gl_Program is gl.GLuint;
   function  gl_Program (Self : in Item) return a_gl_Program;





private

   type Item is abstract tagged limited
      record
         gl_Program               : gl.GLuint := 0;
         vertex_Shader            : openGL.Shader.view;
         fragment_Shader          : openGL.Shader.view;

         Attributes               : openGL.Attribute.views (1 .. 8);
         attribute_Count          : Natural   := 0;

         mvp_Matrix               : Matrix_4x4;
         inverse_modelview_Matrix : Matrix_3x3;

         directional_Light        : openGL.Light.directional.items (1 .. 2);

         Scale                    : Vector_3     := (1.0, 1.0, 1.0);
         Shine                    : openGL.Shine := 1.0;
      end record;


   procedure set_mvp_Uniform (Self : in Item);



   -------------
   -- Parameters
   --

   type Parameters is limited new openGL.Parameters with
      record
         Program : openGL.Program.view;
      end record;


end openGL.Program;
