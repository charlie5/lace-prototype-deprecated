package openGL.Program.lit.colored_skinned
--
--  Provides a program for lit, colored, textured and skinned vertices.
--
is
   type Item is new openGL.Program.lit.item with private;
   type View is access all Item'Class;


   overriding
   procedure define (Self : in out Item;   use_vertex_Shader   : in Shader.view;
                                           use_fragment_Shader : in Shader.view);

   overriding
   procedure set_Uniforms (Self : in Item);

   procedure bone_Transform_is (Self : in Item;   Which : in Integer;
                                                  Now   : in Matrix_4x4);


private

   type bone_transform_Uniforms is array (1 .. 120) of Variable.uniform.mat4;

   type Item is new openGL.Program.lit.item with
      record
         bone_transform_Uniforms : lit.colored_skinned.bone_transform_Uniforms;
      end record;

end openGL.Program.lit.colored_skinned;
