with openGL.Model,
     mmi.Sprite,
     mmi.Joint,
     mmi.World,

     openGL,
     opengl.Variable.uniform,
     openGL.Program,

     float_Math;

private
with collada.Library.visual_scenes;




package my_Box
--
-- Provides access to and control of 'my_Box', a simply rigged collada model with a simple animation.
--
is
   package Math renames float_Math;


   type Item is tagged limited private;
   type View is access all Item'Class;


   type bone_Id is (Base, Bone_1, Bone_2);


   type collada_joint_Id is (Bone_1, Bone_2);


   type joint_inverse_bind_Matrices is array (collada_joint_Id) of math.Matrix_4x4;
   type joint_to_bone_site_Offets   is array (collada_joint_Id) of math.Vector_3;           -- The 'bind time' offset from a joint to its bone.
   type bone_Sprites                is array (bone_Id)          of mmi.Sprite.view;



   --- Forge
   --
   procedure define (Self : in out Item;   in_World     : in mmi.World.view;
                                           Model        : access openGL.Model.item'class;
                                           Mass         : in     math.Real           := 0.0;
                                           is_Kinematic : in     Boolean             := False);





   package Forge is

      function new_Box (in_World     : in mmi.World.view;
                        Model        : access openGL.Model.item'class;
                        Mass         : in     math.Real           := 0.0;
                        is_Kinematic : in     Boolean             := False) return my_Box.view;

      function new_Box (bone_Sprites            : in     my_box.bone_Sprites                     ;
                        joint_inv_bind_Matrices : in     joint_inverse_bind_Matrices;
                        joint_site_Offets       : in     joint_to_bone_site_Offets  ;
                        Model                   : access openGL.Model.item'class                   ) return my_Box.view;

   end Forge;



   type joint_Id is (base_To_bone_1, bone_1_To_bone_2);



   function  skin_Sprite   (Self : in     Item'Class)                          return mmi.Sprite.view;
   function  base_Sprite   (Self : in     Item'Class)                          return mmi.Sprite.view;
   function  Sprite        (Self : in     Item'Class;   for_Bone : in bone_Id) return mmi.Sprite.view;


   procedure enable_Graphics (Self : in out Item);
   procedure attach_program_Parameters_to_model_Faces (Self : in out  Item);   -- tbd: move into 'enable_Grpahics'

   procedure joint_inv_bind_Matrices_are (Self : in out Item'Class;   Now : in joint_inverse_bind_Matrices);
   function  joint_inv_bind_Matrices     (Self : in     Item'Class)     return joint_inverse_bind_Matrices;


   function  joint_site_Offets (Self : in Item'Class) return joint_to_bone_site_Offets;

   procedure evolve (Self : in out Item'Class);



   --- Animation
   --
   type scene_joint_Id is (Bone_1, Bone_2);

   type animation_Transforms is array (bone_Id) of math.Matrix_4x4;

   procedure animation_Transforms_are (Self : in out Item'Class;   Now : in animation_Transforms);


   procedure set_x_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Real);

   procedure set_y_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Real);

   procedure set_z_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Real);





private



   type Joints           is array (joint_Id)         of mmi.Joint.view;
   type joint_Transforms is array (collada_joint_Id) of opengl.Matrix_4x4;




   type joint_global_Transforms is array (scene_joint_Id) of math.Matrix_4x4;
   type collada_Joints          is array (scene_joint_Id) of collada.Library.visual_scenes.Node_view;




   type skin_program_Parameters is new opengl.Program.Parameters with
      record
         bone_Transforms : joint_Transforms := (others => openGL.Math.Identity_4x4);
      end record;

   procedure enable (Self : in out skin_program_Parameters);



   type Item is tagged limited
      record
         bone_Sprites            :         my_box.bone_Sprites;
         skin_Sprite             :         mmi.Sprite.view;

         Joints                  :         my_Box.Joints;
         joint_inv_bind_Matrices :         joint_inverse_bind_Matrices;
         joint_site_Offets       :         joint_to_bone_site_Offets;
         joint_global_Transforms :         my_box.joint_global_Transforms;

         collada_Joints          :         my_box.collada_Joints;
         root_Joint              :         collada.Library.visual_scenes.Node_view;

         Model                   : access  openGL.Model.item'class;
         program_Parameters      : aliased skin_program_Parameters;

         animation_Transforms    :         my_Box.animation_Transforms := (others => math.Identity_4x4);
         bone_rest_Rotations     :         my_Box.animation_Transforms := (others => math.Identity_4x4);
      end record;

end my_Box;
