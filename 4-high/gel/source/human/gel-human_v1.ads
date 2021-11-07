with
     gel.Sprite,
     gel.Joint,
     gel.human_Types_v1,
     physics.Model,
     openGL.Model,
     openGL.Program;

limited
with
     gel.World;

private
with
     collada.Library.visual_scenes;


package gel.Human_v1
--
--  Provides access to and control of a 'make_human' produced model.
--
is
   type Item  is tagged limited private;
   type View  is access all Item'Class;
   type Views is array (math.Index range <>) of View;


   procedure define (Self : in out Item;   World         : access gel    .World.item'Class;
                                           Model         : access openGL .Model.item'Class;
                                           physics_Model : access physics.Model.item'Class;
                                           Mass          : in     math.Real := 0.0;
                                           is_Kinematic  : in     Boolean   := True);


   type bone_Sprites is array (human_types_v1.bone_Id) of gel.Sprite.view;


   procedure use_Model (Named : in String);


   package Forge
   is
      function new_Human (World         : access gel    .World.item'Class;
                          Model         : access openGL .Model.item'Class;
                          physics_Model : access physics.Model.item'Class;
                          Mass          : in     math.Real := 0.0;
                          is_Kinematic  : in     Boolean   := False) return Human_v1.view;

      function new_Human (bone_Sprites      : in     human_v1.bone_Sprites;
                          controller_Joints : in     human_types_v1.controller_Joints;
                          Model             : access openGL.Model.item'Class) return Human_v1.view;
   end Forge;


   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);


   type motion_Mode is (Physics, Animation);

   procedure motion_Mode_is (Self : in out Item;   Now : in motion_Mode);

   function  skin_Sprite   (Self     : in Item'Class)             return gel.Sprite.view;
   function  base_Sprite   (Self     : in Item'Class)             return gel.Sprite.view;
   function  Sprite        (Self     : in Item'Class;
                            for_Bone : in human_types_v1.bone_Id) return gel.Sprite.view;

   procedure controller_Joints_are (Self : in out Item'Class;   Now : in human_types_v1.controller_Joints);
   function  controller_Joints     (Self : in     Item'Class)     return human_types_v1.controller_Joints;

   procedure evolve (Self : in out Item'Class;   world_Age : in Duration);


   -------------
   --- Animation
   --

   type scene_joint_Id is (-- Armature,
                           Hips,
                           Thigh_L, Shin_L, Foot_L, Toe_L,
                           Thigh_R, Shin_R, Foot_R, Toe_R,
                           Spine, Chest,
                           Clavicle_R, upper_Arm_R, Forearm_R, Hand_R, Thumb_02_R, Thumb_03_R, F_ring_01_R, F_index_01_R,
                           Clavicle_L, upper_Arm_L, Forearm_L, Hand_L, Thumb_02_L, Thumb_03_L, F_ring_01_L, F_index_01_L,
                           Neck,
                           Head, Jaw, Eye_R, Eye_L);



--     type controller_joint_Id is (Eye_L,             Eye_R,
--                                  Head,
--                                  Jaw,
--                                  Chest,
--                                  Clavicle_L,        Clavicle_R,
--                                  Foot_L,            Foot_R,
--                                  Forearm_L,         Forearm_R,
--                                  Hips,
--                                  Neck,
--                                  Shin_L,            Shin_R,
--                                  Spine,
--                                  Thigh_L,           Thigh_R,
--                                  Toe_L,             Toe_R,
--                                  upper_Arm_L,       upper_Arm_R,
--                                  Finger_index_01_L, Finger_index_01_R,
--                                  Finger_ring_01_L,  Finger_ring_01_R,
--                                  Hand_L,            Hand_R,
--                                  Thumb_02_L,        Thumb_02_R,
--                                  Thumb_03_L,        Thumb_03_R);




--     type scene_joint_Id is (Armature,
--                             MasterFloor,
--                             Root,
--                             Hips,
--                             UpLeg_L, LoLeg_L, Foot_L, Toe_L,
--                             UpLeg_R, LoLeg_R, Foot_R, Toe_R,
--                             Spine1, Spine2, Spine3,
--                             Neck,   Head,   Jaw,
--                             TongueBase, TongueMid, TongueTip,
--                             Eye_R,   Eye_L,
--                             UpLid_R, LoLid_R,
--                             UpLid_L, LoLid_L,
--
--                             Clavicle_L, UpArm_L, LoArm_L, Hand_L,
--
--                             Wrist_1_L,
--                             Palm_2_L,  Finger_2_1_L, Finger_2_2_L, Finger_2_3_L,
--                             Palm_3_L,  Finger_3_1_L, Finger_3_2_L, Finger_3_3_L,
--                             Wrist_2_L,
--                             Palm_4_L,  Finger_4_1_L, Finger_4_2_L, Finger_4_3_L,
--                             Palm_5_L,  Finger_5_1_L, Finger_5_2_L, Finger_5_3_L,
--                             Palm_1_L,  Finger_1_1_L, Finger_1_2_L, Finger_1_3_L,
--
--                             Clavicle_R, UpArm_R, LoArm_R, Hand_R,
--
--                             Wrist_1_R,
--                             Palm_2_R,  Finger_2_1_R, Finger_2_2_R, Finger_2_3_R,
--                             Palm_3_R,  Finger_3_1_R, Finger_3_2_R, Finger_3_3_R,
--                             Wrist_2_R,
--                             Palm_4_R,  Finger_4_1_R, Finger_4_2_R, Finger_4_3_R,
--                             Palm_5_R,  Finger_5_1_R, Finger_5_2_R, Finger_5_3_R,
--                             Palm_1_R,  Finger_1_1_R, Finger_1_2_R, Finger_1_3_R,
--
--                             Wrist_L, Wrist_R,
--                             Ankle_L, Ankle_R);


   type axis_Kind is (x_Axis, y_Axis, z_Axis);

   procedure set_rotation_Angle   (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               Axis      : in Axis_Kind;
                                                               To        : in math.Real);

   procedure set_x_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Real);

   procedure set_y_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Real);

   procedure set_z_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Real);

   procedure set_Location         (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Vector_3);

   procedure set_Transform        (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Matrix_4x4);

   procedure update_all_global_Transforms (Self : in out Item'Class);


   procedure animate         (Self : in out Item;   world_Age : in Duration);
   procedure reset_Animation (Self : in out Item);


   ----------------
   --- Display Mode
   --
   type display_Mode is (Skin, Bones, Skin_and_Bones);

   procedure Mode_is (Now : in display_Mode);



private
   use Human_types_v1;


   the_display_Mode : display_Mode := Skin;


   type Joints is array (controller_joint_Id) of gel.Joint.view;


   type scene_Joint is
      record
         Node      : collada.Library.visual_scenes.Node_view;
         Transform : math.Matrix_4x4;
      end record;

   type scene_Joints is array (scene_joint_Id) of scene_Joint;



   type joint_Transforms is array (controller_joint_Id) of opengl.Matrix_4x4;

   type skin_program_Parameters is new opengl.Program.Parameters with
      record
         bone_Transforms : human_v1.joint_Transforms := (others => opengl.math.Identity_4x4);
      end record;

   overriding
   procedure enable (Self : in out skin_program_Parameters);


   -------------
   --- Animation
   --

   type channel_Id is new scene_joint_Id range Hips .. scene_joint_Id'Last;

--     type channel_Id is (root_loc,     root_x,       root_y,       root_z,
--  --                         hips_x,       hips_y,       hips_z,
--                         spine_1_x,    spine_1_y,    spine_1_z,
--                         spine_2_x,    spine_2_y,    spine_2_z,
--                         spine_3_x,    spine_3_y,    spine_3_z,
--                         neck_x,       neck_y,       neck_z,
--                         head_x,       head_y,       head_z,
--
--                         l_clavicle_x, l_clavicle_y, l_clavicle_z,
--                         l_uparm_x,    l_uparm_y,    l_uparm_z,
--                         l_loarm_x,    l_loarm_y,    l_loarm_z,
--                         l_hand_x,     l_hand_y,     l_hand_z,
--                         l_wrist_loc,  l_wrist_x,    l_wrist_y,    l_wrist_z,
--
--                         r_clavicle_x, r_clavicle_y, r_clavicle_z,
--                         r_uparm_x,    r_uparm_y,    r_uparm_z,
--                         r_loarm_x,    r_loarm_y,    r_loarm_z,
--                         r_hand_x,     r_hand_y,     r_hand_z,
--                         r_wrist_loc,  r_wrist_x,    r_wrist_y,    r_wrist_z,
--
--                         l_upleg_x,    l_upleg_y,    l_upleg_z,
--                         l_loleg_x,    l_loleg_y,    l_loleg_z,
--                         l_foot_x,     l_foot_y,     l_foot_z,
--
--                         r_upleg_x,    r_upleg_y,    r_upleg_z,
--                         r_loleg_x,    r_loleg_y,    r_loleg_z,
--                         r_foot_x,     r_foot_y,     r_foot_z
--                        );

   type Transform is
      record
         Rotation    : math.Quaternion;
         Translation : math.Vector_3;
      end record;

   type Transforms      is array (Positive range <>) of Transform;
   type Transforms_view is access all Transforms;


   type animation_Channel is
      record
         Target        : access collada.Library.visual_scenes.Transform;
         Times         : access collada.float_Array;

         Values        : access collada.float_Array;
--           Transforms    : access collada.Matrix_4x4_array;

         Cursor        :        math.Index         := 0;       -- Current frame of the anmination.

         initial_Angle : math.Real;                            -- For angle interpolation during 'rotation' animation.
         current_Angle : math.Real                 := 0.0;     --
         interp_Delta  : math.Real                 := 0.0;     --

         initial_Site      : math.Vector_3;                    -- For location interpolation during 'translation' animation.
         current_Site      : math.Vector_3;                    --
         site_interp_Delta : math.Vector_3;                    --

         initial_Transform      : Transform;                   -- For matrix interpolation during 'full_transform' animation.
         current_Transform      : Transform;                   --
         slerp_Time             : math.Real;                   -- Slerp Time (T) value in range 0.0 .. 1.0.
         Transform_interp_Delta : math.Real;                   -- Rate at which the SLERP time parameter increases.
         Transforms             : Transforms_view;
      end record;


   type animation_Channels is array (channel_Id) of animation_Channel;


   --------------
   --- Human Item
   --

   type Item is tagged limited
      record
         Mode               :         human_v1.motion_Mode := Physics;

         Space              :         gel.Sprite.physics_Space_view;

         bone_Sprites       :         human_v1.bone_Sprites;
         skin_Sprite        :         gel.Sprite.view;

         Joints             :         human_v1.Joints;
         controller_Joints  :         human_types_v1.controller_Joints;

         scene_Joints       :         human_v1.scene_Joints;
         root_Joint         :         collada.Library.visual_scenes.Node_view;

         Model              : access  openGL.Model.item'class;
         program_Parameters : aliased skin_program_Parameters;

         Channels           :         animation_Channels;
         start_Time         :         Duration          := 0.0;

         Graphics_enabled   :         Boolean           := False;

--           animation_Origin   :         math.Matrix_4x4   := to_rotate_Matrix (y_Rotation_from (math.to_Radians (90.0))); -- math.Identity_4x4;
--           animation_Origin   :         math.Matrix_4x4   := to_transform_Matrix (math.Inverse (y_Rotation_from (math.to_Radians (-45.0))),
--                                                                                  (0.0, 5.0, 0.0));
      end record;


   procedure enable_Graphics (Self : in out Item);

end gel.Human_v1;
