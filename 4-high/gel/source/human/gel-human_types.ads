
package gel.human_Types
--
--  Provides core types for defining a Human.
--
is
   pragma Pure;

   type controller_joint_Id is (MasterFloor,
                                Root,
                                Hips,
                                UpLeg_L, LoLeg_L, Foot_L, Toe_L,
                                UpLeg_R, LoLeg_R, Foot_R, Toe_R,
                                Spine1, Spine2, Spine3,
                                Neck,   Head,   Jaw,
                                TongueBase, TongueMid, TongueTip,
                                Eye_R,   Eye_L,
                                UpLid_R, LoLid_R,
                                UpLid_L, LoLid_L,

                                Clavicle_L, UpArm_L, LoArm_L, Hand_L,

                                Wrist_1_L,
                                Palm_2_L,  Finger_2_1_L, Finger_2_2_L, Finger_2_3_L,
                                Palm_3_L,  Finger_3_1_L, Finger_3_2_L, Finger_3_3_L,
                                Wrist_2_L,
                                Palm_4_L,  Finger_4_1_L, Finger_4_2_L, Finger_4_3_L,
                                Palm_5_L,  Finger_5_1_L, Finger_5_2_L, Finger_5_3_L,
                                Palm_1_L,  Finger_1_1_L, Finger_1_2_L, Finger_1_3_L,

                                Clavicle_R, UpArm_R, LoArm_R, Hand_R,

                                Wrist_1_R,
                                Palm_2_R,  Finger_2_1_R, Finger_2_2_R, Finger_2_3_R,
                                Palm_3_R,  Finger_3_1_R, Finger_3_2_R, Finger_3_3_R,
                                Wrist_2_R,
                                Palm_4_R,  Finger_4_1_R, Finger_4_2_R, Finger_4_3_R,
                                Palm_5_R,  Finger_5_1_R, Finger_5_2_R, Finger_5_3_R,
                                Palm_1_R,  Finger_1_1_R, Finger_1_2_R, Finger_1_3_R,

                                Wrist_L, Wrist_R,
                                Ankle_L, Ankle_R);

   type bone_Id is new controller_joint_Id range Hips .. controller_joint_Id'Last;


   type controller_Joint is
      record
         inverse_bind_Matrix      : math.Matrix_4x4;
         joint_to_bone_site_Offet : math.Vector_3;       -- The 'bind time' offset from a joint to its bone.
      end record;

   type controller_Joints is array (controller_joint_Id) of controller_Joint;


end gel.human_Types;
