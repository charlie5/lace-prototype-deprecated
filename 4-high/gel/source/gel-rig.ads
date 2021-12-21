with
     gel.Sprite,
     gel.Joint,
     gel.World,

     openGL,
     openGL.Model,
     openGL.Program,

     ada.Strings.unbounded.Hash,
     ada.Containers.Vectors,
     ada.Containers.hashed_Maps;

private
with
     collada.Library.visual_Scenes;

package gel.Rig
--
-- Provides GEL sprites which allow placing a collada skinned/rigged model into a GEL World.
--
-- The rig motion can be controlled either by normal dynamics or pre-canned animations.
--
is
   type Item  is tagged limited private;
   type View  is access all Item'Class;

   type Views is array (Positive range <>) of View;

   use Math;


   --------------
   --- Core Types
   --

   type motion_Mode is (Dynamics, Animation);

   procedure motion_Mode_is (Self : in out Item;   Now : in motion_Mode);


   subtype bone_Id is ada.Strings.unbounded.unbounded_String;

   null_Id : constant bone_Id := ada.Strings.unbounded.null_unbounded_String;


   subtype controller_joint_Id is ada.Strings.unbounded.unbounded_String;


   --------------
   --- Containers
   --

   package inverse_bind_matrix_Vectors is new ada.Containers.Vectors (Positive, Matrix_4x4);
   subtype inverse_bind_matrix_Vector  is inverse_bind_matrix_Vectors.Vector;


   --------------
   --- Joints Ids
   --

   subtype gel_joint_Id is ada.Strings.unbounded.unbounded_String;

   package gel_joint_id_Maps_of_gel_Joint is new ada.Containers.hashed_Maps (Key_type        => gel_joint_Id,
                                                                             Element_type    => gel.Joint.view,
                                                                             Hash            => ada.Strings.unbounded.Hash,
                                                                             equivalent_Keys => ada.Strings.unbounded."=",
                                                                             "="             => gel.Joint."=");
   subtype gel_joint_id_Map_of_gel_Joint is gel_joint_id_Maps_of_gel_Joint.Map;


   package joint_Id_Maps_of_bone_site_offset is new ada.Containers.hashed_Maps (Key_type        => controller_joint_Id,
                                                                                Element_type    => Vector_3,
                                                                                Hash            => ada.Strings.unbounded.Hash,
                                                                                equivalent_Keys => ada.Strings.unbounded."=",
                                                                                "="             => "=");
   subtype joint_Id_Map_of_bone_site_offset is joint_Id_Maps_of_bone_site_offset.Map;


   ------------
   --- Bone Ids
   --

   package bone_id_Maps_of_sprite is new ada.Containers.hashed_Maps (Key_type        => bone_Id,
                                                                     Element_type    => gel.Sprite.view,
                                                                     Hash            => ada.Strings.unbounded.Hash,
                                                                     equivalent_Keys => ada.Strings.unbounded."=",
                                                                     "="             => gel.Sprite."=");
   subtype bone_id_Map_of_sprite is bone_id_Maps_of_sprite.Map;


   ----------------
   --- Bone Details
   --

   type bone_Details is
      record
         Length       : math.Real := 1.0;
         width_Factor,
         depth_Factor : math.Real := 0.1;   -- Factor * Length gives width and depth.

         pitch_Limits,
         yaw_Limits,
         roll_Limits  : gel.Sprite.DoF_Limits := (to_Radians (-15.0),
                                                  to_Radians ( 15.0));
      end record;

   Unspecified : constant := -1.0;

   function to_Details (Length       : Real := Unspecified;
                        width_Factor,
                        depth_Factor : Real := 0.1;

                        pitch_Limits,
                        yaw_Limits,
                        roll_Limits  : gel.Sprite.DoF_Limits := (to_Radians (-15.0),
                                                                 to_Radians ( 15.0))) return bone_Details;

   package bone_id_Maps_of_details is new ada.Containers.hashed_Maps (Key_Type        => bone_id,
                                                                      Element_Type    => bone_Details,
                                                                      Hash            => ada.Strings.unbounded.Hash,
                                                                      Equivalent_Keys => ada.Strings.unbounded."=",
                                                                      "="             => "=");
   subtype bone_id_Map_of_details is bone_id_Maps_of_details.Map;




   ---------
   --- Forge
   --

   package Forge
   is
      function new_Rig (in_World                : in gel.World.view;
                        Model                   : in openGL.Model.view;
                        Mass                    : in Real    := 0.0;
                        is_Kinematic            : in Boolean := False) return Rig.view;

      function new_Rig (bone_Sprites            : in bone_id_Map_of_sprite;
                        joint_inv_bind_Matrices : in inverse_bind_matrix_Vector;
                        joint_site_Offets       : in joint_Id_Map_of_bone_site_offset;
                        Model                   : in openGL.Model.view) return Rig.view;
   end Forge;


   procedure define (Self : in out Item;   in_World     : in gel.World.view;
                                           Model        : in openGL.Model.view;
                                           Mass         : in Real                   := 0.0;
                                           is_Kinematic : in Boolean                := False;
                                           bone_Details : in bone_id_Map_of_details := bone_id_Maps_of_details.empty_Map);


   --------------
   --- Attributes
   --

   procedure Site_is (Self : in out Item;   Now : in Vector_3);
   procedure Spin_is (Self : in out Item;   Now : in Matrix_3x3);

   function  bone_Sprites (Self : in Item)       return bone_id_Map_of_sprite;
   function  skin_Sprite  (Self : in Item'Class) return gel.Sprite.view;
   function  base_Sprite  (Self : in Item'Class) return gel.Sprite.view;
   function  Sprite       (Self : in Item'Class;
                           Bone : in bone_Id)    return gel.Sprite.view;

   function  Joints (Self : in Item) return gel_joint_id_Map_of_gel_Joint;

   procedure joint_inv_bind_Matrices_are (Self : in out Item'Class;   Now : in inverse_bind_matrix_Vector);
   function  joint_inv_bind_Matrices     (Self : in     Item'Class)     return inverse_bind_matrix_Vector;

   function  joint_site_Offets (Self : in Item'Class) return joint_Id_Map_of_bone_site_offset;

   procedure assume_Pose     (Self : in out Item);
   procedure enable_Graphics (Self : in out Item);
   procedure evolve          (Self : in out Item'Class;   world_Age : in Duration);


   -------------
   --- Animation
   --

   subtype scene_joint_Id is ada.Strings.unbounded.unbounded_String;

   package bone_id_Maps_of_transform is new ada.Containers.hashed_Maps (Key_Type        => bone_id,
                                                                        Element_Type    => Matrix_4x4,
                                                                        Hash            => ada.Strings.unbounded.Hash,
                                                                        Equivalent_Keys => ada.Strings.unbounded."=",
                                                                        "="             => "=");
   subtype bone_id_Map_of_transform is bone_id_Maps_of_transform.Map;

   procedure animation_Transforms_are (Self : in out Item'Class;   Now : in bone_id_Map_of_transform);


   type axis_Kind is (x_Axis, y_Axis, z_Axis);

   procedure set_rotation_Angle   (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               Axis      : in Axis_Kind;
                                                               To        : in Real);     -- TODO: Use Radians type (and below).
   procedure set_x_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in Real);
   procedure set_y_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in Real);
   procedure set_z_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in Real);

   procedure set_Location   (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                         To        : in Vector_3);
   procedure set_Location_x (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                         To        : in Real);
   procedure set_Location_y (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                         To        : in Real);
   procedure set_Location_z (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                         To        : in Real);
   procedure set_Transform  (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                         To        : in Matrix_4x4);

   procedure update_all_global_Transforms (Self : in out Item'Class);

   procedure animate         (Self : in out Item;   world_Age : in Duration);
   procedure reset_Animation (Self : in out Item);



private

   -- gl_transform_Vector
   --
   package gl_transform_Vectors is new ada.Containers.Vectors (Positive, openGL.Matrix_4x4);
   subtype gl_transform_Vector  is gl_transform_Vectors.Vector;


   -- joint_id_Map_of_matrix_4x4
   --
   package joint_id_Maps_of_matrix_4x4 is new ada.Containers.hashed_Maps (Key_type        => scene_joint_Id,
                                                                          Element_type    => Matrix_4x4,
                                                                          Hash            => ada.Strings.unbounded.Hash,
                                                                          equivalent_Keys => ada.Strings.unbounded."=",
                                                                          "="             => "=");
   subtype joint_id_Map_of_matrix_4x4 is joint_id_Maps_of_matrix_4x4.Map;


   -- joint_id_Map_of_scene_node
   --
   package joint_id_Maps_of_scene_node is new ada.Containers.hashed_Maps (Key_type        => scene_joint_Id,
                                                                          Element_type    => collada.Library.visual_Scenes.Node_view,
                                                                          Hash            => ada.Strings.unbounded.Hash,
                                                                          equivalent_Keys => ada.Strings.unbounded."=",
                                                                          "="             => collada.Library.visual_Scenes."=");
   subtype joint_id_Map_of_scene_node is joint_id_Maps_of_scene_node.Map;


   -- joint_id_Map_of_slot
   --
   package joint_id_Maps_of_slot is new ada.Containers.hashed_Maps (Key_type        => scene_joint_Id,
                                                                    Element_type    => Positive,
                                                                    Hash            => ada.Strings.unbounded.Hash,
                                                                    equivalent_Keys => ada.Strings.unbounded."=",
                                                                    "="             => "=");
   subtype joint_id_Map_of_slot is joint_id_Maps_of_slot.Map;


   -- skin_program_Parameters
   --
   type skin_program_Parameters is new opengl.Program.Parameters with
      record
         bone_Transforms   : gl_transform_Vector;
         joint_Map_of_slot : joint_id_Map_of_slot;
      end record;

   overriding
   procedure enable (Self : in out skin_program_Parameters);


   -- joint_id_Map_of_joint_id
   --
   package joint_id_Maps_of_joint_id is new ada.Containers.hashed_Maps (Key_type        => scene_joint_Id,
                                                                        Element_type    => scene_joint_Id,
                                                                        Hash            => ada.Strings.unbounded.Hash,
                                                                        equivalent_Keys => ada.Strings.unbounded."=",
                                                                        "="             => ada.Strings.unbounded."=");
   subtype joint_id_Map_of_joint_id is joint_id_Maps_of_joint_id.Map;


   -- scene_Joint
   --
   type scene_Joint is
      record
         Node      : collada.Library.visual_Scenes.Node_view;
         Transform : Matrix_4x4;
      end record;

   package joint_id_Maps_of_scene_Joint is new ada.Containers.hashed_Maps (Key_type        => scene_joint_Id,
                                                                           Element_type    => scene_Joint,
                                                                           Hash            => ada.Strings.unbounded.Hash,
                                                                           equivalent_Keys => ada.Strings.unbounded."=",
                                                                           "="             => "=");
   subtype joint_id_Map_of_scene_Joint is joint_id_Maps_of_scene_Joint.Map;


   -- Transform
   --
   type Transform is
      record
         Rotation    : Quaternion := linear_Algebra_3D.to_Quaternion (linear_Algebra_3D.x_Rotation_from (0.0));
         Translation : Vector_3   := (0.0, 0.0, 0.0);
      end record;

   type Transforms      is array (Positive range <>) of Transform;
   type Transforms_view is access all Transforms;


   -- animation_Channel
   --
   type animation_Channel is
      record
         Target            : access collada.Library.visual_Scenes.Transform;
         target_Joint      :        scene_joint_Id;

         Times             : access collada.float_Array;
         Values            : access collada.float_Array;

         Cursor            :        Index := 0;       -- Current frame of the anmination.

         initial_Angle     :        Real;             -- For angle interpolation during 'rotation' animation.
         current_Angle     :        Real  := 0.0;     --
         interp_Delta      :        Real  := 0.0;     --

         initial_Site      :        Vector_3;         -- For location interpolation during 'translation' animation.
         current_Site      :        Vector_3;         --
         site_interp_Delta :        Vector_3;         --

         initial_Transform :        Transform;        -- For matrix interpolation during 'full_transform' animation.
         current_Transform :        Transform;        --
         slerp_Time        :        Real;             -- Slerp Time (T) value in range '0.0 .. 1.0'.     -- TODO: use 'unit_Interval' type.
         Transforms        :        Transforms_view;
         Transform_interp_Delta :   Real;             -- Rate at which the SLERP time parameter increases.
      end record;

   subtype channel_Id is scene_joint_Id;
   package channel_id_Maps_of_animation_Channel is new ada.Containers.hashed_Maps (Key_Type        => channel_Id,
                                                                                   Element_Type    => animation_Channel,
                                                                                   Hash            => ada.Strings.unbounded.Hash,
                                                                                   Equivalent_Keys => ada.Strings.unbounded."=",
                                                                                   "="             => "=");
   subtype channel_id_Map_of_animation_Channel is channel_id_Maps_of_animation_Channel.Map;


   -- Rig Item
   --
   type Item is tagged limited
      record
         Mode                    : motion_Mode := Dynamics;

         joint_Sprites           : bone_id_Map_of_sprite;                   -- Sprite to show location/rotation of joints (mainly for debugging).
         bone_Sprites            : bone_id_Map_of_sprite;                   -- A sprite for each bone.
         skin_Sprite             : gel.Sprite.view;                         -- A sprite for the skin.

         bind_shape_Matrix       : Matrix_4x4;

         Joints                  : gel_joint_id_Map_of_gel_Joint;
         joint_inv_bind_Matrices : inverse_bind_matrix_Vector;              -- The joint inverse transforms when in the bind pose.

         phys_joint_site_Offets  : joint_Id_Map_of_bone_site_offset;        -- Offset from the bone site to the joint site when in the bind pose.
         anim_joint_site_Offets  : joint_Id_Map_of_bone_site_offset;        -- Offset from the bone site to the joint site when in the bind pose.

         joint_pose_Transforms   : joint_id_Map_of_matrix_4x4;              -- The joint transforms when in the skeletal pose.
         joint_Parent            : joint_id_Map_of_joint_id;

         collada_Joints          : joint_id_Map_of_scene_node;
         scene_Joints            : joint_id_Map_of_scene_Joint;
         root_Joint              : collada.Library.visual_scenes.Node_view;

         animation_Transforms    : bone_id_Map_of_transform;
         bone_pose_Transforms    : bone_id_Map_of_transform;                -- The bone transforms when in the skeletal pose.

         Channels                : channel_id_Map_of_animation_Channel;
         start_Time              : Duration := 0.0;

         overall_Site            : Vector_3 := (0.0, 0.0, 0.0);

         Model                   : openGL.Model.view;
         program_Parameters      : aliased skin_program_Parameters;
      end record;


   function Parent_of             (Self : in Item;   the_Bone : in bone_Id) return bone_Id;
   function joint_site_Offet      (Self : in Item;   for_Bone : in bone_Id) return Vector_3;
   function joint_inv_bind_Matrix (Self : in Item;   for_Bone : in bone_Id) return Matrix_4x4;
   function joint_bind_Matrix     (Self : in Item;   for_Bone : in bone_Id) return Matrix_4x4;


end gel.Rig;
