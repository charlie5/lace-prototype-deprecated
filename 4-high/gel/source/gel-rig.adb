with
     gel.Forge,
     gel.Conversions,
     physics.Model,

     openGL.Model.any,
     opengl.Palette,
     opengl.Program .lit.colored_textured_skinned,
     opengl.Geometry.lit_colored_textured_skinned,

     collada.Document,
     collada.Library,
     collada.Library.controllers,
     collada.Library.animations,

     ada.Strings.unbounded,
     ada.Strings.Maps;

package body gel.Rig
is
   use linear_Algebra_3D;


   -----------
   --- Utility
   --

   function "+" (From : in ada.strings.unbounded.unbounded_String) return String
     renames ada.strings.unbounded.to_String;

   function "+" (From : in String) return ada.strings.unbounded.unbounded_String
     renames ada.strings.unbounded.to_unbounded_String;



   function to_gel_joint_Id (Parent, Child : in bone_Id) return gel_joint_Id
   is
      use ada.Strings.unbounded;
   begin
      return Parent & "_to_" & Child;
   end to_gel_joint_Id;



   function to_Math (From : in collada.Matrix_4x4) return math.Matrix_4x4
   is
   begin
      return (1 => (From (1, 1),  From (1, 2),  From (1, 3),  From (1, 4)),
              2 => (From (2, 1),  From (2, 2),  From (2, 3),  From (2, 4)),
              3 => (From (3, 1),  From (3, 2),  From (3, 3),  From (3, 4)),
              4 => (From (4, 1),  From (4, 2),  From (4, 3),  From (4, 4)));
   end to_Math;
   pragma Unreferenced (to_Math);



   function to_Details (Length       : Real := Unspecified;
                        width_Factor,
                        depth_Factor : Real := 0.1;

                        pitch_Limits,
                        yaw_Limits,
                        roll_Limits  : gel.Sprite.DoF_Limits := (to_Radians (-15.0),
                                                                 to_Radians ( 15.0))) return bone_Details
   is
   begin
      return (Length,       width_Factor, depth_Factor,
              pitch_Limits, yaw_Limits,   roll_Limits);
   end to_Details;


   ---------
   --- Forge
   --

   package body Forge
   is

      function new_Rig (in_World     : in gel.World.view;
                        Model        : in openGL.Model.view;
                        Mass         : in Real    := 0.0;
                        is_Kinematic : in Boolean := False) return Rig.view
      is
         Self : constant Rig.view := new Rig.item;
      begin
         Self.define (in_World, Model, Mass, is_Kinematic);

         return Self;
      end new_Rig;



      function new_Rig (bone_Sprites            : in bone_id_Map_of_sprite;
                        joint_inv_bind_Matrices : in inverse_bind_matrix_Vector;
                        joint_site_Offets       : in joint_Id_Map_of_bone_site_offset;
                        Model                   : in openGL.Model.view) return Rig.view
      is
         the_Box : constant Rig.View := new Rig.item;
      begin
         the_Box.bone_Sprites            := bone_Sprites;
         the_Box.joint_inv_bind_Matrices := joint_inv_bind_Matrices;
         the_Box.phys_joint_site_Offets  := joint_site_Offets;
         the_Box.Model                   := Model;

         return the_Box;
      end new_Rig;

   end Forge;


   ---------------------------
   --- Skin program parameters
   --

   overriding
   procedure enable (Self : in out skin_program_Parameters)
   is
      use joint_id_Maps_of_slot;

      subtype Program_view is openGL.Program.lit.colored_textured_skinned.view;

      Cursor : joint_id_Maps_of_slot.Cursor := Self.joint_Map_of_slot.First;
      Slot   : Integer;

   begin
      while has_Element (Cursor)
      loop
         Slot := Element (Cursor);

         Program_view (Self.Program).bone_Transform_is (Which => Slot,
                                                        Now   => Self.bone_Transforms.Element (Slot));
         next (Cursor);
      end loop;
   end enable;


   -------------
   --- Animation
   --

   procedure define_global_Transform_for (Self : in out Item'Class;   the_Joint : in     collada.Library.visual_scenes.Node_view;
                                                                      Slot      : in out Positive)
   is
      use collada.Library;

      which_Joint          : constant scene_joint_Id      := the_Joint.Id;
      child_Joints         : constant visual_scenes.Nodes := the_Joint.Children;

      default_scene_Joint  :          scene_Joint;
      the_global_Transform : constant Matrix_4x4 := Transpose (the_Joint.global_Transform);     -- Transpose to convert to row-major.

   begin
      Self.joint_pose_Transforms.insert (which_Joint,  the_global_Transform);
      Self.collada_Joints       .insert (which_Joint,  the_Joint);

      default_scene_Joint.Node := the_Joint;
      Self.scene_Joints.insert (which_Joint, default_scene_Joint);

      for i in child_Joints'Range
      loop
         Slot := Slot + 1;
         define_global_Transform_for (Self, child_Joints (i), Slot);      -- Recurse over children.
      end loop;
   end define_global_Transform_for;



   procedure update_global_Transform_for (Self : in out Item'Class;   the_Joint : in collada.Library.visual_scenes.Node_view)
   is
      use collada.Library,
          ada.Strings.unbounded;

      which_Joint          : constant scene_joint_Id      := the_Joint.Id;
      child_Joints         : constant visual_scenes.Nodes := the_Joint.Children;

      the_global_Transform : constant Matrix_4x4 := math.Transpose (the_Joint.global_Transform);     -- Transpose to convert to row-major.
      joint_site_Offet     :          Vector_3;

   begin
      if which_Joint = Self.root_Joint.Name
      then   joint_site_Offet := (0.0, 0.0, 0.0);
      else   joint_site_Offet := Self.anim_joint_site_Offets (which_Joint);
      end if;


      Self.joint_pose_Transforms.replace (which_Joint, (the_global_Transform));
      Self.scene_Joints (which_Joint).Transform      := the_global_Transform;

      declare
         use type gel.Sprite.view;

         the_bone_Id : constant bone_Id := which_Joint;
         Site        :          Vector_3;
         Rotation    :          Matrix_3x3;

      begin
         if Self.bone_Sprites (the_bone_Id) /= null
         then
            Site := get_Translation (the_global_Transform);
            Site := Site - joint_site_Offet * (get_Rotation (the_global_Transform));
            Site := Site * Inverse (Self.base_Sprite.Spin);
            Site := Site + Self.overall_Site;

            Rotation := Inverse (get_Rotation (the_global_Transform));
            Rotation := Self.base_Sprite.Spin * Rotation;

            Self.bone_Sprites (the_bone_Id).all.Site_is (Site);

            if which_Joint /= Self.root_Joint.Name
            then
               Self.bone_Sprites (the_bone_Id).all.Spin_is (Rotation);
            end if;
         end if;
      end;

      for i in child_Joints'Range
      loop
         Self.update_global_Transform_for (child_Joints (i));      -- Recurse over children.
      end loop;
   end update_global_Transform_for;



   procedure update_all_global_Transforms (Self : in out Item'Class)
   is
   begin
      Self.update_global_Transform_for (Self.root_Joint);            -- Re-determine all joint transforms, recursively.
   end update_all_global_Transforms;



   procedure set_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                             Axis      : in axis_Kind;
                                                             To        : in Real)
   is
   begin
      case Axis is
         when x_Axis =>   Self.set_x_rotation_Angle (for_Joint, To);
         when y_Axis =>   Self.set_y_rotation_Angle (for_Joint, To);
         when z_Axis =>   Self.set_z_rotation_Angle (for_Joint, To);
      end case;
   end set_rotation_Angle;



   procedure set_Location (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                       To        : in Vector_3)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_Location (To);
   end set_Location;



   procedure set_Location_x (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                         To        : in Real)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_Location_x (To);
   end set_Location_x;


   procedure set_Location_y (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                         To        : in Real)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_Location_y (To);
   end set_Location_y;


   procedure set_Location_z (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                         To        : in Real)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_Location_z (To);
   end set_location_z;



   procedure set_Transform (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                        To        : in Matrix_4x4)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_Transform (To);
   end set_Transform;



   procedure set_x_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in Real)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_x_rotation_Angle (To);
   end set_x_rotation_Angle;



   procedure set_y_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in Real)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_y_rotation_Angle (To);
   end set_y_rotation_Angle;



   procedure set_z_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in Real)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_z_rotation_Angle (To);
   end set_z_rotation_Angle;


   ----------
   --- Define
   --

   procedure define (Self : in out Item;   in_World     : in gel   .World.view;
                                           Model        : in openGL.Model.view;
                                           Mass         : in Real                   := 0.0;
                                           is_Kinematic : in Boolean                := False;
                                           bone_Details : in bone_id_Map_of_details := bone_id_Maps_of_details.empty_Map)
   is
      use collada.Document,
          collada.Library,
          collada.Library.visual_Scenes,

          ada.Strings.unbounded,
          ada.Strings;

      type any_Model_view is access all openGL.Model.any.item;

      the_Model    : constant any_Model_view        := any_Model_view (Model);
      the_Document : constant collada.Document.item := to_Document (openGL.to_String (the_Model.model_Name));


      function get_root_Joint return visual_Scenes.Node_view
      is
      begin
         if the_Document.Libraries.visual_Scenes.skeletal_Root = ""
         then
            return the_Document.Libraries.visual_Scenes.Contents (1).root_Node;
         else
            return the_Document.Libraries.visual_Scenes.Contents (1).root_Node.Child (1);
         end if;
      end get_root_Joint;


      the_root_Joint    : constant visual_scenes.Node_view := get_root_Joint;
      prior_bone_Length :          Real                    := 1.0;


      package joint_id_Maps_of_vector_3 is new ada.Containers.hashed_Maps (Key_type        => scene_joint_Id,
                                                                           Element_type    => Vector_3,
                                                                           Hash            => ada.Strings.unbounded.Hash,
                                                                           equivalent_Keys => ada.Strings.unbounded."=",
                                                                           "="             => "=");
      subtype joint_id_Map_of_vector_3 is joint_id_Maps_of_vector_3.Map;


      joint_Sites : joint_id_Map_of_vector_3;

      procedure set_Site_for (the_Joint : in visual_Scenes.Node_view)
      is
         which_Joint  : constant scene_joint_Id      := the_Joint.Id;
         child_Joints : constant visual_Scenes.Nodes := the_Joint.Children;

      begin
         if which_Joint = Self.root_Joint.Name
         then
            joint_Sites.insert (which_Joint,
                                (0.0, 0.0, 0.0));
         else
            joint_Sites.insert (which_Joint,
                                get_Translation (Self.joint_bind_Matrix (which_Joint)));
         end if;

         for i in child_Joints'Range
         loop
            set_Site_for (child_Joints (i));     -- Recurse over children.
         end loop;
      end set_Site_for;


      procedure create_Bone (the_Bone    : in bone_Id;
                             start_Joint : in scene_joint_Id;
                             end_Point   : in Vector_3;
                             Scale       : in Vector_3;
                             Mass        : in Real)
      is
         pragma Unreferenced (Mass);
         use opengl.Palette;

         new_Sprite    :          gel.Sprite.view;
         the_bone_Site : constant Vector_3  := midPoint (joint_Sites (start_Joint),
                                                         end_Point);
      begin
         if the_Bone = Self.root_Joint.Name
         then
            declare
               use standard.physics.Model;

               Size : constant Vector_3 := (0.1, 0.1, 0.1);

               physics_Model : constant standard.physics.Model.View
                 := standard.physics.Model.Forge.new_physics_Model (shape_Info  => (Kind         => Cube,
                                                                                    half_Extents => Size / 2.0),
                                                                    Mass        => 1.0);
            begin
               new_Sprite := gel.Sprite.Forge.new_Sprite ("Skin Sprite",
                                                          gel.sprite.World_view (in_World),
                                                          math.Origin_3D,
                                                          Model,
                                                          physics_Model,
                                                          is_Kinematic => is_Kinematic);
            end;

            new_Sprite.Site_is ((0.0, 0.0, 0.0));
            new_Sprite.Spin_is (Identity_3x3);

            Self.bone_pose_Transforms.insert (the_Bone, Identity_4x4);
            Self.skin_Sprite := new_Sprite;

         else
            new_Sprite := gel.Forge.new_box_Sprite (in_World     => in_World.all'Access,
                                                    Mass         => 1.0,
                                                    Size         => Scale,
                                                    Colors       => (1      => Black,
                                                                     3      => Green,
                                                                     4      => Blue,
                                                                     others => Red),
                                                    is_Kinematic => is_Kinematic);
            new_Sprite.Site_is (the_bone_Site);
            new_Sprite.Spin_is (Inverse (get_Rotation (Self.joint_bind_Matrix (start_Joint))));

            new_Sprite.is_Visible (False);

            Self.anim_joint_site_Offets.insert (the_Bone,   Inverse (get_Rotation (Self.joint_inv_bind_Matrix (start_Joint)))
                                                          * (joint_Sites (start_Joint) - the_bone_Site));

            Self.phys_joint_site_Offets.insert (the_Bone,  joint_Sites (start_Joint) - the_bone_Site);


            Self.bone_pose_Transforms  .insert (the_Bone,  to_transform_Matrix (Rotation    => get_Rotation (Self.joint_pose_Transforms (start_Joint)),
                                                                                Translation => the_bone_Site));
         end if;

         Self.bone_Sprites.insert (the_Bone, new_Sprite);

         declare
            new_Sprite : constant gel.Sprite.view := gel.Forge.new_box_Sprite (in_World     => in_World,
                                                                               Mass         => 0.0,
                                                                               Size         => (0.02, 0.02, 0.02),
                                                                               Colors       => (others => Yellow),
                                                                               is_Kinematic => True);
         begin
            Self.joint_Sprites.insert (the_Bone, new_Sprite);
         end;
      end create_Bone;


      procedure create_Bone_for (the_Joint : in visual_Scenes.Node_view;   Parent : in bone_Id)
      is
         use bone_id_Maps_of_details;

         which_Joint      : constant scene_joint_Id      := the_Joint.Id;
         child_Joints     : constant visual_Scenes.Nodes := the_Joint.Children;

         the_bone_Details :          Rig.bone_Details;

         bone_Length      :          Real;
         end_Point        :          Vector_3;

         new_Joint        :          gel.Joint.view;


         function guessed_bone_Length return Real
         is
         begin
            if child_Joints'Length = 0
            then
               return prior_bone_Length;

            else
               if which_Joint = Self.root_Joint.Name
               then
                  return Distance (joint_Sites.Element (which_Joint),
                                   joint_Sites.Element (child_Joints (child_Joints'First).Id));
               else
                  return Distance (joint_Sites.Element (which_Joint),
                                   joint_Sites.Element (child_Joints (child_Joints'Last).Id));
               end if;
            end if;
         end guessed_bone_Length;


      begin
         if bone_Details.contains (which_Joint)
         then
            the_bone_Details := bone_Details.Element (which_Joint);

            if the_bone_Details.Length = Unspecified
            then   bone_Length := guessed_bone_Length;
            else   bone_Length := the_bone_Details.Length;
            end if;
         else
            bone_Length := guessed_bone_Length;
         end if;

         end_Point         :=   joint_Sites.Element (which_Joint)
                              + (0.0, bone_Length, 0.0) * get_Rotation (Self.joint_bind_Matrix (which_Joint));
         prior_bone_Length := bone_Length;

         Self.joint_Parent.insert (which_Joint, Parent);

         create_Bone (which_Joint,
                      which_Joint,
                      end_Point,
                      (the_bone_Details.width_Factor * bone_Length,
                       bone_Length * 0.90,
                       the_bone_Details.depth_Factor * bone_Length),
                      1.0);

         if Parent /= (+"")
         then
            Self.Sprite (Parent).attach_via_ball_Socket (Self.bone_Sprites (which_Joint),

                                                         pivot_Axis   => x_Rotation_from (0.0),
                                                         pivot_Anchor => joint_Sites.Element (which_Joint),

                                                         pitch_Limits => the_bone_Details.pitch_Limits,
                                                         yaw_Limits   => the_bone_Details.  yaw_Limits,
                                                         roll_Limits  => the_bone_Details. roll_Limits,

                                                         new_Joint    => new_Joint);

            Self.Joints.insert (to_gel_joint_Id (Parent, which_Joint),
                                new_Joint);
         end if;

         for i in child_Joints'Range
         loop
            create_Bone_for (child_Joints (i),           -- Recurse over children.
                             parent => which_Joint);
         end loop;
      end create_Bone_for;


      use collada.Library.Controllers;

      global_transform_Slot : Positive := 1;

   begin
      Self.root_Joint := the_root_Joint;                 -- Remember our root joint.
      Self.Model      := Model.all'unchecked_Access;     -- Remember our model.

      --- Parse Controllers.
      --

      -- Set the bind shape matrix.
      --
      Self.bind_shape_Matrix := Transpose (bind_shape_Matrix_of (the_Document.Libraries.Controllers.Contents (1).Skin));


      -- Set the joint slots.
      --
      declare
         the_Skin        : constant Controllers.Skin   := the_Document.Libraries.Controllers.Contents (1).Skin;
         the_joint_Names : constant collada.Text_array := joint_Names_of (the_Skin);
      begin
         for i in 1 .. Integer (the_joint_Names'Length)
         loop
            Self.program_Parameters.joint_Map_of_slot.insert (the_joint_Names (i),
                                                              i);
         end loop;
      end;

      -- Set the inverse bind matrices for all joints.
      --
      declare
         the_Skin       : constant Controllers.Skin         := the_Document.Libraries.Controllers.Contents (1).Skin;
         the_bind_Poses : constant collada.Matrix_4x4_array := bind_Poses_of (the_Skin);
      begin
         for i in 1 .. Integer (the_bind_Poses'Length)
         loop
            Self.joint_inv_bind_Matrices           .append (Transpose (the_bind_Poses (i)));    -- Transpose corrects for collada column vectors.
            Self.program_Parameters.bone_Transforms.append (Identity_4x4);
         end loop;
      end;


      --- Parse Visual Scene.
      --
      Self.define_global_Transform_for (the_root_Joint,                  -- Determine all joint transforms, recursively.
                                        Slot => global_transform_Slot);
      set_Site_for    (the_root_Joint);
      create_Bone_for (the_root_Joint, Parent => +"");                   -- Create all other bones, recursively.


      --- Parse the Collada animations file.
      --
      declare
         use collada.Library.Animations;
         the_Animations : constant access Animation_array := the_Document.Libraries.Animations.Contents;

      begin
         if the_Animations /= null
         then
            for Each in the_Animations'Range
            loop
               declare
                  the_Animation : constant animations.Animation := the_Animations (Each);
                  the_Inputs    : access   collada.float_Array  := Inputs_of (the_Animation);


                  procedure common_setup (Channel     : in channel_Id;
                                          scene_Joint : in scene_Joint_Id;
                                          Sid         : in String)
                  is
                     default_scene_Joint : rig.scene_Joint;
                     default_Channel     : animation_Channel;
                  begin
                     Self.Channels.insert (Channel, default_Channel);

                     Self.Channels (Channel).Target       := Self.scene_Joints (scene_Joint).Node.fetch_Transform (Sid);
                     Self.Channels (Channel).target_Joint := scene_Joint;

                     Self.Channels (Channel).Times        := Inputs_of  (the_Animation);
                     Self.Channels (Channel).Values       := Outputs_of (the_Animation);
                  end common_setup;


                  procedure setup_Rotation (Channel     : in channel_Id;
                                            scene_Joint : in scene_Joint_Id;
                                            Sid         : in String)
                  is
                  begin
                     common_setup (Channel, scene_Joint, Sid);

                     -- For angle interpolation during 'rotation' animation.
                     --
                     Self.Channels (Channel).initial_Angle := Self.Channels (Channel).Values (1);
                     Self.Channels (Channel).current_Angle := Self.Channels (Channel).initial_Angle;
                  end setup_Rotation;
                  pragma Unreferenced (setup_Rotation);


                  procedure setup_Location (Channel     : in channel_Id;
                                            scene_Joint : in scene_Joint_Id;
                                            Sid         : in String)
                  is
                  begin
                     common_setup (Channel, scene_Joint, Sid);

                     -- For location interpolation during 'translation' animation.
                     --
                     Self.Channels (Channel).current_Site  := (Self.Channels (Channel).Values (1),
                                                               Self.Channels (Channel).Values (2),
                                                               Self.Channels (Channel).Values (3));
                     Self.Channels (Channel).initial_Site  := Self.Channels (Channel).current_Site;
                  end setup_Location;
                  pragma Unreferenced (setup_Location);


                  procedure setup_Location_x (Channel     : in channel_Id;
                                              scene_Joint : in scene_Joint_Id;
                                              Sid         : in String)
                  is
                  begin
                     common_setup (Channel, scene_Joint, Sid);

                     -- For matrix interpolation during 'full_transform' animation.
                     --
                     Self.Channels (Channel).Transforms := new Transforms (1 .. Self.Channels (Channel).Values'Length);

                     for i in Self.Channels (Channel).Transforms'Range
                     loop
                        declare
                           the_X_Value : constant Real := Self.Channels (Channel).Values (i);
                        begin
                           Self.Channels (Channel).Transforms (i) := (Rotation    => to_Quaternion (Identity_3x3),
                                                                      Translation => (the_X_Value, 0.0, 0.0));
                        end;
                     end loop;

                     Self.Channels (Channel).initial_Transform := Self.Channels (Channel).Transforms (1);
                     Self.Channels (Channel).current_Transform := Self.Channels (Channel).initial_Transform;

                     Self.Channels (Channel).current_Site      := Self.Channels (Channel).initial_Transform.Translation;
                     Self.Channels (Channel).initial_Site      := Self.Channels (Channel).current_Site;
                  end setup_Location_x;


                  procedure setup_Location_y (Channel     : in channel_Id;
                                              scene_Joint : in scene_Joint_Id;
                                              Sid         : in String)
                  is
                  begin
                     common_setup (Channel, scene_Joint, Sid);

                     -- For matrix interpolation during 'full_transform' animation.
                     --
                     Self.Channels (Channel).Transforms := new Transforms (1 .. Self.Channels (Channel).Values'Length);

                     for i in Self.Channels (Channel).Transforms'Range
                     loop
                        declare
                           the_Y_Value : constant Real := Self.Channels (Channel).Values (i);
                        begin
                           Self.Channels (Channel).Transforms (i) := (rotation    => to_Quaternion (Identity_3x3),
                                                                      translation => (0.0, the_Y_Value, 0.0));
                        end;
                     end loop;

                     Self.Channels (Channel).initial_Transform := Self.Channels (Channel).Transforms (1);
                     Self.Channels (Channel).current_Transform := Self.Channels (Channel).initial_Transform;

                     Self.Channels (Channel).current_Site      := Self.Channels (Channel).initial_Transform.Translation;
                     Self.Channels (Channel).initial_Site      := Self.Channels (Channel).current_Site;
                  end setup_Location_y;


                  procedure setup_Location_z (Channel     : in channel_Id;
                                              scene_Joint : in scene_Joint_Id;
                                              Sid         : in String)
                  is
                  begin
                     common_setup (Channel, scene_Joint, Sid);

                     -- For matrix interpolation during 'full_transform' animation.
                     --
                     Self.Channels (Channel).Transforms := new Transforms (1 .. Self.Channels (Channel).Values'Length);

                     for i in Self.Channels (Channel).Transforms'Range
                     loop
                        declare
                           the_Z_Value : constant Real := Self.Channels (Channel).Values (i);
                        begin
                           Self.Channels (Channel).Transforms (i) := (rotation    => to_Quaternion (Identity_3x3),
                                                                      translation => (0.0, 0.0, the_Z_Value));
                        end;
                     end loop;

                     Self.Channels (Channel).initial_Transform := Self.Channels (Channel).Transforms (1);
                     Self.Channels (Channel).current_Transform := Self.Channels (Channel).initial_Transform;

                     Self.Channels (Channel).current_Site      := Self.Channels (Channel).initial_Transform.Translation;
                     Self.Channels (Channel).initial_Site      := Self.Channels (Channel).current_Site;
                  end setup_Location_z;


                  procedure setup_full_Transform (Channel     : in channel_Id;
                                                  scene_Joint : in scene_Joint_Id;
                                                  Sid         : in String)
                  is
                  begin
                     common_setup (Channel, scene_Joint, Sid);

                     -- For matrix interpolation during 'full_transform' animation.
                     --
                     Self.Channels (Channel).Transforms := new Transforms (1 .. Collada.matrix_Count (Self.Channels (Channel).Values.all));

                     for i in Self.Channels (Channel).Transforms'Range
                     loop
                        declare
                           the_Matrix : constant math.Matrix_4x4 := Transpose (Collada.get_Matrix (Self.Channels (Channel).Values.all,
                                                                               which => i));
                        begin
                           Self.Channels (Channel).Transforms (i) := (Rotation    => to_Quaternion (get_Rotation    (the_Matrix)),
                                                                      Translation =>                get_Translation (the_Matrix));
                        end;
                     end loop;

                     Self.Channels (Channel).initial_Transform := Self.Channels (Channel).Transforms (1);
                     Self.Channels (Channel).current_Transform := Self.Channels (Channel).initial_Transform;

                     Self.Channels (Channel).current_Site      := Self.Channels (Channel).initial_Transform.Translation;
                     Self.Channels (Channel).initial_Site      := Self.Channels (Channel).current_Site;
                  end setup_full_Transform;


                  function Index (Source  : in unbounded_String;
                                  Pattern : in String;
                                  Going   : in Direction              := Forward;
                                  Mapping : in Maps.character_Mapping := ada.Strings.Maps.Identity) return Natural
                    renames ada.Strings.unbounded.Index;

               begin
                  if    Index (the_Animation.Channel.Target, "hips/transform")    /= 0 then
                     setup_full_Transform (+"hips",         +"hips",         "transform");

                  elsif Index (the_Animation.Channel.Target, "thigh_L/transform") /= 0 then
                     setup_full_Transform (+"thigh_L",      +"thigh_L",      "transform");

                  elsif Index (the_Animation.Channel.Target, "shin_L/transform")  /= 0 then
                     setup_full_Transform (+"shin_L",       +"shin_L",       "transform");

                  elsif Index (the_Animation.Channel.Target, "foot_L/transform")  /= 0 then
                     setup_full_Transform (+"foot_L",       +"foot_L",       "transform");

                  elsif Index (the_Animation.Channel.Target, "toe_L/transform")   /= 0 then
                     setup_full_Transform (+"toe_L",        +"toe_L",        "transform");

                  elsif Index (the_Animation.Channel.Target, "thigh_R/transform") /= 0 then
                     setup_full_Transform (+"thigh_R",      +"thigh_R",      "transform");

                  elsif Index (the_Animation.Channel.Target, "shin_R/transform")  /= 0 then
                     setup_full_Transform (+"shin_R",       +"shin_R",       "transform");

                  elsif Index (the_Animation.Channel.Target, "foot_R/transform")  /= 0 then
                     setup_full_Transform (+"foot_R",       +"foot_R",       "transform");

                  elsif Index (the_Animation.Channel.Target, "toe_R/transform")   /= 0 then
                     setup_full_Transform (+"toe_R",        +"toe_R",        "transform");

                  elsif Index (the_Animation.Channel.Target, "spine/transform")   /= 0 then
                     setup_full_Transform (+"spine",        +"spine",        "transform");

                  elsif Index (the_Animation.Channel.Target, "chest/transform")   /= 0 then
                     setup_full_Transform (+"chest",        +"chest",        "transform");


                  elsif Index (the_Animation.Channel.Target, "clavicle_R/transform")   /= 0 then
                     setup_full_Transform (+"clavicle_R",   +"clavicle_R",   "transform");

                  elsif Index (the_Animation.Channel.Target, "upper_arm_R/transform")  /= 0 then
                     setup_full_Transform (+"upper_arm_R",  +"upper_arm_R",  "transform");

                  elsif Index (the_Animation.Channel.Target, "forearm_R/transform")    /= 0 then
                     setup_full_Transform (+"forearm_R",    +"forearm_R",    "transform");

                  elsif Index (the_Animation.Channel.Target, "hand_R/transform")       /= 0 then
                     setup_full_Transform (+"hand_R",       +"hand_R",       "transform");

                  elsif Index (the_Animation.Channel.Target, "thumb_02_R/transform")   /= 0 then
                     setup_full_Transform (+"thumb_02_R",   +"thumb_02_R",   "transform");

                  elsif Index (the_Animation.Channel.Target, "thumb_03_R/transform")   /= 0 then
                     setup_full_Transform (+"thumb_03_R",   +"thumb_03_R",   "transform");

                  elsif Index (the_Animation.Channel.Target, "f_ring_01_R/transform")  /= 0 then
                     setup_full_Transform (+"f_ring_01_R",  +"f_ring_01_R",  "transform");

                  elsif Index (the_Animation.Channel.Target, "f_index_01_R/transform") /= 0 then
                     setup_full_Transform (+"f_index_01_R", +"f_index_01_R", "transform");


                  elsif Index (the_Animation.Channel.Target, "clavicle_L/transform")   /= 0 then
                     setup_full_Transform (+"clavicle_L",   +"clavicle_L",   "transform");

                  elsif Index (the_Animation.Channel.Target, "upper_arm_L/transform")  /= 0 then
                     setup_full_Transform (+"upper_arm_L",  +"upper_arm_L",  "transform");

                  elsif Index (the_Animation.Channel.Target, "forearm_L/transform")    /= 0 then
                     setup_full_Transform (+"forearm_L",    +"forearm_L",    "transform");

                  elsif Index (the_Animation.Channel.Target, "hand_L/transform")       /= 0 then
                     setup_full_Transform (+"hand_L",       +"hand_L",       "transform");

                  elsif Index (the_Animation.Channel.Target, "thumb_02_L/transform")   /= 0 then
                     setup_full_Transform (+"thumb_02_L",   +"thumb_02_L",   "transform");

                  elsif Index (the_Animation.Channel.Target, "thumb_03_L/transform")   /= 0 then
                     setup_full_Transform (+"thumb_03_L",   +"thumb_03_L",   "transform");

                  elsif Index (the_Animation.Channel.Target, "f_ring_01_L/transform")  /= 0 then
                     setup_full_Transform (+"f_ring_01_L",  +"f_ring_01_L",  "transform");

                  elsif Index (the_Animation.Channel.Target, "f_index_01_L/transform") /= 0 then
                     setup_full_Transform (+"f_index_01_L", +"f_index_01_L", "transform");


                  elsif Index (the_Animation.Channel.Target, "neck/transform")         /= 0 then
                     setup_full_Transform (+"neck",         +"neck",         "transform");

                  elsif Index (the_Animation.Channel.Target, "head/transform")         /= 0 then
                     setup_full_Transform (+"head",         +"head",         "transform");

                  elsif Index (the_Animation.Channel.Target, "jaw/transform")          /= 0 then
                     setup_full_Transform (+"jaw",          +"jaw",          "transform");

                  elsif Index (the_Animation.Channel.Target, "eye_R/transform")        /= 0 then
                     setup_full_Transform (+"eye_R",        +"eye_R",        "transform");

                  elsif Index (the_Animation.Channel.Target, "eye_L/transform")        /= 0 then
                     setup_full_Transform (+"eye_L",        +"eye_L",        "transform");

                  elsif Index (the_Animation.Channel.Target, "stride_bone/location.X") /= 0 then
--                       setup_Location_x (+"stride_bone_x", +"stride_bone", "x");
                     setup_Location_x (+"stride_bone_x",    +"human",     "x");

                  elsif Index (the_Animation.Channel.Target, "stride_bone/location.Y") /= 0 then
--                       setup_Location_y (+"stride_bone_y", +"stride_bone", "y");
                     setup_Location_y (+"stride_bone_y",    +"human",     "y");

                  elsif Index (the_Animation.Channel.Target, "stride_bone/location.Z") /= 0 then
--                       setup_Location_z (+"stride_bone_z", +"stride_bone", "z");
                     setup_Location_z (+"stride_bone_z",    +"human",     "z");

                  else
                     raise constraint_Error with +the_Animation.Channel.Target & " not handled";
                  end if;
               end;
            end loop;
         end if;

      end;

   end define;



   procedure enable_Graphics (Self : in out Item)
   is
   begin
      Self            .program_Parameters.Program_is (opengl.Program.view (opengl.Geometry.lit_colored_textured_skinned.Program));
      Self.skin_Sprite.program_Parameters_are (Self.program_Parameters'unchecked_Access);
   end enable_Graphics;



   function Joints (Self : in Item) return gel_joint_id_Map_of_gel_Joint
   is
   begin
      return Self.Joints;
   end Joints;



   function joint_inv_bind_Matrices (Self : in Item'Class) return inverse_bind_matrix_Vector
   is
   begin
      return Self.joint_inv_bind_Matrices;
   end joint_inv_bind_Matrices;



   procedure joint_inv_bind_Matrices_are (Self : in out Item'Class;   Now : in inverse_bind_matrix_Vector)
   is
   begin
      Self.joint_inv_bind_Matrices := Now;
   end joint_inv_bind_Matrices_are;



   function joint_site_Offets (Self : in Item'Class) return joint_Id_Map_of_bone_site_offset
   is
   begin
      return Self.phys_joint_site_Offets;
   end joint_site_Offets;


   --------------
   --- Attributes
   --

   procedure Site_is (Self :in out Item;   Now : in Vector_3)
   is
   begin
      Self.base_Sprite.move (to_Site => Now);
      Self.overall_Site := Now;
   end Site_is;



   procedure Spin_is (Self :in out Item;   Now : in Matrix_3x3)
   is
   begin
      Self.base_Sprite.rotate (to_Spin => Now);
   end Spin_is;



   function Sprite (Self : in Item'Class;   Bone : in bone_Id) return gel.Sprite.view
   is
   begin
      return Self.bone_Sprites (Bone);
   end Sprite;



   function base_Sprite (Self : in Item'Class) return gel.Sprite.view
   is
   begin
      return Self.bone_Sprites.Element (Self.root_Joint.Name);
   end base_Sprite;



   function skin_Sprite (Self : in Item'Class) return gel.Sprite.view
   is
   begin
      return Self.skin_Sprite;
   end skin_Sprite;



   function bone_Sprites (Self : in Item) return bone_id_Map_of_sprite
   is
   begin
      return Self.bone_Sprites;
   end bone_Sprites;



   procedure set_GL_program_Parameters (Self : in out Item'Class;   for_Bone : in controller_joint_Id;
                                                                    To       : in Matrix_4x4)
   is
      use gel.Conversions;
      bone_Slot : constant Positive := Self.program_Parameters.joint_Map_of_slot.Element (for_Bone);
   begin
      Self.program_Parameters.bone_Transforms.replace_Element (bone_Slot,
                                                               to_GL (To));
   end set_GL_program_Parameters;



   procedure animation_Transforms_are (Self : in out Item'Class;   Now : in bone_id_Map_of_transform)
   is
   begin
      Self.animation_Transforms := Now;
   end animation_Transforms_are;



   procedure motion_Mode_is (Self : in out Item;   Now : in motion_Mode)
   is
   begin
      Self.Mode := Now;
   end motion_Mode_is;


   --------------
   --- Operations
   --

   procedure evolve (Self : in out Item'Class;   world_Age : in Duration)
   is

      function get_root_Transform return Matrix_4x4
      is
      begin
         case Self.Mode
         is
            when Dynamics =>
               return Self.base_Sprite.Transform;

            when Animation =>
               declare
                  the_Transform : Matrix_4x4;
               begin
                  set_Rotation    (the_Transform,  x_Rotation_from (to_Radians (0.0)));
                  set_Translation (the_Transform, -get_Translation (Inverse (Self.joint_pose_Transforms (Self.root_Joint.Name))));

                  return the_Transform;
               end;
         end case;
      end get_root_Transform;


      root_Transform     : constant Matrix_4x4 := get_root_Transform;
      inv_root_Transform : constant Matrix_4x4 := Inverse (root_Transform);


      function joint_Transform_for (the_collada_Joint : in controller_joint_Id) return Matrix_4x4
      is
      begin
         case Self.Mode
         is
            when Dynamics =>
               declare
                  the_bone_Transform    : constant Matrix_4x4 := Self.Sprite (the_collada_Joint).Transform;
                  the_joint_site_Offset :          Vector_3   := Self.phys_joint_site_Offets (the_collada_Joint);
                  the_joint_Transform   :          Matrix_4x4;
               begin
                  the_joint_site_Offset :=   the_joint_site_Offset
                                           * get_Rotation (Self.joint_inv_bind_Matrix (the_collada_Joint))
                                           * get_Rotation (the_bone_Transform);

                  set_Translation (the_joint_Transform,  get_Translation (the_bone_Transform) + the_joint_site_Offset);
                  set_Rotation    (the_joint_Transform,  get_Rotation    (the_bone_Transform));

                  Self.joint_Sprites (the_collada_Joint).all.Site_is (get_Translation (the_joint_Transform));

                  return the_joint_Transform;
               end;

            when Animation =>
               Self.joint_Sprites (the_collada_Joint).all.Site_is (         get_Translation (Self.scene_Joints (the_collada_Joint).Transform));
               Self.joint_Sprites (the_collada_Joint).all.Spin_is (Inverse (get_Rotation    (Self.scene_Joints (the_collada_Joint).Transform)));

               return Self.scene_Joints (the_collada_Joint).Transform;
            end case;
      end joint_Transform_for;


      procedure set_Transform_for (the_Bone : in controller_joint_Id)
      is
         the_Slot : constant Positive := Self.program_Parameters.joint_Map_of_slot (the_Bone);
      begin
         Self.set_GL_program_Parameters (for_Bone => the_Bone,
                                         To       =>   Self.bind_shape_Matrix
                                                     * Self.joint_inv_bind_Matrices.Element (the_Slot)
                                                     * joint_Transform_for (the_Bone)
                                                     * inv_root_Transform);
      end set_Transform_for;


      procedure set_proxy_Transform_for (the_Bone : in controller_joint_Id;   the_Proxy : in controller_joint_Id)
      is
         the_Slot : constant Positive := Self.program_Parameters.joint_Map_of_slot (the_Proxy);
      begin
         Self.set_GL_program_Parameters (for_bone => the_Bone,
                                         to       =>   Self.bind_shape_Matrix
                                                     * Self.joint_inv_bind_Matrices .Element (the_Slot)
                                                     * joint_Transform_for (the_Proxy)
                                                     * inv_root_Transform);
      end set_proxy_Transform_for;
      pragma Unreferenced (set_proxy_Transform_for);


      use joint_Id_Maps_of_bone_site_offset;

      Cursor : joint_Id_Maps_of_bone_site_offset.Cursor := Self.phys_joint_site_Offets.First;

   begin
      if Self.Mode = Animation
      then
         Self.animate (world_Age);
      end if;

      while has_Element (Cursor)
      loop
         if Self.program_Parameters.joint_Map_of_slot.Contains (Key (Cursor))
         then
            set_Transform_for (Key (Cursor));     -- Updates gl skin program params.
         end if;

         next (Cursor);
      end loop;
   end evolve;



   procedure assume_Pose (Self : in out Item)
   is
      use bone_id_Maps_of_transform;

      the_Bone : gel.Sprite.view;
      Cursor   : bone_id_Maps_of_transform.Cursor := Self.bone_pose_Transforms.First;

   begin
      while has_Element (Cursor)
      loop
         the_Bone := Self.bone_Sprites (Key (Cursor));
         the_Bone.Transform_is (Element (Cursor));

         next (Cursor);
      end loop;
   end assume_Pose;



   function Parent_of (Self : in Item;   the_Bone : in bone_Id) return bone_Id
   is
   begin
      if Self.joint_Parent.Contains (the_Bone)
      then
         return Self.joint_Parent.Element (the_Bone);
      else
         return null_Id;
      end if;
   end Parent_of;



   function joint_site_Offet (Self : in Item;   for_Bone : in bone_Id) return math.Vector_3
   is
   begin
      return Self.phys_joint_site_Offets.Element (for_Bone);
   end joint_site_Offet;



   function joint_inv_bind_Matrix (Self : in Item;   for_Bone : in bone_Id) return math.Matrix_4x4
   is
      use ada.Strings.unbounded;
   begin
      if for_Bone = Self.root_Joint.Name
      then
         return math.Identity_4x4;
      else
         return Self.joint_inv_bind_Matrices.Element (Self.program_Parameters.joint_Map_of_slot.Element (for_Bone));
      end if;
   end joint_inv_bind_Matrix;



   function joint_bind_Matrix (Self : in Item;   for_Bone : in bone_Id) return Matrix_4x4
   is
   begin
      return Inverse (Self.joint_inv_bind_Matrix (for_Bone));
   end joint_bind_Matrix;


   -------------
   --- Animation
   --

   procedure animate (Self : in out Item;   world_Age : in Duration)
   is
      Now     : Duration;
      Elapsed : Duration;


      procedure update_rotation_Animation (for_Channel : in channel_Id;
                                           for_Joint   : in scene_joint_Id;
                                           for_Axis    : in axis_Kind)
      is
         the_Channel : animation_Channel renames Self.Channels (for_Channel);
         Cursor      : math.Index        renames the_Channel.Cursor;

         function Reduced (Angle : in Real) return Real     -- TODO: Use Degrees type.
         is
         begin
            if    Angle >  180.0 then   return -360.0 + Angle;
            elsif Angle < -180.0 then   return  360.0 + Angle;
            else                        return  Angle;
            end if;
         end Reduced;

      begin
         if Cursor < the_Channel.Times'Last
         then
            if        Cursor = 0
              or else Elapsed > Duration (the_Channel.Times (Cursor))
            then
               Cursor := Cursor + 1;

               if Cursor = 1
               then
                  if the_Channel.Times (Cursor) = 0.0
                  then
                     the_Channel.interp_Delta :=   Reduced (the_Channel.Values (Cursor) - the_Channel.current_Angle);
                  else
                     the_Channel.interp_Delta :=   Reduced (the_Channel.Values (Cursor) - the_Channel.current_Angle)
                                                 / (the_Channel.Times (Cursor));
                  end if;

               else
                  the_Channel.interp_Delta :=      Reduced (the_Channel.Values (Cursor) - the_Channel.current_Angle)
                                                 / (the_Channel.Times  (Cursor) - the_Channel.Times (Cursor - 1));
               end if;

               the_Channel.interp_Delta := the_Channel.interp_Delta / 60.0;  -- 60.0 is frames/sec.
            end if;
         end if;

         if Elapsed < Duration (the_Channel.Times (the_Channel.Times'Last))
         then
            the_Channel.current_Angle := Reduced (  the_Channel.current_Angle
                                                  + the_Channel.interp_Delta);

            Self.set_rotation_Angle (for_Joint,
                                     for_Axis,
                                     To => to_Radians (Degrees (the_Channel.current_Angle)));
         end if;
      end update_rotation_Animation;
      pragma Unreferenced (update_rotation_Animation);



      procedure update_location_Animation (for_Channel : in channel_Id;
                                           for_Joint   : in scene_joint_Id)
      is
         pragma Unreferenced (for_Joint);
         the_Channel :          animation_Channel renames Self.Channels (for_Channel);
         Cursor      :          Index             renames the_Channel.Cursor;
         Elapsed     : constant Duration          :=      Now - Self.start_Time;

         function site_X return Real is begin   return the_Channel.Values ((Cursor - 1) * 3 + 1);   end site_X;
         function site_Y return Real is begin   return the_Channel.Values ((Cursor - 1) * 3 + 2);   end site_Y;
         function site_Z return Real is begin   return the_Channel.Values ((Cursor - 1) * 3 + 3);   end site_Z;

      begin
         if Cursor < the_Channel.Times'Last
         then
            if        Cursor = 0
              or else Elapsed > Duration (the_Channel.Times (Cursor))
            then
               Cursor := Cursor + 1;

               if Cursor = 1
               then
                  if the_Channel.Times (Cursor) = 0.0
                  then
                     the_Channel.site_interp_Delta (1) := site_X - the_Channel.current_Site (1);
                     the_Channel.site_interp_Delta (2) := site_Y - the_Channel.current_Site (2);
                     the_Channel.site_interp_Delta (3) := site_Z - the_Channel.current_Site (3);
                  else
                     the_Channel.site_interp_Delta (1) :=   (site_X - the_Channel.current_Site (1))
                                                          / (the_Channel.Times (Cursor));
                     the_Channel.site_interp_Delta (2) :=   (site_Y - the_Channel.current_Site (2))
                                                          / (the_Channel.Times (Cursor));
                     the_Channel.site_interp_Delta (3) :=   (site_Z - the_Channel.current_Site (3))
                                                          / (the_Channel.Times (Cursor));
                  end if;

               else
                  the_Channel.site_interp_Delta (1) :=   (site_X - the_Channel.current_Site (1))
                                                       / (the_Channel.Times (Cursor) - the_Channel.Times (Cursor - 1));
                  the_Channel.site_interp_Delta (2) :=   (site_Y - the_Channel.current_Site (2))
                                                       / (the_Channel.Times (Cursor) - the_Channel.Times (Cursor - 1));
                  the_Channel.site_interp_Delta (3) :=   (site_Z - the_Channel.current_Site (3))
                                                       / (the_Channel.Times (Cursor) - the_Channel.Times (Cursor - 1));
               end if;

               the_Channel.site_interp_Delta (1) := the_Channel.site_interp_Delta (1) / 60.0;  -- 60.0 is frames/sec.
               the_Channel.site_interp_Delta (2) := the_Channel.site_interp_Delta (2) / 60.0;  --
               the_Channel.site_interp_Delta (3) := the_Channel.site_interp_Delta (3) / 60.0;  --
            end if;

            Self.set_Location (the_Channel.target_Joint,  to => the_Channel.current_Site);

            the_Channel.current_Site (1) := the_Channel.current_Site (1) + the_Channel.site_interp_Delta (1);
            the_Channel.current_Site (2) := the_Channel.current_Site (2) + the_Channel.site_interp_Delta (2);
            the_Channel.current_Site (3) := the_Channel.current_Site (3) + the_Channel.site_interp_Delta (3);

         end if;
      end update_location_Animation;
      pragma Unreferenced (update_location_Animation);



      procedure update_location_X_Animation (for_Channel : in channel_Id;
                                             for_Joint   : in scene_joint_Id)
      is
         pragma Unreferenced (for_Joint);
         the_Channel :          animation_Channel renames Self.Channels (for_Channel);
         Cursor      :          Index             renames the_Channel.Cursor;
         Elapsed     : constant Duration          :=      Now - Self.start_Time;

         function site_X return Real is begin   return the_Channel.Values (Cursor);   end site_X;

      begin
         if Cursor < the_Channel.Times'Last
         then
            if        Cursor = 0
              or else Elapsed > Duration (the_Channel.Times (Cursor))
            then
               Cursor := Cursor + 1;

               if Cursor = 1
               then
                  if the_Channel.Times (Cursor) = 0.0
                  then
                     the_Channel.site_interp_Delta (1) := site_X - the_Channel.current_Site (1);
                  else
                     the_Channel.site_interp_Delta (1) :=   (site_X - the_Channel.current_Site (1))
                                                          / (the_Channel.Times (Cursor));
                  end if;

               else
                  the_Channel.site_interp_Delta (1) :=   (site_X - the_Channel.current_Site (1))
                                                       / (the_Channel.Times (Cursor) - the_Channel.Times (Cursor - 1));
               end if;

               the_Channel.site_interp_Delta (1) := the_Channel.site_interp_Delta (1) / 60.0;  -- 60.0 is frames/sec.
            end if;

            Self.set_Location_x (the_Channel.target_Joint, To => the_Channel.current_Site (1));

            the_Channel.current_Site (1) := the_Channel.current_Site (1) + the_Channel.site_interp_Delta (1);
         end if;
      end update_location_X_Animation;



      procedure update_location_Y_Animation (for_Channel : in channel_Id;
                                             for_Joint   : in scene_joint_Id)
      is
         pragma Unreferenced (for_Joint);
         the_Channel :          animation_Channel renames Self.Channels (for_Channel);
         Cursor      :          Index             renames the_Channel.Cursor;
         Elapsed     : constant Duration          :=      Now - Self.start_Time;

         function site_Y return math.Real is begin   return the_Channel.Values (Cursor);   end site_Y;

      begin
         if Cursor < the_Channel.Times'Last
         then
            if        Cursor = 0
              or else Elapsed > Duration (the_Channel.Times (Cursor))
            then
               Cursor := Cursor + 1;

               if Cursor = 1
               then
                  if the_Channel.Times  (Cursor) = 0.0
                  then
                     the_Channel.site_interp_Delta (2) := site_Y - the_Channel.current_Site (2);
                  else
                     the_Channel.site_interp_Delta (2) :=   (site_Y - the_Channel.current_Site (2))
                                                          / (the_Channel.Times  (Cursor));
                  end if;
               else
                  the_Channel.site_interp_Delta (2) :=   (site_Y - the_Channel.current_Site (2))
                                                       / (the_Channel.Times  (Cursor) - the_Channel.Times (Cursor - 1));
               end if;

               the_Channel.site_interp_Delta (2) := the_Channel.site_interp_Delta (2) / 60.0;  -- 60.0 is frames/sec
            end if;

            Self.set_Location_y (the_Channel.target_Joint, To => the_Channel.current_Site (2));

            the_Channel.current_Site (2) := the_Channel.current_Site (2) + the_Channel.site_interp_Delta (2);
         end if;
      end update_location_Y_Animation;



      procedure update_location_Z_Animation (for_Channel : in channel_Id;
                                             for_Joint   : in scene_joint_Id)
      is
         pragma Unreferenced (for_Joint);
         the_Channel :          animation_Channel renames Self.Channels (for_Channel);
         Cursor      :          math.Index        renames the_Channel.Cursor;
         Elapsed     : constant Duration          :=      Now - Self.start_Time;

         function site_Z return math.Real is begin   return the_Channel.Values (Cursor);   end site_Z;

      begin
         if Cursor < the_Channel.Times'Last
         then
            if        Cursor = 0
              or else Elapsed > Duration (the_Channel.Times (Cursor))
            then
               Cursor := Cursor + 1;

               if Cursor = 1
               then
                  if the_Channel.Times  (Cursor) = 0.0
                  then
                     the_Channel.site_interp_Delta (3) := site_Z - the_Channel.current_Site (3);
                  else
                     the_Channel.site_interp_Delta (3) :=   (site_Z - the_Channel.current_Site (3))
                                                          / (the_Channel.Times  (Cursor));
                  end if;

               else
                  the_Channel.site_interp_Delta (3) :=   (site_Z - the_Channel.current_Site (3))
                                                       / (the_Channel.Times  (Cursor) - the_Channel.Times (Cursor - 1));
               end if;

               the_Channel.site_interp_Delta (3) := the_Channel.site_interp_Delta (3) / 60.0;  -- 60.0 is frames/sec
            end if;

            Self.set_Location_z (the_Channel.target_Joint, To => the_Channel.current_Site (3));

            the_Channel.current_Site (3) := the_Channel.current_Site (3) + the_Channel.site_interp_Delta (3);
         end if;
      end update_location_Z_Animation;



      procedure update_full_transform_Animation (for_Channel : in channel_Id;
                                                 for_Joint   : in scene_joint_Id)
      is
         pragma Unreferenced (for_Joint);
         the_Channel    : animation_Channel renames Self.Channels (for_Channel);
         Cursor         : Index             renames the_Channel.Cursor;
         Cursor_updated : Boolean           :=      False;
         new_Transform  : Matrix_4x4        :=      Identity_4x4;

      begin
         if Cursor = the_Channel.Times'Last
         then
            Cursor          := 0;
            Self.start_Time := Now;
         end if;

         -- Rotation
         --
         declare
            Initial : Transform;
         begin
            if Cursor < the_Channel.Times'Last
            then
               if        Cursor = 0
                 or else Elapsed > Duration (the_Channel.Times (Cursor))
               then
                  Cursor         := Cursor + 1;
                  Cursor_updated := True;

                  if Cursor = 1
                  then
                     Initial := the_Channel.current_Transform;

                     if the_Channel.Times (Cursor) = 0.0
                     then
                        the_Channel.Transform_interp_Delta := 1.0 / 60.0;
                     else
                        the_Channel.Transform_interp_Delta := the_Channel.Times (Cursor);
                     end if;

                  else
                     Initial := the_Channel.Transforms (Cursor - 1);
                     the_Channel.Transform_interp_Delta := the_Channel.Times (Cursor) - the_Channel.Times (Cursor - 1);
                  end if;

                  the_Channel.current_Transform      := the_Channel.Transforms (Cursor);
                  the_Channel.Transform_interp_Delta := 1.0 / (the_Channel.Transform_interp_Delta * 60.0);  -- 60.0 is frames/sec.
                  the_Channel.slerp_Time             := 0.0;

               else
                  if Cursor > 1
                  then   Initial := the_Channel.Transforms (Cursor - 1);
                  else   Initial := the_Channel.Transforms (Cursor);
                  end if;
               end if;

            else
               Initial := the_Channel.Transforms (1);
            end if;

            if Elapsed < Duration (the_Channel.Times (the_Channel.Times'Last))
            then
               set_Rotation (new_Transform, to_Matrix (Interpolated (Initial.Rotation,
                                                                     the_Channel.current_Transform.Rotation,
                                                                     Percent => to_Percentage (the_Channel.slerp_Time))));
               the_Channel.slerp_Time :=   the_Channel.slerp_Time
                                         + the_Channel.Transform_interp_Delta;
            end if;
         end;


         -- Location
         --
         declare
            desired_Site : constant Vector_3 := the_Channel.Transforms (Cursor).Translation;
         begin
            if Cursor < the_Channel.Times'Last
            then
               if Cursor_updated
               then
                  if Cursor = 1
                  then
                     if the_Channel.Times (Cursor) = 0.0
                     then
                        the_Channel.site_interp_Delta := desired_Site - the_Channel.current_Site;
                     else
                        the_Channel.site_interp_Delta :=   (desired_Site - the_Channel.current_Site)
                                                         / (the_Channel.Times (Cursor));
                     end if;

                  else
                     the_Channel.site_interp_Delta :=   (desired_Site - the_Channel.current_Site)
                                                      / (the_Channel.Times  (Cursor) - the_Channel.Times (Cursor - 1));
                  end if;

                  the_Channel.site_interp_Delta := the_Channel.site_interp_Delta / 60.0;  -- 60.0 is frames/sec.
               end if;

               the_Channel.current_Site := the_Channel.current_Site + the_Channel.site_interp_Delta;

               set_Translation (new_Transform, To => the_Channel.current_Site);
            end if;
         end;


         -- Scale
         --

         -- (TODO)


         -- Store the new transform.
         --
         Self.set_Transform (the_Channel.target_Joint,
                             To => Transpose (new_Transform));    -- Transpose to convert to collada column vectors.
      end update_full_transform_Animation;


   begin
      Now := world_Age;

      if Self.start_Time = 0.0 then
         Self.start_Time := Now;
      end if;

      Elapsed := Now - Self.start_Time;

      declare
         use channel_id_Maps_of_animation_Channel,
             ada.Strings.Unbounded;

         Cursor : channel_id_Maps_of_animation_Channel.Cursor := Self.Channels.First;
      begin
         while has_Element (Cursor)
         loop
            if    Key (Cursor) = (+"stride_bone_x")
            then
               update_location_X_Animation (Key (Cursor),
                                            Key (Cursor));

            elsif Key (Cursor) = (+"stride_bone_y")
            then
               update_location_Y_Animation (Key (Cursor),
                                            Key (Cursor));

            elsif Key (Cursor) = (+"stride_bone_z")
            then
               update_location_Z_Animation (Key (Cursor),
                                            Key (Cursor));
            else
               update_full_transform_Animation (Key (Cursor),
                                                Key (Cursor));
            end if;

            next (Cursor);
         end loop;
      end;

      Self.update_all_global_Transforms;
   end animate;



   procedure reset_Animation (Self : in out Item)
   is
      use channel_id_Maps_of_animation_Channel;

      Cursor      : channel_id_Maps_of_animation_Channel.Cursor := Self.Channels.First;
      the_Channel : animation_Channel;

   begin
      Self.start_Time := 0.0;

      while has_Element (Cursor)
      loop
         the_Channel := Element (Cursor);

         the_Channel.Cursor        := 0;
         the_Channel.current_Angle := the_Channel.initial_Angle;
         the_Channel.current_Site  := the_Channel.initial_Site;
         the_Channel.interp_Delta  := 0.0;

         Self.Channels.replace_Element (Cursor, the_Channel);

         next (Cursor);
      end loop;
   end reset_Animation;


end gel.Rig;
