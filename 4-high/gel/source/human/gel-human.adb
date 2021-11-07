with
     openGL.Model.any,
--       gel.Model.box.colored,
     openGL.Model.box.lit_colored_textured,
--       gel.cone_twist_Joint,
     gel.Conversions,

     collada.Document,
     collada.Library,
     collada.Library.controllers,
--       collada.Library.visual_scenes,
     collada.Library.animations,

     opengl.Palette,
     opengl.Geometry.lit_textured_skinned,
     opengl.Program .lit_textured_skinned,

     float_math.Algebra.linear.d3,
     ada.Strings.unbounded;

with ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;





package body gel.Human
is

   use math.Algebra.linear.d3;


--       my_Scale : constant := 0.3;
--     my_Scale : constant := 0.5;
--     my_Scale : constant := 0.1;
   my_Scale : constant := 1.0;




   model_Name : access String;


   procedure use_Model (Named : in String)
   is
   begin
      if model_Name /= null then
         raise Program_Error with "'gel.human' model name has already been set";
      end if;

      model_Name := new String' (Named);
   end use_Model;









   --- Utility
   --

   function "+" (From : in ada.strings.unbounded.unbounded_String) return String
     renames ada.strings.unbounded.to_String;



   function to_joint_Id (From : in String) return scene_joint_Id
   is
      Pad : String := From;
   begin
      if From = "" then
         return Armature;
      end if;

      for Each in Pad'Range loop
         if Pad (Each) = '-' then
            Pad (Each) := '_';
         end if;
      end loop;

--        put_Line ("Pad: '" & Pad & "'");

      return scene_joint_Id'Value (Pad);
   end to_joint_Id;



   function to_Math (From : in collada.Matrix_4x4) return math.Matrix_4x4
   is
      use type math.Real;
   begin
      return (1 => (From (1, 1),  From (1, 2),  From (1, 3),  From (1, 4)),
              2 => (From (2, 1),  From (2, 2),  From (2, 3),  From (2, 4)),
              3 => (From (3, 1),  From (3, 2),  From (3, 3),  From (3, 4)),
              4 => (From (4, 1),  From (4, 2),  From (4, 3),  From (4, 4)));
   end to_Math;



   to_scene_joint_Id      : array (controller_joint_Id) of scene_joint_Id;
   to_controller_joint_Id : array (scene_joint_Id     ) of controller_joint_Id;





   --- Forge
   --

   package body Forge
   is

      function new_Human (World         : access gel.World.item'Class;
--                            Space         : in     gel.Sprite.physics_Space_view;
                          Model         : access openGL.Model     .item'class;
                          physics_Model : access standard.physics.Model.item'class;
                          Mass          : in     math.Real           := 0.0;
                          is_Kinematic  : in     Boolean             := False) return Human.view
      is
         Self : constant Human.view := new Human.item;
      begin
         Self.define (World, --Space,
                      Model, physics_Model, Mass, is_Kinematic);

         return Self;
      end new_Human;



      function new_Human (bone_Sprites      : in     human.bone_Sprites;
                          controller_Joints : in     human_types.controller_Joints;
                          Model             : access openGL.Model.item'Class) return Human.view
      is
         the_Human : constant Human.View := new Human.item;
      begin
         the_Human.bone_Sprites      := bone_Sprites;
         the_Human.controller_Joints := controller_Joints;
         the_Human.Model             := Model;

         return the_Human;
      end new_Human;

   end Forge;




   --- skin_program_Parameters
   --

   overriding
   procedure enable (Self : in out skin_program_Parameters)
   is
   begin
      for Each in Self.bone_Transforms'Range loop
         openGL.Program.lit_textured_skinned.view (Self.Program)
           .bone_Transform_is (which => controller_joint_Id'Pos (Each) + 1,
                               now   => Self.bone_Transforms (Each));
      end loop;
   end enable;






   procedure set_global_Transform_for (Self      : in out Item'Class;
                                       the_Joint : in     collada.Library.visual_scenes.Node_view)
   is
      use collada.Library;

      which_Joint         : constant scene_joint_Id       := to_joint_Id (+the_Joint.Name);
      child_Joints        : constant visual_scenes.Nodes  := the_Joint.Children;
   begin
      Self.scene_Joints (which_Joint).Transform := the_Joint.global_Transform;
      Self.scene_Joints (which_Joint).Node      := the_Joint;                       -- tbd: move this to initialisation.

      for Each in child_Joints'Range loop
         set_global_Transform_for (Self, child_Joints (Each));      -- Recurse over children.
      end loop;
   end set_global_Transform_for;





   procedure update_all_global_Transforms (Self : in out Item'Class)
   is
   begin
      set_global_Transform_for (Self, Self.root_Joint);             -- Re-determine all joint transforms, recursively.
   end update_all_global_Transforms;






   procedure set_rotation_Angle (Self : in out Item'Class;     for_Joint : in scene_joint_Id;
                                                               Axis      : in Axis_Kind;
                                                               To        : in math.Real)
   is
   begin
      case Axis is
         when x_Axis =>   Self.set_x_rotation_Angle (for_Joint, To);
         when y_Axis =>   Self.set_y_rotation_Angle (for_Joint, To);
         when z_Axis =>   Self.set_z_rotation_Angle (for_Joint, To);
      end case;
   end set_rotation_Angle;





   procedure set_Location (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                       To        : in math.Vector_3)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_Location (To);
   end set_Location;





   procedure set_x_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Real)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_x_rotation_Angle (To);
   end set_x_rotation_Angle;



   procedure set_y_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Real)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_y_rotation_Angle (To);
   end set_y_rotation_Angle;



   procedure set_z_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Real)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_z_rotation_Angle (To);
   end set_z_rotation_Angle;





   procedure destroy (Self : in out Item)
   is
      use openGL.Model, gel.Sprite;

      the_base_Sprite : gel.Sprite.view := Self.base_Sprite;


      procedure free_Model_for (the_Sprite : in out gel.Sprite.view)
      is
         type Model_view is access all openGL.Model.item'Class;
         procedure deallocate is new ada.unchecked_Deallocation (openGL.Model.item'Class,  Model_view);

         the_Model        : Model_view      := Model_view (the_sprite.graphics_Model);
         the_child_Joints : constant gel.Joint.views := the_Sprite.child_Joints;
         the_Child        : gel.Sprite.view;
      begin
         if the_Sprite /= the_base_Sprite then
            destroy (the_Model.all);
            deallocate (the_Model);
         end if;

         --  do children
         --
         for Each in the_child_Joints'Range loop
            the_Child := the_child_Joints (Each).Sprite_B.all'Access;
            free_Model_for (the_Child);    -- recurse
         end loop;
      end free_Model_for;


   begin
      free_Model_for (the_base_Sprite);
      free           (the_base_Sprite);
   end destroy;





   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.Unchecked_Deallocation (Item'Class, View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;





   --- Human item
   --
   the_global_Document            : collada.Document.item;
   the_global_Document_is_defined : Boolean := False;

   procedure define (Self : in out Item;   World         : access gel.World.item'Class;
                                           Model         : access openGL.Model.item'Class;
                                           physics_Model : access standard.physics.Model.item'Class;
                                           Mass          : in     math.Real                    := 0.0;
                                           is_Kinematic  : in     Boolean                      := False)
   is
      pragma Unreferenced (Mass);

      use collada.Library, collada.Library.visual_scenes,
          math.Algebra.linear.d3,
          ada.Strings.unbounded, ada.Strings;


      type gl_Model_view is access all openGL.Model.any.item;

--        the_Model : constant gl_Model_view := gl_Model_view (Model);


      function the_Document return collada.Document.item
      is
      begin
         if not the_global_Document_is_defined then
            the_global_Document           := collada.Document.to_Document (model_Name.all);   -- tbd: free this at app close.
            the_global_Document_is_defined := True;
         end if;

         return the_global_Document;
      end the_Document;

--        the_root_Joint : visual_scenes.Node_view := the_Document.libraries.visual_Scenes.Contents (1).root_Node;
      the_root_Joint : constant visual_scenes.Node_view
        := the_Document.libraries.visual_Scenes.Contents (1).root_Node.Child (1);


      joint_Sites : array (scene_joint_Id) of math.Vector_3;

      procedure set_Site_for (the_Joint : visual_scenes.Node_view;   parent_Site : in math.Vector_3)
      is
         pragma Unreferenced (parent_Site);
         use Math;

--     collada_Translation : collada.Vector_3     := the_Joint.Translation;
--     the_Site            : math.Vector_3        := parent_Site + math.Vector_3'(math.Real (collada_Translation (1)),
--                                                                                math.Real (collada_Translation (2)),
--                                                                                math.Real (collada_Translation (3)));
         which_Joint         : constant scene_joint_Id      := to_joint_Id (+the_Joint.Name);
         child_Joints        : constant visual_scenes.Nodes := the_Joint.Children;
      begin
--           joint_Sites (which_Joint) := the_Site;
--           joint_Sites (which_Joint) := get_Translation (the_Joint.global_Transform);

         joint_Sites (which_Joint)
           := get_Translation (Inverse (Self.controller_Joints (to_controller_joint_Id (which_Joint))
                                            .inverse_bind_Matrix));

--           joint_Sites (which_Joint) := joint_Sites (which_Joint) * my_Scale;

         for Each in child_Joints'Range loop
            set_Site_for (child_Joints (Each),  parent_site => joint_Sites (which_Joint));      -- do children, recursively
         end loop;
      end set_Site_for;

   begin
      --  Set the inverse bind matrices for all joints.
      --
      declare
         use collada.Library.controllers,  Math;

         the_Skin       : constant controllers.Skin         := the_Document.libraries.controllers.Contents (1).Skin;
         the_bind_Poses : constant collada.Matrix_4x4_array := bind_Poses_of (the_Skin);
      begin
         for Each in Self.controller_Joints'Range loop
            Self.controller_Joints (Each).inverse_bind_Matrix
              := Transpose (the_bind_Poses (controller_joint_Id'Pos (Each) + 1));
            -- transpose to correct for collada col major

            --  Scale the site in the joints inverse bind matrix.
            declare
               the_Site : math.Vector_3 := get_Translation (Self.controller_Joints (Each).inverse_bind_Matrix);
            begin
--                 the_Site := (the_Site (1),-- * my_Scale * 1.0,
--                              the_Site (2),-- * my_Scale * 1.0,
--                              the_Site (3));-- * my_Scale * 1.0);
               the_Site := (the_Site (1) * my_Scale * 1.0,
                            the_Site (2) * my_Scale * 1.0,
                            the_Site (3) * my_Scale * 1.0);
               set_Translation (Self.controller_Joints (Each).inverse_bind_Matrix,  the_Site);
            end;
         end loop;
      end;


      set_global_Transform_for (Self, the_root_Joint);     -- determine all joint transforms, recursively
      set_Site_for             (the_root_Joint,  parent_site => (0.0, 0.0, 0.0));   -- determine all joint sites

      Self.Model      := Model; --.all'Unchecked_Access;       -- remember our model
      Self.root_Joint := the_root_Joint;                   -- remember our root joint.

--        the_Model.Scale := (my_Scale, my_Scale, my_Scale);



      --  Define a sprite for each bone.
      --
      declare
         use openGL.Model.box.lit_colored_textured, openGL.Model.box,
             openGL, opengl.Palette,
             math.Vectors;
         use type math.Degrees;

         the_joint_Model : openGL.Model.box.lit_colored_textured.view
           := openGL.Model.box.lit_colored_textured.new_Box
             (Size => (0.1, 0.1, 0.1),
              Faces => (front => (colors => (others => (Red,     Opaque)),
                                  texture_Name => openGL.null_Asset),
                        rear  => (colors => (others => (Blue,    Opaque)),
                                  texture_Name => openGL.null_Asset),
                        upper => (colors => (others => (Green,   Opaque)),
                                  texture_Name => openGL.null_Asset),
                        lower => (colors => (others => (Yellow,  Opaque)),
                                  texture_Name => openGL.null_Asset),
                        left  => (colors => (others => (Cyan,    Opaque)),
                                  texture_Name => openGL.null_Asset),
                        right => (colors => (others => (Magenta, Opaque)),
                                  texture_Name => openGL.null_Asset)));


         procedure create_Bone (the_Bone    : in bone_Id;
                                start_Joint : in scene_joint_Id;
                                end_Point   : in math.Vector_3;
                                Scale       : in math.Vector_3;
                                Mass        : in math.Real)
         is
            use Math;
            the_bone_Site        : constant math.Vector_3    :=      midPoint (joint_Sites (start_Joint),  end_Point);
            the_controller_Joint :          controller_Joint renames Self.controller_Joints (controller_joint_Id (the_Bone));
            sprite_Name          : constant String           :=      "human.bone_Sprite" & bone_Id'Image (the_Bone);

            the_physics_Model : constant standard.physics.Model.view
              := standard.physics.Model.Forge.new_physics_Model (shape_Info => (Kind         => standard.physics.Model.Cube,
                                                                                half_Extents => Scale / 2.0),
                                                                 Mass       => Mass);
         begin
            if the_Bone = Hips
            then
               declare
                  the_graphics_Model : constant openGL.Model.box.lit_colored_textured.view
                    := openGL.Model.box.lit_colored_textured.new_Box
                      (Size => (Scale (1) * my_Scale,   Scale (2) * my_Scale,   Scale (3) * my_Scale),
                       Faces => (front => (colors => (others => (Black,     Opaque)),
                                           texture_Name => openGL.null_Asset),
                                 rear  => (colors => (others => (Black,    Opaque)),
                                           texture_Name => openGL.null_Asset),
                                 upper => (colors => (others => (Black,   Opaque)),
                                           texture_Name => openGL.null_Asset),
                                 lower => (colors => (others => (Black,  Opaque)),
                                           texture_Name => openGL.null_Asset),
                                 left  => (colors => (others => (Black,    Opaque)),
                                           texture_Name => openGL.null_Asset),
                                 right => (colors => (others => (Black, Opaque)),
                                           texture_Name => openGL.null_Asset)));

                  the_human_graphics_Model : aliased openGL.Model.any.view
                    := openGL.Model.any.new_Model (--model   => gel.to_Asset ("assets/gel/model/gel-human.dae"),
                                                   Model   => openGL.to_Asset ("assets/gel/collada/mh-human-dae.dae"),
                                                   -- model   => gel.to_Asset ("assets/gel/collada/alfieri.dae"),
                                                   Texture => openGL.null_Asset, -- gel.to_Asset ("assets/collada/gel-human-texture.tga"),
                                                   Texture_is_lucid => False);
               begin
                  Self.bone_Sprites (the_Bone) := gel.Sprite.forge.new_Sprite (sprite_Name,
                                                                               gel.sprite.World_view (World),
                                                                               Origin_3D,
                                                                               the_human_graphics_Model,
--                                                                                 the_graphics_Model,
--                                                                                 Model,
                                                                               the_physics_Model,
                                                                               owns_graphics => True,
                                                                               owns_physics  => True,
                                                                               is_kinematic  => is_Kinematic);
--                    Self.bone_Sprites (the_Bone).is_Visible (True);
                  --              Self.bone_Sprites (the_Bone).is_Visible (True);
               end;
            else
               declare
                  the_graphics_Model : constant openGL.Model.box.lit_colored_textured.view
                    := openGL.Model.box.lit_colored_textured.new_Box
                      (Size  => (Scale (1) * my_Scale,   Scale (2) * my_Scale,   Scale (3) * my_Scale),
                       Faces => (front => (colors => (others => (Red,     Opaque)),
                                           texture_Name => openGL.null_Asset),
                                 rear  => (colors => (others => (Blue,    Opaque)),
                                           texture_Name => openGL.null_Asset),
                                 upper => (colors => (others => (Green,   Opaque)),
                                           texture_Name => openGL.null_Asset),
                                 lower => (colors => (others => (Yellow,  Opaque)),
                                           texture_Name => openGL.null_Asset),
                                 left  => (colors => (others => (Cyan,    Opaque)),
                                           texture_Name => openGL.null_Asset),
                                 right => (colors => (others => (Magenta, Opaque)),
                                           texture_Name => openGL.null_Asset)));
               begin
--                    raise program_Error with "TBD";
                  Self.bone_Sprites (the_Bone) := gel.Sprite.forge.new_Sprite (sprite_Name,
                                                                               gel.sprite.World_view (World),
                                                                               Origin_3D,
                                                                               the_graphics_Model,
                                                                               the_physics_Model,
                                                                               owns_graphics => True,
                                                                               owns_physics  => True,
                                                                               is_kinematic  => is_Kinematic);
--                    Self.bone_Sprites (the_Bone).is_Visible (False);
--                    Self.bone_Sprites (the_Bone).is_Visible (True);
               end;
            end if;

--              the_bone_Site := the_bone_Site * my_Scale;

            Self.bone_Sprites (the_Bone).Site_is (the_bone_Site);
            Self.bone_Sprites (the_Bone).Spin_is (get_Rotation (the_controller_Joint.inverse_bind_Matrix));

            the_controller_Joint.joint_to_bone_site_Offet
              :=   Inverse (get_Rotation (the_controller_Joint.inverse_bind_Matrix))
                 * (joint_Sites (start_Joint) - the_bone_Site);
         end create_Bone;



         procedure attach_via_Ball (bone_A_Id, bone_B_Id : in Bone_Id;
                                    pitch_limits,
                                    yaw_limits,
                                    roll_Limits          : in gel.Sprite.dof_limits := (math.to_Radians (-20.0),
                                                                                        math.to_Radians ( 20.0)))
         is
            use Math;
            joint_Id       : constant controller_joint_Id
              := controller_joint_Id (bone_B_Id);

            the_joint_Site : constant math.Vector_3
              := joint_Sites (scene_joint_id'Value (bone_id'Image (bone_B_Id)));

            Bone_A  : constant gel.Sprite.view := Self.bone_Sprites (bone_A_Id);
            Bone_B  : constant gel.Sprite.view := Self.bone_Sprites (bone_B_Id);

            Frame_A : math.Matrix_4x4 := Self.controller_Joints (controller_joint_Id (bone_A_Id)).inverse_bind_Matrix;
            Frame_B : math.Matrix_4x4 := Self.controller_Joints (controller_joint_Id (bone_B_Id)).inverse_bind_Matrix;

            A_rot   : constant math.Matrix_3x3 := inverse (get_Rotation (Frame_A));
            B_rot   : constant math.Matrix_3x3 := inverse (get_Rotation (Frame_B));
         begin
            set_Translation (Frame_A,  A_rot * (the_joint_Site - Bone_A.Site));
            set_Translation (Frame_B,  B_rot * (the_joint_Site - Bone_B.Site));
--              set_Translation (Frame_A,  A_rot * (the_joint_Site - Bone_A.Site) * my_Scale);
--              set_Translation (Frame_B,  B_rot * (the_joint_Site - Bone_B.Site) * my_Scale);

--              if    bone_A_Id = hips
--                and bone_B_Id = spine1
--              then
--                 set_Rotation (Frame_B,  X_Rotation_from (to_Radians (180.0)));
--              end if;


            Self.bone_Sprites (bone_A_Id).attach_via_ball_Socket (Self.bone_Sprites (bone_B_Id),
                                                                  frame_in_parent => Frame_A,
                                                                  frame_in_child  => Frame_B,
                                                                  pitch_limits    => pitch_Limits,
                                                                  yaw_limits      => yaw_limits,
                                                                  roll_limits     => roll_limits,
                                                                  new_joint       => Self.Joints (joint_Id));
         end attach_via_Ball;


         procedure attach_via_Hinge (bone_A_Id, bone_B_Id : in Bone_Id;
                                     Limits               : in gel.Sprite.dof_limits := (math.to_Radians ( 0.0),
                                                                                         math.to_Radians (90.0)))
         is
            use Math;
            joint_Id       : constant controller_joint_Id
              := controller_joint_Id (bone_B_Id);

            the_joint_Site : constant math.Vector_3
              := joint_Sites (scene_joint_id'Value (bone_id'Image (bone_B_Id)));

            Bone_A  : constant gel.Sprite.view := Self.bone_Sprites (bone_A_Id);
            Bone_B  : constant gel.Sprite.view := Self.bone_Sprites (bone_B_Id);

            Frame_A : math.Matrix_4x4 := Self.controller_Joints (controller_joint_Id (bone_A_Id)).inverse_bind_Matrix;
            Frame_B : math.Matrix_4x4 := Self.controller_Joints (controller_joint_Id (bone_B_Id)).inverse_bind_Matrix;

            A_rot   : constant math.Matrix_3x3 := inverse (get_Rotation (Frame_A));
            B_rot   : constant math.Matrix_3x3 := inverse (get_Rotation (Frame_B));
         begin
            set_Translation (Frame_A,  A_rot * (the_joint_Site - Bone_A.Site));
            set_Translation (Frame_B,  B_rot * (the_joint_Site - Bone_B.Site));
--              set_Translation (Frame_A,  A_rot * (the_joint_Site - Bone_A.Site) * my_Scale);
--              set_Translation (Frame_B,  B_rot * (the_joint_Site - Bone_B.Site) * my_Scale);

            set_Rotation    (Frame_A,  A_rot * z_rotation_from (math.to_Radians (-90.0)));
            set_Rotation    (Frame_B,  B_rot * z_rotation_from (math.to_Radians (-90.0)));


            Self.bone_Sprites (bone_A_Id).attach_via_Hinge (Self.bone_Sprites (bone_B_Id),
                                                            frame_in_parent   => Frame_A,
                                                            frame_in_child    => Frame_B,
                                                            limits            => Limits,
                                                            collide_Connected => False,
                                                            new_joint         => Self.Joints (joint_Id));
         end attach_via_Hinge;

         use Math;


         bone_Extent : math.Real;
      begin
--           the_Model.Scale := (1.0, 1.0, 0.7);
--           the_Model.Scale := (0.5, 0.5, 0.5);
--           the_Model.Scale := (0.5 * my_Scale, 0.5 * my_Scale, 0.5 * my_Scale);


         --  the MasterFloor/Base sprite
         --


         --  hips
         --
         bone_Extent := 0.5; -- * Distance (joint_Sites (Hips), to => joint_Sites (Spine1));
         create_Bone (Hips,
                      Hips,
                      joint_Sites (Root),
                      bone_Extent * (4.0, 1.0, 2.0),
                      Mass => 0.0); -- 1.0 * 0.5);

--           Self.enable_Graphics;



         --  spine1
         --
         bone_Extent := 0.75 * Distance (joint_Sites (Spine1), to => joint_Sites (Spine2));
--           create_Bone (Spine1,  Spine1, joint_Sites (Spine2),  (0.6, 0.4, 0.7) * bone_Extent,  0.5); -- 0.6 * 0.5);
         create_Bone (Spine1,  Spine1, joint_Sites (Spine2),  (2.0, 0.8, 1.5) * bone_Extent,  0.5); -- 0.6 * 0.5);

         attach_via_Ball (Hips, Spine1);
--           attach_via_Hinge (Hips, Spine1);


         --  spine2
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Spine2), to => joint_Sites (Spine3));
         create_Bone (Spine2,  Spine2, joint_Sites (Spine3),  (0.6, 0.8, 0.8) * bone_Extent,  0.5); -- 0.5 * 0.5);

         attach_via_Ball (Spine1, Spine2);


         --  spine3
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Spine3), to => joint_Sites (Neck));
         create_Bone (Spine3,  Spine3, joint_Sites (Neck),  (1.0, 0.6, 0.5) * bone_Extent,  0.5); -- 0.4 * 0.5);

         attach_via_Ball (Spine2, Spine3);


         --  neck
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Neck), to => joint_Sites (Head));
         create_Bone (Neck,  Neck, joint_Sites (Head),  (0.3, 0.3, 0.2) * bone_Extent,  0.4); -- 0.4 * 0.5);

         attach_via_Ball (Spine3, Neck);


         --  head
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Head), to => joint_Sites (Neck));
--           create_Bone (Head,  Head, joint_Sites (Head) + (0.0, 0.0, 0.5),  (0.6, 0.5, 0.3) * bone_Extent,  0.25);
         create_Bone (Head,  Head, joint_Sites (Head) + (0.0, 0.0, 0.5),  (1.0, 0.7, 1.4) * bone_Extent,  0.25);

         attach_via_Ball (Neck, Head);


         --- left arm
         --

         --  left clavicle
         --
         bone_Extent := 0.6 * Distance (joint_Sites (Clavicle_L), to => joint_Sites (upArm_L));
         create_Bone (Clavicle_L,  Clavicle_L, joint_Sites (upArm_L),  (0.25, 1.0, 0.5) * bone_Extent,  0.5);

         attach_via_Ball (Spine3,   Clavicle_L,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_limits  => (-0.5, 0.5));


         --  left upper arm
         --
         bone_Extent := 0.75 * Distance (joint_Sites (upArm_L), to => joint_Sites (loArm_L));
         create_Bone (upArm_L,  upArm_L, joint_Sites (loArm_L),  (1.0, 0.2, 0.2) * bone_Extent,  0.5); -- 0.4 * 0.5);
--           create_Bone (upArm_L,  upArm_L, joint_Sites (loArm_L),  (0.2, 1.0, 0.2) * bone_Extent,  0.5); -- 0.4 * 0.5);

         attach_via_Ball (Clavicle_L,   upArm_L,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_limits  => (-0.5, 0.5));


         --  left lower arm
         --
         bone_Extent := 0.75 * Distance (joint_Sites (loArm_L), to => joint_Sites (Hand_L));
         create_Bone (loArm_L,  loArm_L, joint_Sites (Hand_L),  (1.0, 0.2, 0.2) * bone_Extent,  0.5); -- 0.4 * 0.5);

         attach_via_Ball (upArm_L,   loArm_L,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_limits  => (-0.5, 0.5));


         --  left hand
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Hand_L), to => joint_Sites (loArm_L));
         create_Bone (Hand_L,  Hand_L,  joint_Sites (Hand_L) + (1.0, 0.0, 0.0),  (0.4, 0.08, 0.15) * bone_Extent,  0.5);

         attach_via_Ball (loArm_L,   Hand_L,
                          pitch_limits => (-0.5, 0.0),
                          yaw_limits   => (-0.5, 0.0),
                          roll_limits  => (-0.0, 0.0));



         --- right arm
         --

         --  right clavicle
         --
         bone_Extent := 0.6 * Distance (joint_Sites (Clavicle_R), to => joint_Sites (upArm_R));
         create_Bone (Clavicle_R,  Clavicle_R, joint_Sites (upArm_R),  (0.25, 1.0, 0.5) * bone_Extent,  0.5);

         attach_via_Ball (Spine3,  Clavicle_R);


         --  right upper arm
         --
         bone_Extent := 0.75 * Distance (joint_Sites (upArm_R), to => joint_Sites (loArm_R));
         create_Bone (upArm_R,  upArm_R, joint_Sites (loArm_R),  (1.0, 0.2, 0.2) * bone_Extent,  0.5); -- 0.4 * 0.5);

         attach_via_Ball (Clavicle_R,  upArm_R);


         --  right lower arm
         --
         bone_Extent := 0.75 * Distance (joint_Sites (loArm_R), to => joint_Sites (Hand_R));
         create_Bone (loArm_R,  loArm_R, joint_Sites (Hand_R),  (1.0, 0.2, 0.2) * bone_Extent,  0.5); -- 0.4 * 0.5);

         attach_via_Ball (upArm_R,  loArm_R);



         --  right hand
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Hand_R), to => joint_Sites (loArm_R));
         create_Bone (Hand_R,  Hand_R,  joint_Sites (Hand_R) - (1.0, 0.0, 0.0),  (0.4, 0.08, 0.15) * bone_Extent, 0.5);

         attach_via_Ball (loArm_R,  Hand_R);


         --- left leg
         --

         --  left upper leg
         --
         bone_Extent := 0.8 * Distance (joint_Sites (upLeg_L), to => joint_Sites (loLeg_L));
         create_Bone (upLeg_L,  upLeg_L, joint_Sites (loLeg_L),  (0.2, 1.0, 0.2) * bone_Extent,  1.1);

         attach_via_Ball (Hips,  upLeg_L);


         --  left lower leg
         --
         bone_Extent := 0.9 * Distance (joint_Sites (loLeg_L), to => joint_Sites (Foot_L));
         create_Bone (loLeg_L,  loLeg_L, joint_Sites (Foot_L),  (0.15, 1.0, 0.15) * bone_Extent,  1.1);

         attach_via_Hinge (upLeg_L,  loLeg_L);


         --  left foot
         --
         bone_Extent := 0.9 * Distance (joint_Sites (Foot_L), to => joint_Sites (Toe_L));
         create_Bone (Foot_L,  Foot_L, joint_Sites (Toe_L),  (0.5, 0.20, 1.0) * bone_Extent,  1.1);

         attach_via_Ball (loLeg_L,  Foot_L);


         --- right leg
         --

         --  right upper leg
         --
         bone_Extent := 0.8 * Distance (joint_Sites (upLeg_R), to => joint_Sites (loLeg_R));
         create_Bone (upLeg_R,  upLeg_R, joint_Sites (loLeg_R),  (0.2, 1.0, 0.2) * bone_Extent,  1.1);

         attach_via_Ball (Hips,  upLeg_R);


         --  right lower leg
         --
         bone_Extent := 0.9 * Distance (joint_Sites (loLeg_R), to => joint_Sites (Foot_R));
         create_Bone (loLeg_R,  loLeg_R, joint_Sites (Foot_R),  (0.15, 1.0, 0.15) * bone_Extent,  1.1);

         attach_via_Hinge (upLeg_R,  loLeg_R);


         --  right foot
         --
         bone_Extent := 0.9 * Distance (joint_Sites (Foot_R), to => joint_Sites (Toe_R));
         create_Bone (Foot_R,  Foot_R, joint_Sites (Toe_R),  (0.5, 0.20, 1.0) * bone_Extent,  1.1);

         attach_via_Ball (loLeg_R,  Foot_R);
      end;




      --- Parse the Collada animations file.
      --
      declare
         use collada.Library.animations;

         the_Animations  : constant access animations.Animation_array := the_Document.Libraries.Animations.Contents;
      begin
         if the_Animations /= null then
            for Each in the_Animations'Range loop
               declare
                  the_Animation   : constant animations.Animation       := the_Animations (Each);
                  the_Inputs      : access collada.float_Array        := Inputs_of (the_Animation);

                  procedure setup (Channel : channel_Id;   scene_Joint : scene_Joint_Id;   Sid : in String)
                  is
                  begin
                     Self.Channels (Channel).Target := Self.scene_Joints (scene_Joint).Node.fetch_Transform (Sid);
                     Self.Channels (Channel).Times  := Inputs_of  (the_Animation);
                     Self.Channels (Channel).Angles := outputs_of (the_Animation);
                     Self.Channels (Channel).current_Angle := Self.Channels (Channel).Angles (1);
                     Self.Channels (Channel).initial_Angle := Self.Channels (Channel).current_Angle;


                     for Each in Self.Channels (Channel).Times'Range loop
                        Self.Channels (Channel).Times (Each) := Self.Channels (Channel).Times (Each) / 5.0;
                     end loop;

                  end setup;

                  procedure setup_Location (Channel : channel_Id;   scene_Joint : scene_Joint_Id;   Sid : in String)
                  is
                  begin
                     Self.Channels (Channel).Target := Self.scene_Joints (scene_Joint).Node.fetch_Transform (Sid);
                     Self.Channels (Channel).Times  := Inputs_of  (the_Animation);
                     Self.Channels (Channel).Angles := outputs_of (the_Animation);
                     Self.Channels (Channel).current_Site  := (Self.Channels (Channel).Angles (1),
                                                               Self.Channels (Channel).Angles (2),
                                                               Self.Channels (Channel).Angles (3));
                     Self.Channels (Channel).initial_Site  := Self.Channels (Channel).current_Site;


                     for Each in Self.Channels (Channel).Times'Range loop
                        Self.Channels (Channel).Times (Each) := Self.Channels (Channel).Times (Each) / 5.0;
                     end loop;
                  end setup_Location;

               begin
                  if    +the_Animation.Channel.Target = "Root/rotationX.ANGLE" then
                     setup (root_x, Root, "rotationX");
                  elsif +the_Animation.Channel.Target = "Root/rotationY.ANGLE" then
                     setup (root_y, Root, "rotationY");
                  elsif +the_Animation.Channel.Target = "Root/rotationZ.ANGLE" then
                     setup (root_z, Root, "rotationZ");
                  elsif +the_Animation.Channel.Target = "Root/location" then
                     setup_Location (root_loc, Root, "location");

                  elsif +the_Animation.Channel.Target = "Spine1/rotationX.ANGLE" then
                     setup (spine_1_x, Spine1, "rotationX");
                  elsif +the_Animation.Channel.Target = "Spine1/rotationY.ANGLE" then
                     setup (spine_1_y, Spine1, "rotationY");
                  elsif +the_Animation.Channel.Target = "Spine1/rotationZ.ANGLE" then
                     setup (spine_1_z, Spine1, "rotationZ");

                  elsif +the_Animation.Channel.Target = "Spine2/rotationX.ANGLE" then
                     setup (spine_2_x, Spine2, "rotationX");
                  elsif +the_Animation.Channel.Target = "Spine2/rotationY.ANGLE" then
                     setup (spine_2_y, Spine2, "rotationY");
                  elsif +the_Animation.Channel.Target = "Spine2/rotationZ.ANGLE" then
                     setup (spine_2_z, Spine2, "rotationZ");

                  elsif +the_Animation.Channel.Target = "Spine3/rotationX.ANGLE" then
                     setup (spine_3_x, Spine3, "rotationX");
                  elsif +the_Animation.Channel.Target = "Spine3/rotationY.ANGLE" then
                     setup (spine_3_y, Spine3, "rotationY");
                  elsif +the_Animation.Channel.Target = "Spine3/rotationZ.ANGLE" then
                     setup (spine_3_z, Spine3, "rotationZ");

                  elsif +the_Animation.Channel.Target = "Neck/rotationX.ANGLE" then
                     setup (neck_x, Neck, "rotationX");
                  elsif +the_Animation.Channel.Target = "Neck/rotationY.ANGLE" then
                     setup (neck_y, Neck, "rotationY");
                  elsif +the_Animation.Channel.Target = "Neck/rotationZ.ANGLE" then
                     setup (neck_z, Neck, "rotationZ");

                  elsif +the_Animation.Channel.Target = "Head/rotationX.ANGLE" then
                     setup (head_x, Head, "rotationX");
                  elsif +the_Animation.Channel.Target = "Head/rotationY.ANGLE" then
                     setup (head_y, Head, "rotationY");
                  elsif +the_Animation.Channel.Target = "Head/rotationZ.ANGLE" then
                     setup (head_z, Head, "rotationZ");


                  elsif +the_Animation.Channel.Target = "Clavicle_L/rotationX.ANGLE" then
                     setup (l_clavicle_x, Clavicle_L, "rotationX");
                  elsif +the_Animation.Channel.Target = "Clavicle_L/rotationY.ANGLE" then
                     setup (l_clavicle_y, Clavicle_L, "rotationY");
                  elsif +the_Animation.Channel.Target = "Clavicle_L/rotationZ.ANGLE" then
                     setup (l_clavicle_z, Clavicle_L, "rotationZ");

                  elsif +the_Animation.Channel.Target = "UpArm_L/rotationX.ANGLE" then
                     setup (l_upArm_x, UpArm_L, "rotationX");
                  elsif +the_Animation.Channel.Target = "UpArm_L/rotationY.ANGLE" then
                     setup (l_upArm_y, UpArm_L, "rotationY");
                  elsif +the_Animation.Channel.Target = "UpArm_L/rotationZ.ANGLE" then
                     setup (l_upArm_z, UpArm_L, "rotationZ");

                  elsif +the_Animation.Channel.Target = "LoArm_L/rotationX.ANGLE" then
                     setup (l_loArm_x, LoArm_L, "rotationX");
                  elsif +the_Animation.Channel.Target = "LoArm_L/rotationY.ANGLE" then
                     setup (l_loArm_y, LoArm_L, "rotationY");
                  elsif +the_Animation.Channel.Target = "LoArm_L/rotationZ.ANGLE" then
                     setup (l_loArm_z, LoArm_L, "rotationZ");

                  elsif +the_Animation.Channel.Target = "Hand_L/rotationX.ANGLE" then
                     setup (l_Hand_x, Hand_L, "rotationX");
                  elsif +the_Animation.Channel.Target = "Hand_L/rotationY.ANGLE" then
                     setup (l_Hand_y, Hand_L, "rotationY");
                  elsif +the_Animation.Channel.Target = "Hand_L/rotationZ.ANGLE" then
                     setup (l_Hand_z, Hand_L, "rotationZ");

                  elsif +the_Animation.Channel.Target = "Wrist_L/rotationX.ANGLE" then
                     setup (l_Wrist_x, Wrist_L, "rotationX");
                  elsif +the_Animation.Channel.Target = "Wrist_L/rotationY.ANGLE" then
                     setup (l_Wrist_y, Wrist_L, "rotationY");
                  elsif +the_Animation.Channel.Target = "Wrist_L/rotationZ.ANGLE" then
                     setup (l_Wrist_z, Wrist_L, "rotationZ");
                  elsif +the_Animation.Channel.Target = "Wrist_L/location" then
                     setup_Location (l_Wrist_loc, Wrist_L, "location");


                  elsif +the_Animation.Channel.Target = "Clavicle_R/rotationX.ANGLE" then
                     setup (r_clavicle_x, Clavicle_R, "rotationX");
                  elsif +the_Animation.Channel.Target = "Clavicle_R/rotationY.ANGLE" then
                     setup (r_clavicle_y, Clavicle_R, "rotationY");
                  elsif +the_Animation.Channel.Target = "Clavicle_R/rotationZ.ANGLE" then
                     setup (r_clavicle_z, Clavicle_R, "rotationZ");

                  elsif +the_Animation.Channel.Target = "UpArm_R/rotationX.ANGLE" then
                     setup (r_upArm_x, UpArm_R, "rotationX");
                  elsif +the_Animation.Channel.Target = "UpArm_R/rotationY.ANGLE" then
                     setup (r_upArm_y, UpArm_R, "rotationY");
                  elsif +the_Animation.Channel.Target = "UpArm_R/rotationZ.ANGLE" then
                     setup (r_upArm_z, UpArm_R, "rotationZ");

                  elsif +the_Animation.Channel.Target = "LoArm_R/rotationX.ANGLE" then
                     setup (r_loArm_x, LoArm_R, "rotationX");
                  elsif +the_Animation.Channel.Target = "LoArm_R/rotationY.ANGLE" then
                     setup (r_loArm_y, LoArm_R, "rotationY");
                  elsif +the_Animation.Channel.Target = "LoArm_R/rotationZ.ANGLE" then
                     setup (r_loArm_z, LoArm_R, "rotationZ");

                  elsif +the_Animation.Channel.Target = "Hand_R/rotationX.ANGLE" then
                     setup (r_Hand_x, Hand_R, "rotationX");
                  elsif +the_Animation.Channel.Target = "Hand_R/rotationY.ANGLE" then
                     setup (r_Hand_y, Hand_R, "rotationY");
                  elsif +the_Animation.Channel.Target = "Hand_R/rotationZ.ANGLE" then
                     setup (r_Hand_z, Hand_R, "rotationZ");

                  elsif +the_Animation.Channel.Target = "Wrist_R/rotationX.ANGLE" then
                     setup (r_Wrist_x, Wrist_R, "rotationX");
                  elsif +the_Animation.Channel.Target = "Wrist_R/rotationY.ANGLE" then
                     setup (r_Wrist_y, Wrist_R, "rotationY");
                  elsif +the_Animation.Channel.Target = "Wrist_R/rotationZ.ANGLE" then
                     setup (r_Wrist_z, Wrist_R, "rotationZ");
                  elsif +the_Animation.Channel.Target = "Wrist_R/location" then
                     setup_Location (r_Wrist_loc, Wrist_R, "location");


                  elsif +the_Animation.Channel.Target = "UpLeg_L/rotationX.ANGLE" then
                     setup (l_upLeg_x, UpLeg_L, "rotationX");
                  elsif +the_Animation.Channel.Target = "UpLeg_L/rotationY.ANGLE" then
                     setup (l_upLeg_y, UpLeg_L, "rotationY");
                  elsif +the_Animation.Channel.Target = "UpLeg_L/rotationZ.ANGLE" then
                     setup (l_upLeg_z, UpLeg_L, "rotationZ");

                  elsif +the_Animation.Channel.Target = "LoLeg_L/rotationX.ANGLE" then
                     setup (l_loLeg_x, LoLeg_L, "rotationX");
                  elsif +the_Animation.Channel.Target = "LoLeg_L/rotationY.ANGLE" then
                     setup (l_loLeg_y, LoLeg_L, "rotationY");
                  elsif +the_Animation.Channel.Target = "LoLeg_L/rotationZ.ANGLE" then
                     setup (l_loLeg_z, LoLeg_L, "rotationZ");

                  elsif +the_Animation.Channel.Target = "Foot_L/rotationX.ANGLE" then
                     setup (l_Foot_x, Foot_L, "rotationX");
                  elsif +the_Animation.Channel.Target = "Foot_L/rotationY.ANGLE" then
                     setup (l_Foot_y, Foot_L, "rotationY");
                  elsif +the_Animation.Channel.Target = "Foot_L/rotationZ.ANGLE" then
                     setup (l_Foot_z, Foot_L, "rotationZ");


                  elsif +the_Animation.Channel.Target = "UpLeg_R/rotationX.ANGLE" then
                     setup (r_upLeg_x, UpLeg_R, "rotationX");
                  elsif +the_Animation.Channel.Target = "UpLeg_R/rotationY.ANGLE" then
                     setup (r_upLeg_y, UpLeg_R, "rotationY");
                  elsif +the_Animation.Channel.Target = "UpLeg_R/rotationZ.ANGLE" then
                     setup (r_upLeg_z, UpLeg_R, "rotationZ");

                  elsif +the_Animation.Channel.Target = "LoLeg_R/rotationX.ANGLE" then
                     setup (r_loLeg_x, LoLeg_R, "rotationX");
                  elsif +the_Animation.Channel.Target = "LoLeg_R/rotationY.ANGLE" then
                     setup (r_loLeg_y, LoLeg_R, "rotationY");
                  elsif +the_Animation.Channel.Target = "LoLeg_R/rotationZ.ANGLE" then
                     setup (r_loLeg_z, LoLeg_R, "rotationZ");

                  elsif +the_Animation.Channel.Target = "Foot_R/rotationX.ANGLE" then
                     setup (r_Foot_x, Foot_R, "rotationX");
                  elsif +the_Animation.Channel.Target = "Foot_R/rotationY.ANGLE" then
                     setup (r_Foot_y, Foot_R, "rotationY");
                  elsif +the_Animation.Channel.Target = "Foot_R/rotationZ.ANGLE" then
                     setup (r_Foot_z, Foot_R, "rotationZ");
                  end if;
               end;
            end loop;
         end if;

      end;

   end define;



   procedure motion_Mode_is (Self : in out Item;   Now : in motion_Mode)
   is
   begin
      Self.Mode := Now;
   end motion_Mode_is;




   procedure enable_Graphics (Self : in out Item)
   is
      use ada.Strings;
   begin
      Self.program_Parameters.Program_is (opengl.Program.view (opengl.Geometry.lit_textured_skinned.Program));
      Self.base_Sprite.program_Parameters_are (Self.program_Parameters'Unchecked_Access);
   end enable_Graphics;



   function  controller_Joints (Self : in Item'Class) return human_types.controller_Joints
   is
   begin
      return Self.controller_Joints;
   end controller_Joints;



   procedure controller_Joints_are (Self : in out Item'Class;   Now : in human_types.controller_Joints)
   is
   begin
      Self.controller_Joints := Now;
   end controller_Joints_are;






   --- Attributes
   --

   function Sprite (Self : in Item'Class;   for_Bone : in bone_Id) return gel.Sprite.view
   is
   begin
      return Self.bone_Sprites (for_Bone);
   end Sprite;



   function  base_Sprite (Self : in Item'Class) return gel.Sprite.view
   is
   begin
      return Self.bone_Sprites (Hips);
   end base_Sprite;



   procedure set_Transform (Self : in out Item'Class;   for_Bone : in controller_joint_Id;
                                                        To       : in math.Matrix_4x4)
   is
      use gel.Conversions;
   begin
      Self.program_Parameters.bone_Transforms (for_Bone) := to_GL (To);
   end set_Transform;




   --- Operations
   --

   procedure evolve (Self : in out Item'Class)
   is
      use Math, math.Vectors;

      function get_root_Transform return math.Matrix_4x4
      is
      begin
         if Self.Mode = Physics then
            return Self.base_Sprite.Transform;
         else
            declare
               the_Transform : math.Matrix_4x4 := math.Identity_4x4;
            begin
               set_Rotation    (the_Transform,  x_rotation_from (to_Radians (-90.0)));
               set_Translation (the_Transform,
                                -get_Translation (Inverse (Self.controller_Joints (masterFloor).inverse_bind_Matrix)));
               return the_Transform;
            end;
         end if;
      end get_root_Transform;


--        subtype as_Hinge is gel.hinge_Joint.view;
--        root_Transform     : math.Matrix_4x4 := Self.base_Sprite.Transform
--                                  * as_Hinge (Self.Joints (base_To_hips)).Frame_A * Self.bone_inv_bind_Matrices (Root);

      root_Transform     : constant math.Matrix_4x4 := get_root_Transform;
      inv_root_Transform : constant math.Matrix_4x4 := Inverse (root_Transform);


      function joint_Transform_for (the_collada_Joint : in controller_joint_Id) return math.Matrix_4x4
      is
      begin
         if Self.Mode = Physics then
            declare
               the_bone_Transform    : constant math.Matrix_4x4
                 := Self.bone_Sprites (bone_Id (the_collada_Joint)).Transform;

               the_bone_Rotation     : constant math.Matrix_3x3
                 := get_Rotation (the_bone_Transform);
--                 the_inv_bone_Rotation : math.Matrix_3x3 := Inverse (the_bone_Rotation);

               the_joint_site_Offset : constant math.Vector_3
                 := Self.controller_Joints (the_collada_Joint).joint_to_bone_site_Offet * the_bone_Rotation;

               the_joint_Transform   : math.Matrix_4x4;
            begin
               set_Translation (the_joint_Transform,  get_Translation (the_bone_Transform) + the_joint_site_Offset);
               set_Rotation    (the_joint_Transform,  get_Rotation    (the_bone_Transform));

               return the_joint_Transform;
            end;
         else   -- must be animation mode
            return Self.scene_Joints (to_scene_joint_Id (the_collada_Joint)).Transform;
         end if;
      end joint_Transform_for;



      procedure set_Transform_for (the_Bone : in controller_joint_Id)
      is
      begin
         Self.set_Transform (for_bone => the_Bone,
                             to       =>   Self.controller_Joints (the_Bone).inverse_bind_Matrix
                                         * joint_Transform_for (the_Bone)
                                         * inv_root_Transform);
      end set_Transform_for;



      procedure set_proxy_Transform_for (the_Bone : in controller_joint_Id;   the_Proxy : in controller_joint_Id)
      is
      begin
         Self.set_Transform (for_bone => the_Bone,
                             to       =>   Self.controller_Joints (the_Proxy).inverse_bind_Matrix
                                         * joint_Transform_for (the_Proxy)
                                         * inv_root_Transform);
      end set_proxy_Transform_for;


   begin
      if not Self.Graphics_enabled then
         Self.enable_Graphics;
         Self.Graphics_enabled := True;
      end if;



--        new_Line;
--        put_Line (math.Image (math.Matrix (inv_root_Transform)));
--        new_Line;
--        put_Line (math.Image (math.Matrix (the_hinge_Joint.Frame_A)));
--        put_Line (math.Image (math.Matrix (Self.Sprites (Spine1).Transform * the_hinge_Joint.Frame_A)));


      set_Transform_for (Hips);
      set_Transform_for (Spine1);
      set_Transform_for (Spine2);
      set_Transform_for (Spine3);
      set_Transform_for (Neck);
      set_Transform_for (Head);

      set_proxy_Transform_for (Jaw,        the_proxy => Head);
      set_proxy_Transform_for (TongueBase, the_proxy => Head);
      set_proxy_Transform_for (TongueMid,  the_proxy => Head);
      set_proxy_Transform_for (TongueTip,  the_proxy => Head);
      set_proxy_Transform_for (Eye_r,      the_proxy => Head);
      set_proxy_Transform_for (Eye_l,      the_proxy => Head);
      set_proxy_Transform_for (upLid_r,    the_proxy => Head);
      set_proxy_Transform_for (loLid_r,    the_proxy => Head);
      set_proxy_Transform_for (upLid_l,    the_proxy => Head);
      set_proxy_Transform_for (loLid_l,    the_proxy => Head);


      set_proxy_Transform_for (Wrist_L,      the_proxy => Hand_L);
      set_proxy_Transform_for (Wrist_R,      the_proxy => Hand_R);
      set_proxy_Transform_for (Ankle_L,      the_proxy => Foot_L);
      set_proxy_Transform_for (Ankle_R,      the_proxy => Foot_R);


      --  left arm
      --
      set_Transform_for (Clavicle_L);
      set_Transform_for (upArm_L);
      set_Transform_for (loArm_L);
      set_Transform_for (Hand_L);


      set_proxy_Transform_for (Wrist_1_L,    the_proxy => Hand_L);
      set_proxy_Transform_for (Palm_2_L,     the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_2_1_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_2_2_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_2_3_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Palm_3_L,     the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_3_1_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_3_2_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_3_3_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Wrist_2_L,    the_proxy => Hand_L);
      set_proxy_Transform_for (Palm_4_L,     the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_4_1_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_4_2_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_4_3_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Palm_5_L,     the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_5_1_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_5_2_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_5_3_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Palm_1_L,     the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_1_1_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_1_2_L, the_proxy => Hand_L);
      set_proxy_Transform_for (Finger_1_3_L, the_proxy => Hand_L);



      --  right arm
      --
      set_Transform_for (Clavicle_R);
      set_Transform_for (upArm_R);
      set_Transform_for (loArm_R);
      set_Transform_for (Hand_R);

      set_proxy_Transform_for (Wrist_1_R,    the_proxy => Hand_R);
      set_proxy_Transform_for (Palm_2_R,     the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_2_1_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_2_2_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_2_3_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Palm_3_R,     the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_3_1_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_3_2_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_3_3_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Wrist_2_R,    the_proxy => Hand_R);
      set_proxy_Transform_for (Palm_4_R,     the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_4_1_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_4_2_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_4_3_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Palm_5_R,     the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_5_1_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_5_2_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_5_3_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Palm_1_R,     the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_1_1_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_1_2_R, the_proxy => Hand_R);
      set_proxy_Transform_for (Finger_1_3_R, the_proxy => Hand_R);


      --  left leg
      --
      set_Transform_for (upLeg_L);
      set_Transform_for (loLeg_L);

      set_Transform_for       (Foot_L);
      set_proxy_Transform_for (Toe_L, the_proxy => Foot_L);


      --  right leg
      --
      set_Transform_for (upLeg_R);
      set_Transform_for (loLeg_R);

      set_Transform_for       (Foot_R);
      set_proxy_Transform_for (Toe_R, the_proxy => Foot_R);
   end evolve;






   --  animate
   --
   procedure animate (Self : in out Item;   world_Age : in Duration)
   is
      Now     : Duration;
      Elapsed : Duration;


      procedure update_rotation_Animation (for_Channel : in channel_Id;
                                           for_Joint   : in gel.human.scene_joint_Id;
                                           for_Axis    : in gel.human.axis_Kind)
      is
         use Math;

         the_Channel : animation_Channel renames Self.Channels (for_Channel);
         Cursor      : math.Index        renames the_Channel.Cursor;
--           Elapsed     : Duration          :=      Now - start_Time;

         function Reduced (Angle : in math.Real) return math.Real
         is
         begin
            if    Angle >  180.0 then   return -360.0 + Angle;
            elsif Angle < -180.0 then   return  360.0 + Angle;
            else                        return  Angle;
            end if;
         end Reduced;

      begin
         if Cursor < the_Channel.Times'Last then
            if        Cursor = 0
              or else Elapsed > Duration (the_Channel.Times (Cursor))
            then
               Cursor := Cursor + 1;

               if Cursor = 1 then
                  if the_Channel.Times  (Cursor) = 0.0 then
                     the_Channel.interp_Delta := Reduced (the_Channel.Angles (Cursor) - the_Channel.current_Angle);
                  else
                     the_Channel.interp_Delta :=   Reduced (the_Channel.Angles (Cursor) - the_Channel.current_Angle)
                                                 / (the_Channel.Times  (Cursor));
                  end if;
               else
                  the_Channel.interp_Delta :=   Reduced (the_Channel.Angles (Cursor) - the_Channel.current_Angle)
                                              / (the_Channel.Times  (Cursor) - the_Channel.Times (Cursor - 1));
               end if;

               the_Channel.interp_Delta := the_Channel.interp_Delta / 60.0;  -- 60.0 is frames/sec
            end if;
         end if;

         if Elapsed < Duration (the_Channel.Times (the_Channel.Times'Last)) then
            the_Channel.current_Angle := Reduced (the_Channel.current_Angle + the_Channel.interp_Delta);
            Self.set_rotation_Angle (for_Joint,  for_Axis,
                                     to => to_Radians (math.Degrees (the_Channel.current_Angle)));
         end if;
      end update_rotation_Animation;



      procedure update_location_Animation (for_Channel : in channel_Id;
                                           for_Joint   : in gel.human.scene_joint_Id)
      is
         the_Channel : animation_Channel renames Self.Channels (for_Channel);
         Cursor      : math.Index        renames the_Channel.Cursor;
         Elapsed     : constant Duration          :=      Now - Self.start_Time;

         function site_X return math.Real is begin   return the_Channel.Angles ((Cursor - 1) * 3 + 1);   end site_X;
         function site_Y return math.Real is begin   return the_Channel.Angles ((Cursor - 1) * 3 + 2);   end site_Y;
         function site_Z return math.Real is begin   return the_Channel.Angles ((Cursor - 1) * 3 + 3);   end site_Z;

      begin
         if Cursor < the_Channel.Times'Last then
            if        Cursor = 0
              or else Elapsed > Duration (the_Channel.Times (Cursor))
            then
               Cursor := Cursor + 1;

               if Cursor = 1 then
                  if the_Channel.Times  (Cursor) = 0.0 then
                     the_Channel.site_interp_Delta (1) := site_X - the_Channel.current_Site (1);
                     the_Channel.site_interp_Delta (2) := site_Y - the_Channel.current_Site (2);
                     the_Channel.site_interp_Delta (3) := site_Z - the_Channel.current_Site (3);
                  else
                     the_Channel.site_interp_Delta (1) :=   (site_X - the_Channel.current_Site (1))
                                                          / (the_Channel.Times  (Cursor));
                     the_Channel.site_interp_Delta (2) :=   (site_Y - the_Channel.current_Site (2))
                                                          / (the_Channel.Times  (Cursor));
                     the_Channel.site_interp_Delta (3) :=   (site_Z - the_Channel.current_Site (3))
                                                          / (the_Channel.Times  (Cursor));
                  end if;
               else
                  the_Channel.site_interp_Delta (1) :=   (site_X - the_Channel.current_Site (1))
                                                       / (the_Channel.Times  (Cursor) - the_Channel.Times (Cursor - 1));
                  the_Channel.site_interp_Delta (2) :=   (site_Y - the_Channel.current_Site (2))
                                                       / (the_Channel.Times  (Cursor) - the_Channel.Times (Cursor - 1));
                  the_Channel.site_interp_Delta (3) :=   (site_Z - the_Channel.current_Site (3))
                                                       / (the_Channel.Times  (Cursor) - the_Channel.Times (Cursor - 1));
               end if;

               the_Channel.site_interp_Delta (1) := the_Channel.site_interp_Delta (1) / 60.0;  -- 60.0 is frames/sec
               the_Channel.site_interp_Delta (2) := the_Channel.site_interp_Delta (2) / 60.0;  -- 60.0 is frames/sec
               the_Channel.site_interp_Delta (3) := the_Channel.site_interp_Delta (3) / 60.0;  -- 60.0 is frames/sec
            end if;

            the_Channel.current_Site (1) := the_Channel.current_Site (1) + the_Channel.site_interp_Delta (1);
            the_Channel.current_Site (2) := the_Channel.current_Site (2) + the_Channel.site_interp_Delta (2);
            the_Channel.current_Site (3) := the_Channel.current_Site (3) + the_Channel.site_interp_Delta (3);

            Self.set_Location (for_Joint,  to => the_Channel.current_Site);
         end if;
      end update_location_Animation;

   begin
      Now := world_Age;   -- the_Applet.World.Age;

      if Self.start_Time = 0.0 then
         Self.start_Time := Now;
      end if;

      Elapsed := Now - Self.start_Time;

      update_rotation_Animation (root_x,    gel.human.Root,  gel.human.x_Axis);
      update_rotation_Animation (root_y,    gel.human.Root,  gel.human.y_Axis);
      update_rotation_Animation (root_z,    gel.human.Root,  gel.human.z_Axis);
      update_location_Animation (root_loc,  gel.human.Root);

      update_rotation_Animation (spine_1_x,   gel.human.Spine1,  gel.human.x_Axis);
      update_rotation_Animation (spine_1_y,   gel.human.Spine1,  gel.human.y_Axis);
      update_rotation_Animation (spine_1_z,   gel.human.Spine1,  gel.human.z_Axis);

      update_rotation_Animation (spine_2_x,  gel.human.Spine2,  gel.human.x_Axis);
      update_rotation_Animation (spine_2_y,  gel.human.Spine2,  gel.human.y_Axis);
      update_rotation_Animation (spine_2_z,  gel.human.Spine2,  gel.human.z_Axis);

      update_rotation_Animation (spine_3_x,  gel.human.Spine3,  gel.human.x_Axis);
      update_rotation_Animation (spine_3_y,  gel.human.Spine3,  gel.human.y_Axis);
      update_rotation_Animation (spine_3_z,  gel.human.Spine3,  gel.human.z_Axis);

      update_rotation_Animation (neck_x,  gel.human.Neck,  gel.human.x_Axis);
      update_rotation_Animation (neck_y,  gel.human.Neck,  gel.human.y_Axis);
      update_rotation_Animation (neck_z,  gel.human.Neck,  gel.human.z_Axis);

      update_rotation_Animation (head_x,  gel.human.Head,  gel.human.x_Axis);
      update_rotation_Animation (head_y,  gel.human.Head,  gel.human.y_Axis);
      update_rotation_Animation (head_z,  gel.human.Head,  gel.human.z_Axis);


      update_rotation_Animation (l_clavicle_x,  gel.human.clavicle_L,  gel.human.x_Axis);
      update_rotation_Animation (l_clavicle_y,  gel.human.clavicle_L,  gel.human.y_Axis);
      update_rotation_Animation (l_clavicle_z,  gel.human.clavicle_L,  gel.human.z_Axis);

      update_rotation_Animation (l_uparm_x,  gel.human.upArm_L,  gel.human.x_Axis);
      update_rotation_Animation (l_uparm_y,  gel.human.upArm_L,  gel.human.y_Axis);
      update_rotation_Animation (l_uparm_z,  gel.human.upArm_L,  gel.human.z_Axis);

      update_rotation_Animation (l_loarm_x,  gel.human.loArm_L,  gel.human.x_Axis);
      update_rotation_Animation (l_loarm_y,  gel.human.loArm_L,  gel.human.y_Axis);
      update_rotation_Animation (l_loarm_z,  gel.human.loArm_L,  gel.human.z_Axis);

      --                 update_rotation_Animation (l_hand_x,  gel.human.Hand_L,  gel.human.x_Axis);
      --                 update_rotation_Animation (l_hand_y,  gel.human.Hand_L,  gel.human.y_Axis);
      --                 update_rotation_Animation (l_hand_z,  gel.human.Hand_L,  gel.human.z_Axis);

      --              update_rotation_Animation (l_wrist_x,  gel.human.Wrist_L,  gel.human.x_Axis);
      --              update_rotation_Animation (l_wrist_y,  gel.human.Wrist_L,  gel.human.y_Axis);
      --              update_rotation_Animation (l_wrist_z,  gel.human.Wrist_L,  gel.human.z_Axis);
      --              update_location_Animation (l_wrist_loc, gel.human.Wrist_L);


      update_rotation_Animation (r_clavicle_x,  gel.human.clavicle_R,  gel.human.x_Axis);
      update_rotation_Animation (r_clavicle_y,  gel.human.clavicle_R,  gel.human.y_Axis);
      update_rotation_Animation (r_clavicle_z,  gel.human.clavicle_R,  gel.human.z_Axis);

      update_rotation_Animation (r_uparm_x,  gel.human.upArm_R,  gel.human.x_Axis);
      update_rotation_Animation (r_uparm_y,  gel.human.upArm_R,  gel.human.y_Axis);
      update_rotation_Animation (r_uparm_z,  gel.human.upArm_R,  gel.human.z_Axis);

      update_rotation_Animation (r_loarm_x,  gel.human.loArm_R,  gel.human.x_Axis);
      update_rotation_Animation (r_loarm_y,  gel.human.loArm_R,  gel.human.y_Axis);
      update_rotation_Animation (r_loarm_z,  gel.human.loArm_R,  gel.human.z_Axis);

      update_rotation_Animation (r_hand_x,  gel.human.Hand_R,  gel.human.x_Axis);
      update_rotation_Animation (r_hand_y,  gel.human.Hand_R,  gel.human.y_Axis);
      update_rotation_Animation (r_hand_z,  gel.human.Hand_R,  gel.human.z_Axis);

      --              update_rotation_Animation (l_wrist_x,  gel.human.Wrist_L,  gel.human.x_Axis);
      --              update_rotation_Animation (l_wrist_y,  gel.human.Wrist_L,  gel.human.y_Axis);
      --              update_rotation_Animation (l_wrist_z,  gel.human.Wrist_L,  gel.human.z_Axis);
      --              update_location_Animation (l_wrist_loc, gel.human.Wrist_L);


      update_rotation_Animation (l_upLeg_x,  gel.human.upLeg_L,  gel.human.x_Axis);
      update_rotation_Animation (l_upLeg_y,  gel.human.upLeg_L,  gel.human.y_Axis);
      update_rotation_Animation (l_upLeg_z,  gel.human.upLeg_L,  gel.human.z_Axis);

      update_rotation_Animation (l_loLeg_x,  gel.human.loLeg_L,  gel.human.x_Axis);
      update_rotation_Animation (l_loLeg_y,  gel.human.loLeg_L,  gel.human.y_Axis);
      update_rotation_Animation (l_loLeg_z,  gel.human.loLeg_L,  gel.human.z_Axis);

      update_rotation_Animation (l_Foot_x,  gel.human.Foot_L,  gel.human.x_Axis);
      update_rotation_Animation (l_Foot_y,  gel.human.Foot_L,  gel.human.y_Axis);
      update_rotation_Animation (l_Foot_z,  gel.human.Foot_L,  gel.human.z_Axis);


      update_rotation_Animation (r_upLeg_x,  gel.human.upLeg_R,  gel.human.x_Axis);
      update_rotation_Animation (r_upLeg_y,  gel.human.upLeg_R,  gel.human.y_Axis);
      update_rotation_Animation (r_upLeg_z,  gel.human.upLeg_R,  gel.human.z_Axis);

      update_rotation_Animation (r_loLeg_x,  gel.human.loLeg_R,  gel.human.x_Axis);
      update_rotation_Animation (r_loLeg_y,  gel.human.loLeg_R,  gel.human.y_Axis);
      update_rotation_Animation (r_loLeg_z,  gel.human.loLeg_R,  gel.human.z_Axis);

      update_rotation_Animation (r_Foot_x,  gel.human.Foot_R,  gel.human.x_Axis);
      update_rotation_Animation (r_Foot_y,  gel.human.Foot_R,  gel.human.y_Axis);
      update_rotation_Animation (r_Foot_z,  gel.human.Foot_R,  gel.human.z_Axis);


      Self.update_all_global_Transforms;
   end animate;




   procedure reset_Animation (Self : in out Item)
   is
   begin
      Self.start_Time := 0.0;

      for Each in Self.Channels'Range loop
         Self.Channels (Each).Cursor        := 0;
         Self.Channels (Each).current_Angle := Self.Channels (Each).initial_Angle;
         Self.Channels (Each).current_Site  := Self.Channels (Each).initial_Site;
         Self.Channels (Each).interp_Delta  := 0.0;

      end loop;
   end reset_Animation;





begin

   for Each in to_scene_joint_Id'Range loop
      to_scene_joint_Id (Each) := scene_joint_Id'Value (controller_joint_Id'Image (Each));
   end loop;


   for Each in to_controller_joint_Id'Range loop
      begin
         to_controller_joint_Id (Each) := controller_joint_Id'Value (scene_joint_Id'Image (Each));
      exception
         when constraint_Error =>
            if Each /= Armature then   raise;   end if;
      end;
   end loop;

end gel.Human;
