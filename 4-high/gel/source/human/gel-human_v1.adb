with
     openGL.Model.any,
     openGL.Model.box.colored,
--       openGL.Model.box.lit_colored_textured,
--       gel.cone_twist_Joint,
     gel.Conversions,

     collada.Document,
     collada.Library,
     collada.Library.controllers,
--       collada.Library.visual_scenes,
     collada.Library.animations,

     opengl.Palette,
     opengl.Geometry.lit_textured_skinned,
     opengl.Program.lit_textured_skinned,

     ada.Strings.unbounded,
     ada.unchecked_Deallocation,
     ada.Text_IO;


package body gel.Human_v1
is
   use ada.Text_IO,
       gel.linear_Algebra_3D;

   package std_Physics renames standard.Physics;


   my_Scale   : constant := 1.0;
   model_Name : access String;


   procedure use_Model (Named : in String)
   is
   begin
      if model_Name /= null then
         raise Program_Error with "'gel.human' model name has already been set";
      end if;

      model_Name := new String' (Named);
   end use_Model;


   -----------
   --- Utility
   --

   function "+" (From : in ada.strings.unbounded.unbounded_String) return String
     renames ada.strings.unbounded.to_String;


   function to_joint_Id (From : in String) return scene_joint_Id
   is
      Pad : String := From;
   begin
      if From = "" then
         raise Constraint_Error;
         -- return Armature;
      end if;

      for Each in Pad'Range loop
         if   Pad (Each) = '-'
           or Pad (Each) = '.'
         then
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


   ---------
   --- Forge
   --

   package body Forge
   is

      function new_Human (World         : access gel        .World.item'Class;
                          Model         : access openGL     .Model.item'Class;
                          physics_Model : access std_Physics.Model.item'Class;
                          Mass          : in     math.Real := 0.0;
                          is_Kinematic  : in     Boolean   := False) return Human_v1.view
      is
         Self : constant Human_v1.view := new Human_v1.item;
      begin
         Self.define (World, --Space,
                      Model,
                      physics_Model,
                      Mass,
                      is_Kinematic);
         return Self;
      end new_Human;



      function new_Human (bone_Sprites      : in     human_v1.bone_Sprites;
                          controller_Joints : in     human_types_v1.controller_Joints;
                          Model             : access openGL.Model.item'Class) return Human_v1.view
      is
         the_Human : constant human_v1.View := new human_v1.item;
      begin
         the_Human.bone_Sprites      := bone_Sprites;
         the_Human.controller_Joints := controller_Joints;
         the_Human.Model             := Model;

         return the_Human;
      end new_Human;

   end Forge;



   ---------------------------
   --- Skin Program Parameters
   --

   overriding
   procedure enable (Self : in out skin_program_Parameters)
   is
   begin
      for Each in Self.bone_Transforms'Range
      loop
         openGL.Program.lit_textured_skinned.view (Self.Program)
           .bone_Transform_is (which => controller_joint_Id'Pos (Each) + 1,
                               now   => Self.bone_Transforms (Each));
      end loop;
   end enable;



   procedure set_global_Transform_for (Self      : in out Item'Class;
                                       the_Joint : in     collada.Library.visual_scenes.Node_view)
   is
      use collada.Library, Math;

      which_Joint         : constant scene_joint_Id       := to_joint_Id (+the_Joint.Name);
      child_Joints        : constant visual_scenes.Nodes  := the_Joint.Children;

   begin
      Self.scene_Joints (which_Joint).Transform := (the_Joint.global_Transform);

      declare
         use type gel.Sprite.view;

         the_bone_Id          : bone_Id    := bone_Id'Value (scene_joint_Id'Image (which_Joint));
         the_global_Transform : Matrix_4x4 := the_Joint.global_Transform; -- * to_rotate_Matrix (X_Rotation_from (to_Radians (90.0)));

         the_controller_Joint :  controller_Joint renames Self.controller_Joints (controller_joint_Id (the_Bone_Id));

         Site        : Vector_3;
         Rotation    : Matrix_3x3;

         bone_Offset : Vector_3 := the_controller_Joint.joint_to_bone_site_Offet;
      begin
--           put_Line ("KKK: " & scene_joint_Id'Image (which_Joint));

         if Self.bone_Sprites (the_bone_Id) /= null
         then
            if false -- which_Joint = Hips
            then
               null;
--                 Site := get_Translation (the_global_Transform);
--                 Site := (Site (1), Site (3), -Site (2));         -- Convert from Z-up to Y-up.
--  --                 Site := get_Translation (to_translation_Matrix (  (Site (1), Site (3), -Site (2))));         -- Convert from Z-up to Y-up.
--  --                                                                 * Self.bone_Sprites (Hips).Transform);
--
--                 Rotation := Inverse (get_Rotation (the_global_Transform));
--                 Rotation := (1 => ( Rotation (1, 1),  Rotation (1, 2),  Rotation (1, 3)),                  -- Convert from Z-up to Y-up.
--                              2 => ( Rotation (3, 1),  Rotation (3, 2),  Rotation (3, 3)),                 --
--                              3 => (-Rotation (2, 1), -Rotation (2, 2), -Rotation (2, 3)));                  --
--
--  --                 Rotation :=   Inverse (get_Rotation (Self.bone_Sprites (Hips).Transform))
--  --                             * Rotation;
--
--                 Site := Rotation * Site;
--                 Self.bone_Sprites (the_bone_Id).Site_is (Site);
--  --                 Self.bone_Sprites (the_bone_Id).Spin_is (Rotation);
            else
               Rotation :=  Inverse (get_Rotation (the_global_Transform));
               Rotation := (1 => ( Rotation (1, 1),  Rotation (1, 2),  Rotation (1, 3)),                  -- Convert from Z-up to Y-up.
                            2 => ( Rotation (3, 1),  Rotation (3, 2),  Rotation (3, 3)),                 --
                            3 => (-Rotation (2, 1), -Rotation (2, 2), -Rotation (2, 3)));                  --

--  --                 Rotation :=   Inverse (get_Rotation (Self.bone_Sprites (Hips).Transform))
--                 Rotation :=   Inverse (get_Rotation (Self.skin_Sprite.Transform)) -- animation_Origin))
--                             * Rotation;
--                 Rotation :=   Rotation * get_Rotation (Self.skin_Sprite.Transform);


               Site := get_Translation (the_global_Transform);

               Site := Site - the_controller_Joint.joint_to_bone_site_Offet * (get_Rotation (the_global_Transform));
--                 Site := Site - the_controller_Joint.joint_to_bone_site_Offet * Inverse (Rotation);

--                 Site :=   (Site (1), Site (3), -Site (2))         -- Convert from Z-up to Y-up.
--                         * get_Rotation (Self.animation_Origin);
               Site :=   (Site (1), Site (3), -Site (2))         -- Convert from Z-up to Y-up.
                       * Self.skin_Sprite.Transform; -- animation_Origin);
--                                                                 * Self.bone_Sprites (Hips).Transform);

               bone_Offset := (bone_Offset (1),  bone_Offset (3),  -bone_Offset (2));
               bone_Offset := bone_Offset *  (Rotation);
--                 Site := Site - bone_Offset;
--                 Site := Site + (bone_Offset (1),  bone_Offset (3),  -bone_Offset (2));
--                 Site := Site + (bone_Offset (1),  bone_Offset (2),   bone_Offset (3));


               Self.bone_Sprites (the_bone_Id).Site_is (Site);
               Self.bone_Sprites (the_bone_Id).Spin_is (Rotation);
            end if;
         end if;
      end;

      Self.scene_Joints (which_Joint).Node := the_Joint;                       -- tbd: move this to initialisation.

      for Each in child_Joints'Range
      loop
         set_global_Transform_for (Self, child_Joints (Each));      -- Recurse over children.
      end loop;
   end set_global_Transform_for;



   procedure update_all_global_Transforms (Self : in out Item'Class)
   is
   begin
      set_global_Transform_for (Self, Self.root_Joint);             -- Re-determine all joint transforms, recursively.
--        Self.skin_Sprite.Transform_is (Self.animation_Origin);
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



   procedure set_Transform (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                        To        : in math.Matrix_4x4)
   is
   begin
      Self.scene_Joints (for_Joint).Node.set_Transform (To);
   end set_Transform;



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



   --------------
   --- Human Item
   --
   the_global_Document            : collada.Document.item;
   the_global_Document_is_defined : Boolean := False;

   procedure define (Self : in out Item;   World         : access gel        .World.item'Class;
                                           Model         : access openGL     .Model.item'Class;
                                           physics_Model : access std_Physics.Model.item'Class;
                                           Mass          : in     math.Real := 0.0;
                                           is_Kinematic  : in     Boolean   := True)
   is
      pragma Unreferenced (Mass);

      use collada.Library,
          collada.Library.visual_scenes,
          math.Algebra.linear.d3,
          ada.Strings,
          ada.Strings.unbounded;


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

         for Each in child_Joints'Range
         loop
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
         for Each in Self.controller_Joints'Range
         loop
            Self.controller_Joints (Each).inverse_bind_Matrix
              := Transpose (the_bind_Poses (controller_joint_Id'Pos (Each) + 1));             -- Transpose to correct for collada col major.

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

               set_Translation (Self.controller_Joints (Each).inverse_bind_Matrix,
                                the_Site);
            end;
         end loop;
      end;


      set_global_Transform_for (Self, the_root_Joint);                              -- Determine all joint transforms, recursively.
      set_Site_for             (the_root_Joint,  parent_site => (0.0, 0.0, 0.0));   -- Determine all joint sites.

      Self.Model      := Model; --.all'Unchecked_Access;       -- Remember our model.
      Self.root_Joint := the_root_Joint;                       -- Remember our root joint.

--        the_Model.Scale := (my_Scale, my_Scale, my_Scale);


      --  Define a sprite for each bone.
      --
      declare
         use openGL.Model.box.colored, openGL.Model.box,
             openGL, opengl.Palette,
             math.Vectors;

         use type math.Degrees;

         procedure create_Bone (the_Bone    : in human_Types_v1.bone_Id;
                                start_Joint : in scene_joint_Id;
                                end_Point   : in math.Vector_3;
                                Scale       : in math.Vector_3;
                                Mass        : in math.Real)
         is
            use Math;

            the_bone_Site        : constant math.Vector_3    :=      midPoint (joint_Sites (start_Joint),  end_Point);
            the_controller_Joint :          controller_Joint renames Self.controller_Joints (controller_joint_Id (the_Bone));
            sprite_Name          : constant String           :=      "human.bone_Sprite" & bone_Id'Image (the_Bone);

            function get_Mass return math.Real
            is
            begin
               if is_Kinematic then
                  return 0.0;
               else
                  return Mass;
               end if;
            end get_Mass;


            the_physics_Model : constant standard.physics.Model.view
              := standard.physics.Model.Forge.new_physics_Model (shape_Info => (kind         => std_Physics.Model.Cube,
                                                                                half_extents => Scale / 2.0),
                                                                 Mass       => get_Mass);
         begin
--              if the_Bone = Hips
--              then
--                 declare
--                    the_graphics_Model : constant gel.graphics_Model.box.lit_colored_textured.view
--                      := gel.graphics_Model.Box.lit_colored_textured.Forge.new_Box
--                        (scale => 1.0 * (Scale (1) * my_Scale,   Scale (2) * my_Scale,   Scale (3) * my_Scale), -- Self.Model.Scale,
--                         faces => (front => (colors => (others => (Red,     Opaque)),
--                                             texture_Name => null_Asset,
--                                             texture_object => <>),
--                                   rear  => (colors => (others => (Blue,    Opaque)),
--                                             texture_Name => null_Asset,
--                                             texture_object => <>),
--                                   upper => (colors => (others => (Green,   Opaque)),
--                                             texture_Name => null_Asset,
--                                             texture_object => <>),
--                                   lower => (colors => (others => (Yellow,  Opaque)),
--                                             texture_Name => null_Asset,
--                                             texture_object => <>),
--                                   left  => (colors => (others => (Cyan,    Opaque)),
--                                             texture_Name => null_Asset,
--                                             texture_object => <>),
--                                   right => (colors => (others => (Magenta, Opaque)),
--                                             texture_Name => null_Asset,
--                                             texture_object => <>)));
--
--  --                    the_human_graphics_Model : aliased gel.graphics_Model.open_gl.view
--  --                      := gel.graphics_Model.open_gl.forge.new_Model (scale   => (1.0, 1.0, 1.0),
--  --                                                                     --                                                     model   => gel.to_Asset ("assets/gel/model/gel-human.dae"),
--  --                                                                     model   => to_Asset (model_Name.all),  --gel.to_Asset ("assets/gel/collada/mh-human-dae.dae"),
--  --                                                                     --                                                     model   => gel.to_Asset ("assets/gel/collada/alfieri.dae"),
--  --                                                                     texture => gel.null_Asset, -- gel.to_Asset ("assets/collada/gel-human-texture.tga"),
--  --                                                                     Texture_is_lucid => False);
--                 begin
--  --                    if   the_display_Mode = Skin
--  --                      or the_display_Mode = Skin_and_Bones
--  --                    then
--  --                       Self.bone_Sprites (the_Bone) := gel.Sprite.forge.new_Sprite (sprite_Name,
--  --                                                                                    World,
--  --                                                                                    the_human_graphics_Model,
--  --  --                                                                                 Model,
--  --                                                                                    the_physics_Model,
--  --                                                                                    owns_graphics => True,
--  --                                                                                    owns_physics  => True,
--  --                                                                                    is_kinematic  => is_Kinematic);
--  --                    else
--                       Self.bone_Sprites (the_Bone) := gel.Sprite.forge.new_Sprite (sprite_Name,
--                                                                                    World,
--                                                                                    the_graphics_Model,
--                                                                                    the_physics_Model,
--                                                                                    owns_graphics => True,
--                                                                                    owns_physics  => True,
--                                                                                    is_kinematic  => is_Kinematic);
--
--  --                    end if;
--                 end;
--              else
               declare
                  the_graphics_Model : constant openGL.Model.box.colored.view
                    := openGL.Model.Box.colored.new_Box
                      (Size => (Scale (1) * my_Scale,   Scale (2) * my_Scale,   Scale (3) * my_Scale),
                       Faces => (front => (colors => (others => (Red,     Opaque))),
                                 rear  => (colors => (others => (Blue,    Opaque))),
                                 upper => (colors => (others => (Green,   Opaque))),
                                 lower => (colors => (others => (Yellow,  Opaque))),
                                 left  => (colors => (others => (Cyan,    Opaque))),
                                 right => (colors => (others => (Magenta, Opaque)))));
               begin
                  Self.bone_Sprites (the_Bone) := gel.Sprite.forge.new_Sprite (sprite_Name,
                                                                               gel.sprite.World_view (World),
                                                                               Origin_3D,
                                                                               the_graphics_Model,
                                                                               the_physics_Model,
                                                                               owns_graphics => True,
                                                                               owns_physics  => True,
                                                                               is_kinematic  => is_Kinematic);
                  if the_display_Mode = Skin
                  then
                     Self.bone_Sprites (the_Bone).is_Visible (False);
                  end if;
               end;
--              end if;

--              the_bone_Site := the_bone_Site * my_Scale;

            Self.bone_Sprites (the_Bone).Site_is (the_bone_Site);

--              if the_Bone = Hips
--              then
--                 Self.bone_Sprites (the_Bone).Spin_is (get_Rotation (the_controller_Joint.inverse_bind_Matrix)); -- * x_Rotation_from (to_Radians (180.0)));
--              else
            Self.bone_Sprites (the_Bone).Spin_is (get_Rotation (the_controller_Joint.inverse_bind_Matrix));
--              end if;

            the_controller_Joint.joint_to_bone_site_Offet
              :=   Inverse (get_Rotation (the_controller_Joint.inverse_bind_Matrix))
                 * (joint_Sites (start_Joint) - the_bone_Site);
         end create_Bone;


         procedure attach_via_Ball (bone_A_Id,
                                    bone_B_Id   : in Bone_Id;
                                    pitch_limits,
                                    yaw_limits,
                                    roll_Limits : in gel.Sprite.dof_limits := (math.to_Radians (-0.0),
                                                                               math.to_Radians ( 0.0))) -- -20.0 .. 20.0
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


         procedure attach_via_Hinge (bone_A_Id,
                                     bone_B_Id : in Bone_Id;
                                     Limits    : in gel.Sprite.dof_limits := (math.to_Radians ( 0.0),
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
                                                            Frame_in_parent   => Frame_A,
                                                            Frame_in_child    => Frame_B,
                                                            Limits            => Limits,
                                                            collide_Connected => False,
                                                            new_Joint         => Self.Joints (joint_Id));
         end attach_via_Hinge;

         use Math;


         bone_Extent : math.Real;

      begin
         --  Skin
         --
         declare
            the_human_graphics_Model : aliased openGL.Model.any.view
              := openGL.Model.any.new_Model (Model   => to_Asset (model_Name.all),
                                             Texture => openGL.null_Asset, -- gel.to_Asset ("assets/collada/gel-human-texture.tga"),
                                             Texture_is_lucid => False);

            the_physics_Model : constant standard.physics.Model.view
              := standard.physics.Model.Forge.new_physics_Model (shape_Info  => (Kind         => standard.physics.Model.Cube,
                                                                                 half_Extents => (0.1, 0.1, 0.1)),
                                                                 Mass        => 1.0,
                                                                 is_Tangible => False);
         begin
            Self.skin_Sprite := gel.Sprite.forge.new_Sprite ("human.skin_Sprite",
                                                             gel.sprite.World_view (World),
                                                             Origin_3D,
                                                             the_human_graphics_Model,
                                                             the_physics_Model,
                                                             owns_graphics => True,
                                                             owns_physics  => True,
                                                             is_kinematic  => is_Kinematic);
            if the_display_Mode = Bones
            then
               Self.skin_Sprite.is_Visible (False);
            end if;
         end;


         --  Hips
         --
         bone_Extent := 0.25 * Distance (joint_Sites (Hips), to => joint_Sites (Spine));
         create_Bone (Hips,
                      Hips,
                      joint_Sites (Spine),
                      bone_Extent * (1.3, 0.6, 0.6),
                      Mass => 1.0);


         --  Thigh_L
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Thigh_L), to => joint_Sites (Shin_L));
         create_Bone (Thigh_L,  Thigh_L, joint_Sites (Shin_L),  (0.25, 0.5, 0.25) * bone_Extent,  0.5);

         attach_via_Ball (bone_A_Id    => Hips,
                          bone_B_Id    => Thigh_L,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_Limits  => (-0.5, 0.5));


         --  Shin_L
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Shin_L), to => joint_Sites (Foot_L));
         create_Bone (Shin_L,  Shin_L, joint_Sites (Foot_L),  (0.2, 0.8, 0.2) * bone_Extent,  0.5);

         attach_via_Ball (bone_A_Id    => Thigh_L,
                          bone_B_Id    => Shin_L,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_Limits  => (-0.5, 0.5));


         --  Foot_L
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Foot_L), to => joint_Sites (Toe_L));
         create_Bone (Foot_L,  Foot_L, joint_Sites (Toe_L),  (0.4, 0.8, 0.2) * bone_Extent,  0.5);

         attach_via_Ball (bone_A_Id    => Shin_L,
                          bone_B_Id    => Foot_L,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_Limits  => (-0.5, 0.5));


         --  Toe_L
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Toe_L), to => joint_Sites (Foot_L));
         create_Bone (Toe_L,  Toe_L, joint_Sites (Toe_L) + (0.0, -0.05, 0.0),  (0.6, 0.3, 0.1) * bone_Extent,  0.5);

         attach_via_Ball (bone_A_Id    => Foot_L,
                          bone_B_Id    => Toe_L,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_Limits  => (-0.5, 0.5));


         --  Thigh_R
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Thigh_R), to => joint_Sites (Shin_R));
         create_Bone (Thigh_R,  Thigh_R, joint_Sites (Shin_R),  (0.25, 0.5, 0.25) * bone_Extent,  0.5);

         attach_via_Ball (bone_A_Id    => Hips,
                          bone_B_Id    => Thigh_R,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_Limits  => (-0.5, 0.5));


         --  Shin_R
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Shin_R), to => joint_Sites (Foot_R));
         create_Bone (Shin_R,  Shin_R, joint_Sites (Foot_R),  (0.2, 0.8, 0.2) * bone_Extent,  0.5);

         attach_via_Ball (bone_A_Id    => Thigh_R,
                          bone_B_Id    => Shin_R,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_Limits  => (-0.5, 0.5));


         --  Foot_R
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Foot_R), to => joint_Sites (Toe_R));
         create_Bone (Foot_R,  Foot_R, joint_Sites (Toe_R),  (0.4, 0.8, 0.2) * bone_Extent,  0.5);

         attach_via_Ball (bone_A_Id    => Shin_R,
                          bone_B_Id    => Foot_R,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_Limits  => (-0.5, 0.5));


         --  Toe_R
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Toe_R), to => joint_Sites (Foot_R));
         create_Bone (Toe_R,  Toe_R, joint_Sites (Toe_R) + (0.0, -0.05, 0.0),  (0.6, 0.3, 0.1) * bone_Extent,  0.5);

         attach_via_Ball (bone_A_Id    => Foot_R,
                          bone_B_Id    => Toe_R,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_Limits  => (-0.5, 0.5));


         --  Spine
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Spine), to => joint_Sites (Chest));
         create_Bone (Spine,  Spine, joint_Sites (Chest),  (1.4, 0.6, 0.9) * bone_Extent,  0.5);

         attach_via_Ball (bone_A_Id    => Hips,
                          bone_B_Id    => Spine,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_Limits  => (-0.5, 0.5));


         --  Chest
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Chest), to => joint_Sites (Neck));
         create_Bone (Chest,  Chest, joint_Sites (Neck),  (0.8, 0.2, 0.20) * bone_Extent,  0.5); -- 0.6 * 0.5);

         attach_via_Ball (bone_A_Id    => Spine,
                          bone_B_Id    => Chest,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_Limits  => (-0.5, 0.5));


         --- Right Arm
         --

         --  Right Clavicle
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Clavicle_R), to => joint_Sites (upper_Arm_R));
         create_Bone (Clavicle_R,  Clavicle_R, joint_Sites (upper_Arm_R),  (0.25, 0.25, 0.25) * bone_Extent,  0.5);

         attach_via_Ball (Chest,   Clavicle_R,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_limits  => (-0.5, 0.5));


         --  Right Upper Arm
         --
         bone_Extent := 1.0 * Distance (joint_Sites (upper_Arm_R), to => joint_Sites (Forearm_R));
         create_Bone (upper_Arm_R,  upper_Arm_R, joint_Sites (Forearm_R),  (0.2, 0.7, 0.2) * bone_Extent,  0.5);

         attach_via_Ball (Clavicle_R,   upper_Arm_R,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_limits  => (-0.5, 0.5));


         --  Right Forearm
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Forearm_R), to => joint_Sites (Hand_R));
         create_Bone (Forearm_R,  Forearm_R, joint_Sites (Hand_R),  (0.2, 0.8, 0.2) * bone_Extent,  0.5);

         attach_via_Ball (upper_Arm_R,   Forearm_R,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_limits  => (-0.5, 0.5));


         --  Right Hand
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Hand_R), to => joint_Sites (Thumb_02_R));
         create_Bone (Hand_R,  Hand_R,  joint_Sites (Thumb_02_R) - (0.0, 0.0, 0.0),  (0.8, 0.9, 0.2) * bone_Extent,  0.5);

         attach_via_Ball (Forearm_R,   Hand_R,
                          pitch_limits => (-0.5, 0.0),
                          yaw_limits   => (-0.5, 0.0),
                          roll_limits  => (-0.0, 0.0));

--           --  Right Thumb_02
--           --
--           bone_Extent := 1.0 * Distance (joint_Sites (Thumb_02_R), to => joint_Sites (Hand_R));
--           create_Bone (Thumb_02_R,  Thumb_02_R,  joint_Sites (Hand_R) - (0.5, 0.0, 0.0),  (0.4, 0.15, 0.08) * bone_Extent,  0.5);
--
--           attach_via_Ball (Hand_R,   Thumb_02_R,
--                            pitch_limits => (-0.5, 0.0),
--                            yaw_limits   => (-0.5, 0.0),
--                            roll_limits  => (-0.0, 0.0));
--
--           --  Right Thumb_03
--           --
--           bone_Extent := 1.0 * Distance (joint_Sites (Thumb_03_R), to => joint_Sites (Hand_R));
--           create_Bone (Thumb_03_R,  Thumb_03_R,  joint_Sites (Hand_R) - (0.5, 0.0, 0.0),  (0.4, 0.15, 0.08) * bone_Extent,  0.5);
--
--           attach_via_Ball (Hand_R,   Thumb_03_R,
--                            pitch_limits => (-0.5, 0.0),
--                            yaw_limits   => (-0.5, 0.0),
--                            roll_limits  => (-0.0, 0.0));
--
--           --  Right Index Finger
--           --
--           bone_Extent := 1.0 * Distance (joint_Sites (F_ring_01_R), to => joint_Sites (Hand_R));
--           create_Bone (F_ring_01_R,  F_ring_01_R,  joint_Sites (Hand_R) - (0.5, 0.0, 0.0),  (0.4, 0.15, 0.08) * bone_Extent,  0.5);
--
--           attach_via_Ball (Hand_R,   F_ring_01_R,
--                            pitch_limits => (-0.5, 0.0),
--                            yaw_limits   => (-0.5, 0.0),
--                            roll_limits  => (-0.0, 0.0));
--
--           --  Right Ring Finger
--           --
--           bone_Extent := 1.0 * Distance (joint_Sites (F_index_01_R), to => joint_Sites (Hand_R));
--           create_Bone (F_index_01_R,  F_index_01_R,  joint_Sites (Hand_R) - (0.5, 0.0, 0.0),  (0.4, 0.15, 0.08) * bone_Extent,  0.5);
--
--           attach_via_Ball (Hand_R,   F_index_01_R,
--                            pitch_limits => (-0.5, 0.0),
--                            yaw_limits   => (-0.5, 0.0),
--                            roll_limits  => (-0.0, 0.0));


         --- Left Arm
         --

         --  Left Clavicle
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Clavicle_L), to => joint_Sites (upper_Arm_L));
         create_Bone (Clavicle_L,  Clavicle_L, joint_Sites (upper_Arm_L),  (0.25, 0.25, 0.25) * bone_Extent,  0.5);

         attach_via_Ball (Chest,   Clavicle_L,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_limits  => (-0.5, 0.5));


         --  Left Upper Arm
         --
         bone_Extent := 1.0 * Distance (joint_Sites (upper_Arm_L), to => joint_Sites (Forearm_L));
         create_Bone (upper_Arm_L,  upper_Arm_L, joint_Sites (Forearm_L),  (0.2, 0.7, 0.2) * bone_Extent,  0.5);

         attach_via_Ball (Clavicle_L,   upper_Arm_L,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_limits  => (-0.5, 0.5));


         --  Left Forearm
         --
         bone_Extent := 0.75 * Distance (joint_Sites (Forearm_L), to => joint_Sites (Hand_L));
         create_Bone (Forearm_L,  Forearm_L, joint_Sites (Hand_L),  (0.2, 0.8, 0.2) * bone_Extent,  0.5);

         attach_via_Ball (upper_Arm_L,   Forearm_L,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_limits  => (-0.5, 0.5));


         --  Left Hand
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Hand_L), to => joint_Sites (Thumb_02_L));
         create_Bone (Hand_L,  Hand_L,  joint_Sites (Thumb_02_L) - (0.0, 0.0, 0.0),  (0.8, 0.9, 0.2) * bone_Extent,  0.5);

         attach_via_Ball (Forearm_L,   Hand_L,
                          pitch_limits => (-0.5, 0.0),
                          yaw_limits   => (-0.5, 0.0),
                          roll_limits  => (-0.0, 0.0));


--           --  Left Thumb_02
--           --
--           bone_Extent := 1.0 * Distance (joint_Sites (Thumb_02_L), to => joint_Sites (Hand_L));
--           create_Bone (Thumb_02_L,  Thumb_02_L,  joint_Sites (Hand_L) + (0.5, 0.0, 0.0),  (0.4, 0.15, 0.08) * bone_Extent,  0.5);
--
--           attach_via_Ball (Hand_L,   Thumb_02_L,
--                            pitch_limits => (-0.5, 0.0),
--                            yaw_limits   => (-0.5, 0.0),
--                            roll_limits  => (-0.0, 0.0));
--
--           --  Left Thumb_03
--           --
--           bone_Extent := 1.0 * Distance (joint_Sites (Thumb_03_L), to => joint_Sites (Hand_L));
--           create_Bone (Thumb_03_L,  Thumb_03_L,  joint_Sites (Hand_L) + (0.5, 0.0, 0.0),  (0.4, 0.15, 0.08) * bone_Extent,  0.5);
--
--           attach_via_Ball (Hand_L,   Thumb_03_L,
--                            pitch_limits => (-0.5, 0.0),
--                            yaw_limits   => (-0.5, 0.0),
--                            roll_limits  => (-0.0, 0.0));
--
--           --  Left Index Finger
--           --
--           bone_Extent := 1.0 * Distance (joint_Sites (F_ring_01_L), to => joint_Sites (Hand_L));
--           create_Bone (F_ring_01_L,  F_ring_01_L,  joint_Sites (Hand_L) + (0.5, 0.0, 0.0),  (0.4, 0.15, 0.08) * bone_Extent,  0.5);
--
--           attach_via_Ball (Hand_L,   F_ring_01_L,
--                            pitch_limits => (-0.5, 0.0),
--                            yaw_limits   => (-0.5, 0.0),
--                            roll_limits  => (-0.0, 0.0));
--
--           --  Left Ring Finger
--           --
--           bone_Extent := 1.0 * Distance (joint_Sites (F_index_01_L), to => joint_Sites (Hand_L));
--           create_Bone (F_index_01_L,  F_index_01_L,  joint_Sites (Hand_L) + (0.5, 0.0, 0.0),  (0.4, 0.15, 0.08) * bone_Extent,  0.5);
--
--           attach_via_Ball (Hand_L,   F_index_01_L,
--                            pitch_limits => (-0.5, 0.0),
--                            yaw_limits   => (-0.5, 0.0),
--                            roll_limits  => (-0.0, 0.0));


         --  Neck
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Neck), to => joint_Sites (Head));
         create_Bone (Neck,  Neck, joint_Sites (Head),  (0.4, 0.6, 0.4) * bone_Extent,  0.4); -- 0.4 * 0.5);

         attach_via_Ball (bone_A_Id    => Chest,
                          bone_B_Id    => Neck,
                          pitch_limits => (-0.4, 0.3),
                          yaw_limits   => (-0.3, 0.3),
                          roll_Limits  => (-0.2, 0.2));


         --  Head
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Head), to => joint_Sites (Neck));
         create_Bone (Head,  Head, joint_Sites (Head) + (0.0, 0.0, 0.05),  (0.8, 0.6, 0.7) * bone_Extent,  0.25);

         attach_via_Ball (bone_A_Id    => Neck,
                          bone_B_Id    => Head,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_Limits  => (-0.5, 0.5));

         --  Jaw
         --
         bone_Extent := 1.0 * Distance (joint_Sites (Jaw), to => joint_Sites (Head));
         create_Bone (Jaw,  Jaw, joint_Sites (Jaw) + (0.0, -0.07, 0.03),  (0.9, 1.0, 0.3) * bone_Extent,  0.25);

         attach_via_Ball (bone_A_Id    => Head,
                          bone_B_Id    => Jaw,
                          pitch_limits => (-0.5, 0.5),
                          yaw_limits   => (-0.5, 0.5),
                          roll_Limits  => (-0.5, 0.5));

--           --  Eye_R
--           --
--           bone_Extent := 0.1 * Distance (joint_Sites (Eye_R), to => joint_Sites (Head));
--           create_Bone (Eye_R,  Eye_R, joint_Sites (Eye_R) + (0.0, 0.0, 0.0),  (1.0, 1.4, 0.7) * bone_Extent,  0.25);
--
--           attach_via_Ball (Head, Eye_R);
--
--           --  Eye_L
--           --
--           bone_Extent := 0.1 * Distance (joint_Sites (Eye_L), to => joint_Sites (Head));
--           create_Bone (Eye_L,  Eye_L, joint_Sites (Eye_L) + (0.0, 0.0, 0.0),  (1.0, 1.4, 0.7) * bone_Extent,  0.25);
--
--           attach_via_Ball (Head, Eye_L);
      end;


      --- Parse the Collada animations file.
      --
      declare
         use collada.Library.animations;
         the_Animations  : constant access animations.Animation_array := the_Document.Libraries.Animations.Contents;
      begin
         if the_Animations /= null
         then
            for Each in the_Animations'Range
            loop
               declare
                  the_Animation   : constant animations.Animation       := the_Animations (Each);
                  the_Inputs      : access   collada.float_Array        := Inputs_of (the_Animation);

                  procedure common_setup (Channel : channel_Id;   scene_Joint : scene_Joint_Id;   Sid : in String)
                  is
                  begin
                     Self.Channels (Channel).Target        := Self.scene_Joints (scene_Joint).Node.fetch_Transform (Sid);
                     Self.Channels (Channel).Times         := Inputs_of  (the_Animation);
                     Self.Channels (Channel).Values        := Outputs_of (the_Animation);

                     for Each in Self.Channels (Channel).Times'Range
                     loop
                        Self.Channels (Channel).Times (Each) := Self.Channels (Channel).Times (Each) / 1.0;   -- ???
                     end loop;
                  end common_setup;

                  procedure setup_Rotation (Channel : channel_Id;   scene_Joint : scene_Joint_Id;   Sid : in String)
                  is
                  begin
                     common_setup (Channel, scene_Joint, Sid);

                     -- For angle interpolation during 'rotation' animation.
                     --
                     Self.Channels (Channel).initial_Angle := Self.Channels (Channel).Values (1);
                     Self.Channels (Channel).current_Angle := Self.Channels (Channel).initial_Angle;
                  end setup_Rotation;

                  procedure setup_Location (Channel : channel_Id;   scene_Joint : scene_Joint_Id;   Sid : in String)
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

                  procedure setup_full_Transform (Channel : channel_Id;   scene_Joint : scene_Joint_Id;   Sid : in String)
                  is
                  begin
                     common_setup (Channel, scene_Joint, Sid);

                     -- For matrix interpolation during 'full_transform' animation.
                     --
                     Self.Channels (Channel).Transforms := new Transforms (1 .. Collada.matrix_Count (Self.Channels (Channel).Values.all));

                     for i in Self.Channels (Channel).Transforms'Range
                     loop
                        declare
                           the_Matrix : math.Matrix_4x4 := math.Transpose (Collada.get_Matrix (Self.Channels (Channel).Values.all, which => i));
                        begin
                           Self.Channels (Channel).Transforms (i) := (rotation    => to_Quaternion (get_Rotation    (the_Matrix)),
                                                                      translation =>                get_Translation (the_Matrix));
                        end;

                     end loop;

                     Self.Channels (Channel).initial_Transform := Self.Channels (Channel).Transforms (1);
                     Self.Channels (Channel).current_Transform := Self.Channels (Channel).initial_Transform;

                     Self.Channels (Channel).current_Site  := Self.Channels (Channel).initial_Transform.Translation;
                     Self.Channels (Channel).initial_Site  := Self.Channels (Channel).current_Site;
                  end setup_full_Transform;

               begin
                  if    Index (the_Animation.Channel.Target, "hips/transform") /= 0 then
                     setup_full_Transform (Hips, Hips, "transform");

                  elsif Index (the_Animation.Channel.Target, "thigh_L/transform") /= 0 then
                     setup_full_Transform (Thigh_L, Thigh_L, "transform");

                  elsif Index (the_Animation.Channel.Target, "shin_L/transform") /= 0 then
                     setup_full_Transform (Shin_L, Shin_L, "transform");

                  elsif Index (the_Animation.Channel.Target, "foot_L/transform") /= 0 then
                     setup_full_Transform (Foot_L, Foot_L, "transform");

                  elsif Index (the_Animation.Channel.Target, "toe_L/transform") /= 0 then
                     setup_full_Transform (Toe_L, Toe_L, "transform");

                  elsif Index (the_Animation.Channel.Target, "thigh_R/transform") /= 0 then
                     setup_full_Transform (Thigh_R, Thigh_R, "transform");

                  elsif Index (the_Animation.Channel.Target, "shin_R/transform") /= 0 then
                     setup_full_Transform (Shin_R, Shin_R, "transform");

                  elsif Index (the_Animation.Channel.Target, "foot_R/transform") /= 0 then
                     setup_full_Transform (Foot_R, Foot_R, "transform");

                  elsif Index (the_Animation.Channel.Target, "toe_R/transform") /= 0 then
                     setup_full_Transform (Toe_R, Toe_R, "transform");

                  elsif Index (the_Animation.Channel.Target, "spine/transform") /= 0 then
                     setup_full_Transform (Spine, Spine, "transform");

                  elsif Index (the_Animation.Channel.Target, "chest/transform") /= 0 then
                     setup_full_Transform (Chest, Chest, "transform");


                  elsif Index (the_Animation.Channel.Target, "clavicle_R/transform") /= 0 then
                     setup_full_Transform (Clavicle_R, Clavicle_R, "transform");

                  elsif Index (the_Animation.Channel.Target, "upper_arm_R/transform") /= 0 then
                     setup_full_Transform (upper_Arm_R, upper_Arm_R, "transform");

                  elsif Index (the_Animation.Channel.Target, "forearm_R/transform") /= 0 then
                     setup_full_Transform (Forearm_R, Forearm_R, "transform");

                  elsif Index (the_Animation.Channel.Target, "hand_R/transform") /= 0 then
                     setup_full_Transform (Hand_R, Hand_R, "transform");

                  elsif Index (the_Animation.Channel.Target, "thumb_02_R/transform") /= 0 then
                     setup_full_Transform (Thumb_02_R, Thumb_02_R, "transform");

                  elsif Index (the_Animation.Channel.Target, "thumb_03_R/transform") /= 0 then
                     setup_full_Transform (Thumb_03_R, Thumb_03_R, "transform");

                  elsif Index (the_Animation.Channel.Target, "f_ring_01_R/transform") /= 0 then
                     setup_full_Transform (F_ring_01_R, F_ring_01_R, "transform");

                  elsif Index (the_Animation.Channel.Target, "f_index_01_R/transform") /= 0 then
                     setup_full_Transform (F_index_01_R, F_index_01_R, "transform");


                  elsif Index (the_Animation.Channel.Target, "clavicle_L/transform") /= 0 then
                     setup_full_Transform (Clavicle_L, Clavicle_L, "transform");

                  elsif Index (the_Animation.Channel.Target, "upper_arm_L/transform") /= 0 then
                     setup_full_Transform (upper_Arm_L, upper_Arm_L, "transform");

                  elsif Index (the_Animation.Channel.Target, "forearm_L/transform") /= 0 then
                     setup_full_Transform (Forearm_L, Forearm_L, "transform");

                  elsif Index (the_Animation.Channel.Target, "hand_L/transform") /= 0 then
                     setup_full_Transform (Hand_L, Hand_L, "transform");

                  elsif Index (the_Animation.Channel.Target, "thumb_02_L/transform") /= 0 then
                     setup_full_Transform (Thumb_02_L, Thumb_02_L, "transform");

                  elsif Index (the_Animation.Channel.Target, "thumb_03_L/transform") /= 0 then
                     setup_full_Transform (Thumb_03_L, Thumb_03_L, "transform");

                  elsif Index (the_Animation.Channel.Target, "f_ring_01_L/transform") /= 0 then
                     setup_full_Transform (F_ring_01_L, F_ring_01_L, "transform");

                  elsif Index (the_Animation.Channel.Target, "f_index_01_L/transform") /= 0 then
                     setup_full_Transform (F_index_01_L, F_index_01_L, "transform");


                  elsif Index (the_Animation.Channel.Target, "neck/transform") /= 0 then
                     setup_full_Transform (Neck, Neck, "transform");

                  elsif Index (the_Animation.Channel.Target, "head/transform") /= 0 then
                     setup_full_Transform (Head, Head, "transform");

                  elsif Index (the_Animation.Channel.Target, "jaw/transform") /= 0 then
                     setup_full_Transform (Jaw, Jaw, "transform");

                  elsif Index (the_Animation.Channel.Target, "eye_R/transform") /= 0 then
                     setup_full_Transform (Eye_R, Eye_R, "transform");

                  elsif Index (the_Animation.Channel.Target, "eye_L/transform") /= 0 then
                     setup_full_Transform (Eye_L, Eye_L, "transform");
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
   begin
      if the_display_Mode /= Bones
      then
         Self.program_Parameters.Program_is (opengl.Program.view (opengl.Geometry.lit_textured_skinned.Program));
         Self.skin_Sprite.program_Parameters_are (Self.program_Parameters'Unchecked_Access);
--           Self.base_Sprite.program_Parameters_are (Self.program_Parameters'Unchecked_Access);
      end if;
   end enable_Graphics;



   function  controller_Joints (Self : in Item'Class) return human_types_v1.controller_Joints
   is
   begin
      return Self.controller_Joints;
   end controller_Joints;



   procedure controller_Joints_are (Self : in out Item'Class;   Now : in human_types_v1.controller_Joints)
   is
   begin
      Self.controller_Joints := Now;
   end controller_Joints_are;


   --------------
   --- Attributes
   --

   function Sprite (Self : in Item'Class;   for_Bone : in bone_Id) return gel.Sprite.view
   is
   begin
      return Self.bone_Sprites (for_Bone);
   end Sprite;



   function base_Sprite (Self : in Item'Class) return gel.Sprite.view
   is
   begin
      return Self.bone_Sprites (Hips);
   end base_Sprite;



   function skin_Sprite (Self : in     Item'Class) return gel.Sprite.view
   is
   begin
      return Self.skin_Sprite;
   end skin_Sprite;



   procedure set_GL_program_Parameters (Self : in out Item'Class;   for_Bone : in controller_joint_Id;
                                                        To       : in math.Matrix_4x4)
   is
      use gel.Conversions;
   begin
      Self.program_Parameters.bone_Transforms (for_Bone) := to_GL (To);
   end set_GL_program_Parameters;


   --------------
   --- Operations
   --

   procedure evolve (Self : in out Item'Class;   world_Age : in Duration)
   is
      use Math, math.Vectors;


      function get_root_Transform return math.Matrix_4x4
      is
      begin
         if Self.Mode = Physics
         then
            put_Line ("Self.base_Sprite.Transform: ");
            put_Line (math.Image (math.Matrix (Self.base_Sprite.Transform)));
            return Self.base_Sprite.Transform;
--              return math.Identity_4x4;
--              return to_transform_Matrix (Rotation => x_Rotation_from (the_Angle => to_Radians (-90.0)),
--                                          Translation => (0.0, 0.0, 0.0));

         else -- Is an animation.
--              return  (Self.skin_Sprite.Transform);
--              return Inverse (Self.animation_Origin);

            declare
               the_Transform : math.Matrix_4x4 := math.Identity_4x4;
            begin
               set_Rotation    (the_Transform,  x_Rotation_from (to_Radians (-90.0)));    -- Convert from Z-up to Y-up.
--                 set_Rotation    (the_Transform,  Inverse (get_Rotation ( (Self.controller_Joints (Hips).inverse_bind_Matrix))));
               set_Translation (the_Transform,
--                                  -get_Translation (Inverse (Self.animation_Origin)));
                                -get_Translation (Inverse (Self.controller_Joints (Hips).inverse_bind_Matrix)));
               return the_Transform;
            end;
         end if;
      end get_root_Transform;


      root_Transform     : constant math.Matrix_4x4 := get_root_Transform;
      inv_root_Transform : constant math.Matrix_4x4 := Inverse (root_Transform);


      function joint_Transform_for (the_collada_Joint : in controller_joint_Id) return math.Matrix_4x4
      is
      begin
--           put_Line ("KKKKKKKK: '" & controller_joint_Id'Image (the_collada_Joint) & "'");

         if Self.Mode = Physics
         then
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

         else   -- Must be animation mode.
            return Self.scene_Joints (to_scene_joint_Id (the_collada_Joint)).Transform;
         end if;
      end joint_Transform_for;



      procedure set_Transform_for (the_Bone : in controller_joint_Id)
      is
      begin
--           new_Line;
--           put_Line ("JOINT_TRANS for Bone:" & controller_joint_Id'Image (the_Bone));
--           put_Line (math.Image (math.Matrix (joint_Transform_for (the_Bone))));
--
--           new_Line;
--           put_Line ("Self.controller_Joints (the_Bone).inverse_bind_Matrix:");
--           put_Line (math.Image (math.Matrix (Self.controller_Joints (the_Bone).inverse_bind_Matrix)));

         Self.set_GL_program_Parameters (for_bone => the_Bone,
                                         to       =>   Self.controller_Joints (the_Bone).inverse_bind_Matrix
                                                     * joint_Transform_for (the_Bone)
                                                     * inv_root_Transform);
      end set_Transform_for;



      procedure set_proxy_Transform_for (the_Bone : in controller_joint_Id;   the_Proxy : in controller_joint_Id)
      is
      begin
         Self.set_GL_program_Parameters (for_bone => the_Bone,
                                         to       =>   Self.controller_Joints (the_Proxy).inverse_bind_Matrix
                                                     * joint_Transform_for (the_Proxy)
                                                     * inv_root_Transform);
      end set_proxy_Transform_for;


   begin
      if not Self.Graphics_enabled
      then
         Self.enable_Graphics;
         Self.Graphics_enabled := True;
      end if;

      if Self.Mode = Animation
      then
         Self.animate (world_Age);
      else
--  --           Self.skin_Sprite.Transform_is (Self.base_Sprite.Transform);
         Self.skin_Sprite.rotate (Self.base_Sprite.Spin);
         Self.skin_Sprite.move   (Self.base_Sprite.Site);
         null;
      end if;


      -- tbd: Can probly do the below 'set_Transform_for' calls in a loop.

      --  Torso
      --
      set_Transform_for (Hips);
      --  set_Transform_for (Spine);
      --  set_Transform_for (Chest);
      --  set_Transform_for (Neck);
      --
      --  set_Transform_for       (Head);
      --  set_Transform_for       (Jaw);
      --  set_proxy_Transform_for (Eye_R, Head);
      --  set_proxy_Transform_for (Eye_L, Head);
      --
      --
      --  --  Left Arm
      --  --
      --  set_Transform_for       (Clavicle_L);
      --  set_Transform_for       (upper_Arm_L);
      --  set_Transform_for       (Forearm_L);
      --  set_Transform_for       (Hand_L);
      --  set_proxy_Transform_for (Thumb_02_L,   Hand_L);
      --  set_proxy_Transform_for (Thumb_03_L,   Hand_L);
      --  set_proxy_Transform_for (F_ring_01_L,  Hand_L);
      --  set_proxy_Transform_for (F_index_01_L, Hand_L);
      --
      --
      --  --  Right Arm
      --  --
      --  set_Transform_for       (Clavicle_R);
      --  set_Transform_for       (upper_Arm_R);
      --  set_Transform_for       (Forearm_R);
      --  set_Transform_for       (Hand_R);
      --  set_proxy_Transform_for (Thumb_02_R,   Hand_R);
      --  set_proxy_Transform_for (Thumb_03_R,   Hand_R);
      --  set_proxy_Transform_for (F_ring_01_R,  Hand_R);
      --  set_proxy_Transform_for (F_index_01_R, Hand_R);
      --
      --
      --  --  Left Leg
      --  --
      --  set_Transform_for (Thigh_L);
      --  set_Transform_for (Shin_L);
      --  set_Transform_for (Foot_L);
      --  set_Transform_for (Toe_L);
      --
      --
      --  --  Right Leg
      --  --
      --  set_Transform_for (Thigh_R);
      --  set_Transform_for (Shin_R);
      --  set_Transform_for (Foot_R);
      --  set_Transform_for (Toe_R);
   end evolve;


   -------------
   --  Animation
   --
   procedure animate (Self : in out Item;   world_Age : in Duration)
   is
      Now     : Duration;
      Elapsed : Duration;


      procedure update_rotation_Animation (for_Channel : in channel_Id;
                                           for_Joint   : in gel.human_v1.scene_joint_Id;
                                           for_Axis    : in axis_Kind)
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
                     the_Channel.interp_Delta := Reduced (the_Channel.Values (Cursor) - the_Channel.current_Angle);
                  else
                     the_Channel.interp_Delta :=   Reduced (the_Channel.Values (Cursor) - the_Channel.current_Angle)
                                                 / (the_Channel.Times  (Cursor));
                  end if;
               else
                  the_Channel.interp_Delta :=   Reduced (the_Channel.Values (Cursor) - the_Channel.current_Angle)
                                              / (the_Channel.Times  (Cursor) - the_Channel.Times (Cursor - 1));
               end if;

               the_Channel.interp_Delta := the_Channel.interp_Delta / 60.0;  -- 60.0 is frames/sec
            end if;
         end if;

         if Elapsed < Duration (the_Channel.Times (the_Channel.Times'Last))
         then
            the_Channel.current_Angle := Reduced (  the_Channel.current_Angle
                                                  + the_Channel.interp_Delta);
            Self.set_rotation_Angle (for_Joint,  for_Axis,
                                     to => to_Radians (math.Degrees (the_Channel.current_Angle)));
         end if;
      end update_rotation_Animation;



      procedure update_location_Animation (for_Channel : in channel_Id;
                                           for_Joint   : in gel.human_v1.scene_joint_Id)
      is
         the_Channel : animation_Channel renames Self.Channels (for_Channel);
         Cursor      : math.Index        renames the_Channel.Cursor;
         Elapsed     : constant Duration      := Now - Self.start_Time;

         function site_X return math.Real is begin   return the_Channel.Values ((Cursor - 1) * 3 + 1);   end site_X;
         function site_Y return math.Real is begin   return the_Channel.Values ((Cursor - 1) * 3 + 2);   end site_Y;
         function site_Z return math.Real is begin   return the_Channel.Values ((Cursor - 1) * 3 + 3);   end site_Z;

      begin
         if Cursor < the_Channel.Times'Last
         then
            if        Cursor = 0
              or else Elapsed > Duration (the_Channel.Times (Cursor))
            then
               Cursor := Cursor + 1;

               if Cursor = 1 then
                  if the_Channel.Times  (Cursor) = 0.0
                  then
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

            Self.set_Location (for_Joint,  to => the_Channel.current_Site);

            the_Channel.current_Site (1) := the_Channel.current_Site (1) + the_Channel.site_interp_Delta (1);
            the_Channel.current_Site (2) := the_Channel.current_Site (2) + the_Channel.site_interp_Delta (2);
            the_Channel.current_Site (3) := the_Channel.current_Site (3) + the_Channel.site_interp_Delta (3);

         end if;
      end update_location_Animation;


      procedure update_full_transform_Animation (for_Channel : in channel_Id;
                                                 for_Joint   : in gel.Human_v1.scene_joint_Id)
      is
         the_Channel    : animation_Channel renames Self.Channels (for_Channel);
         Cursor         : math.Index        renames the_Channel.Cursor;
         Cursor_updated : Boolean := False;
         new_Transform  : math.Matrix_4x4;

      begin
         if Cursor = the_Channel.Times'Last
         then
--              the_Channel.initial_Transform := the_Channel.Transforms (1);
--              the_Channel.current_Transform := the_Channel.initial_Transform;
--
--              the_Channel.current_Site  := the_Channel.initial_Transform.Translation;
--              the_Channel.initial_Site  := the_Channel.current_Site;

            Cursor          := 0;
            Self.start_Time := Now;
         end if;

         -- Rotation
         --
         declare
            use Math;
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
                        the_Channel.Transform_interp_Delta := 1.0 / 60.0;    -- the_Channel.Values (Cursor) - the_Channel.current_Angle;
                     else
                        the_Channel.Transform_interp_Delta := the_Channel.Times (Cursor);
                     end if;
                  else
                     Initial := the_Channel.Transforms (Cursor - 1);
                     the_Channel.Transform_interp_Delta := the_Channel.Times (Cursor) - the_Channel.Times (Cursor - 1);
                  end if;

                  the_Channel.current_Transform      := the_Channel.Transforms (Cursor);
                  the_Channel.Transform_interp_Delta := 1.0 / (the_Channel.Transform_interp_Delta * 60.0);  -- 60.0 is frames/sec
                  the_Channel.slerp_Time             := 0.0;
               else
                  if Cursor > 1 then
                     Initial := the_Channel.Transforms (Cursor - 1);
                  else
                     Initial := the_Channel.Transforms (Cursor);
                  end if;
               end if;
            else
               Initial := the_Channel.Transforms (1);
            end if;

            if Elapsed < Duration (the_Channel.Times (the_Channel.Times'Last))
            then
--                 put_Line ("Initial.Rotation: " & Image (Initial.Rotation)
--                           & "     current_Transform.Rotation: " & Image (the_Channel.current_Transform.Rotation));

               set_Rotation (new_Transform, to_Matrix (Interpolated (Initial.Rotation,
                                                                     the_Channel.current_Transform.Rotation,
                                                                     to_Percentage (the_Channel.slerp_Time))));
--                 set_Rotation (new_Transform, to_Matrix (Slerp (the_Channel.Transforms (Cursor  ).Rotation,
--                                                                the_Channel.Transforms (Cursor+1).Rotation,
--                                                                the_Channel.slerp_Time)));
--                 put_Line ("SLERP Time: " & Real'Image (the_Channel.slerp_Time)
--                           & "     Transform_interp_Delta: " & Real'Image (the_Channel.Transform_interp_Delta));
               the_Channel.slerp_Time :=   the_Channel.slerp_Time
                                         + the_Channel.Transform_interp_Delta;
            end if;
         end;

--           new_Line;
--           put_Line ("Cursor:    " & math.Index'Image (Cursor));
--           put_Line ("Elapsed:   " & Duration'Image  (Elapsed));
--           put_Line ("next Time: " & math.Real'Image (the_Channel.Times (Cursor)));
--
--           if cursor = 2
--             and elapsed > Duration (the_Channel.Times (Cursor))
--           then
--              put_Line ("*** Cursor: " & math.Index'Image (Cursor));
--           end if;

         -- Location
         --
         declare
            use type math.Vector_3;

--              function site_X return math.Real is begin   return the_Channel.Values ((Cursor - 1) * 3 + 1);   end site_X;
--              function site_Y return math.Real is begin   return the_Channel.Values ((Cursor - 1) * 3 + 2);   end site_Y;
--              function site_Z return math.Real is begin   return the_Channel.Values ((Cursor - 1) * 3 + 3);   end site_Z;

            desired_Site : math.Vector_3 := the_Channel.Transforms (Cursor + 0).Translation;

         begin
            if Cursor < the_Channel.Times'Last
            then
               if Cursor_updated --       Cursor = 1
--                   or else Elapsed > Duration (the_Channel.Times (Cursor))
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

                  the_Channel.site_interp_Delta := the_Channel.site_interp_Delta / 60.0;  -- 60.0 is frames/sec
               end if;

               the_Channel.current_Site := the_Channel.current_Site + the_Channel.site_interp_Delta;

--                 if for_Channel = Hips
--                 then
--                    new_Line;
--                    put_Line ("the_Channel.site_interp_Delta: " & math.Image (the_Channel.site_interp_Delta));
--                    put_Line ("the_Channel.current_Site:      " & math.Image (the_Channel.current_Site));
--                 end if;

               set_Translation (new_Transform,   to => the_Channel.current_Site);
            else
               null;
            end if;
         end;


         -- Scale
         --
         -- (TODO)


         -- Store the new transform.
         --
         Self.set_Transform (for_Joint, to => new_Transform);

      end update_full_transform_Animation;


   begin
      Now := world_Age;

      if Self.start_Time = 0.0 then
         Self.start_Time := Now;
      end if;

      Elapsed := Now - Self.start_Time;

      update_full_transform_Animation (Hips,              Hips);
      update_full_transform_Animation (Thigh_L,           Thigh_L);
      update_full_transform_Animation (Shin_L,            Shin_L);
      update_full_transform_Animation (Foot_L,            Foot_L);
      update_full_transform_Animation (Toe_L,             Toe_L);
      update_full_transform_Animation (Thigh_R,           Thigh_R);
      update_full_transform_Animation (Shin_R,            Shin_R);
      update_full_transform_Animation (Foot_R,            Foot_R);
      update_full_transform_Animation (Toe_R,             Toe_R);
      update_full_transform_Animation (Spine,             Spine);
      update_full_transform_Animation (Chest,             Chest);
      update_full_transform_Animation (Clavicle_R,        Clavicle_R);
      update_full_transform_Animation (upper_Arm_R,       upper_Arm_R);
      update_full_transform_Animation (Forearm_R,         Forearm_R);
      update_full_transform_Animation (Hand_R,            Hand_R);
      --  update_full_transform_Animation (Thumb_02_R,        Thumb_02_R);
      --  update_full_transform_Animation (Thumb_03_R,        Thumb_03_R);
      --  update_full_transform_Animation (F_ring_01_R,       F_ring_01_R);
      --  update_full_transform_Animation (F_index_01_R,      F_index_01_R);
      update_full_transform_Animation (Clavicle_L,        Clavicle_L);
      update_full_transform_Animation (upper_Arm_L,       upper_Arm_L);
      update_full_transform_Animation (Forearm_L,         Forearm_L);
      update_full_transform_Animation (Hand_L,            Hand_L);
      --  update_full_transform_Animation (Thumb_02_L,        Thumb_02_L);
      --  update_full_transform_Animation (Thumb_03_L,        Thumb_03_L);
      --  update_full_transform_Animation (F_ring_01_L,       F_ring_01_L);
      --  update_full_transform_Animation (F_index_01_L,      F_index_01_L);
      update_full_transform_Animation (Neck,              Neck);
      update_full_transform_Animation (Head,              Head);
      update_full_transform_Animation (Jaw,               Jaw);
      --  update_full_transform_Animation (Eye_R,             Eye_R);
      --  update_full_transform_Animation (Eye_L,             Eye_L);

      Self.update_all_global_Transforms;
   end animate;



   procedure reset_Animation (Self : in out Item)
   is
   begin
      Self.start_Time := 0.0;

      for Each in Self.Channels'Range
      loop
         Self.Channels (Each).Cursor        := 0;
         Self.Channels (Each).current_Angle := Self.Channels (Each).initial_Angle;
         Self.Channels (Each).current_Site  := Self.Channels (Each).initial_Site;
         Self.Channels (Each).interp_Delta  := 0.0;
      end loop;
   end reset_Animation;



   procedure Mode_is (Now : in display_Mode)
   is
   begin
      the_display_Mode := Now;
   end Mode_is;



begin

   for Each in to_scene_joint_Id'Range
   loop
      to_scene_joint_Id (Each) := scene_joint_Id'Value (controller_joint_Id'Image (Each));
   end loop;


   for Each in to_controller_joint_Id'Range
   loop
      begin
         to_controller_joint_Id (Each) := controller_joint_Id'Value (scene_joint_Id'Image (Each));
--        exception
--           when constraint_Error =>
--              if Each /= Armature then   raise;   end if;
      end;
   end loop;


end gel.Human_v1;
