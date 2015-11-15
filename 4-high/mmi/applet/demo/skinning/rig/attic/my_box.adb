with mmi.Sprite,
     mmi.Forge,
     openGL.Model.open_gl,
     openGL.Model.box.colored,
     mmi.Joint,
     mmi.hinge_Joint,
     mmi.cone_twist_Joint,
     mmi.Conversions,

     collada.Document,
     collada.Asset,
     collada.Library,
     collada.Library.geometries,
     collada.Library.controllers,
     collada.Library.visual_scenes,

     opengl.Palette,
     opengl.Program.lit_textured_skinned,
     opengl.Geometry.lit_textured_skinned,

     float_math.Algebra.linear.d3,

     ada.Strings.fixed,
     ada.Strings.unbounded;

with Ada.Text_IO; use Ada.Text_IO;
with MMI.physics_Model;





package body my_Box
is

   use math.Algebra.linear.d3;


   --- Core types
   --




   --- Utility
   --

   function "+" (From : in ada.strings.unbounded.unbounded_String) return String
     renames ada.strings.unbounded.to_String;



   function to_joint_Id (From : in String) return scene_joint_Id
   is
      Pad : String := From;
   begin
      for Each in Pad'range loop
         if Pad (Each) = '-' then
            Pad (Each) := '_';
         end if;
      end loop;

      return scene_joint_Id'Value (Pad);
   end;



   function to_Math (From : in collada.Matrix_4x4) return math.Matrix_4x4
   is
      use type math.Real;
   begin
      return (1 => (math.Real (From (1, 1)),  math.Real (From (1, 2)),  math.Real (From (1, 3)),  math.Real (From (1, 4))),
              2 => (math.Real (From (2, 1)),  math.Real (From (2, 2)),  math.Real (From (2, 3)),  math.Real (From (2, 4))),
              3 => (math.Real (From (3, 1)),  math.Real (From (3, 2)),  math.Real (From (3, 3)),  math.Real (From (3, 4))),
              4 => (math.Real (From (4, 1)),  math.Real (From (4, 2)),  math.Real (From (4, 3)),  math.Real (From (4, 4))));
   end;


   -- Mappings between bone_Id and collada_joint_Id
   --

   collada_Joint_for : constant array (bone_Id) of collada_joint_Id
     := (base   => <>,
         bone_1 => Bone_1,
         bone_2 => Bone_2);


   Bone_for : constant array (collada_joint_Id) of bone_Id
     := (bone_1 => Bone_1,
         bone_2 => Bone_2);


   my_Bone_for : constant array (collada_joint_Id) of scene_joint_Id
     := (bone_1 => Bone_1,
         bone_2 => Bone_2);




   --- Globals
   --

--     the_Program : aliased opengl.Program.lit_textured_skinned.item;



   type bone_transform_Uniforms is array (collada_joint_Id) of opengl.Variable.uniform.mat4;

   the_bone_transform_Uniforms : bone_transform_Uniforms;




   --- Forge
   --

   package body Forge is

      function new_Box (in_World     : in mmi.World.view;
                        Model        : access openGL.Model.item'class;
                        Mass         : in     math.Real           := 0.0;
                        is_Kinematic : in     Boolean             := False) return my_Box.view
      is
         Self : my_Box.view := new my_Box.item;
      begin
         Self.define (in_World, Model, Mass, is_Kinematic);

         return Self;
      end;



      function new_Box (bone_Sprites            : in     my_box.bone_Sprites                     ;
                        joint_inv_bind_Matrices : in     joint_inverse_bind_Matrices;
                        joint_site_Offets       : in     joint_to_bone_site_Offets  ;
                        Model                   : access openGL.Model.item'class                   ) return my_Box.view
      is
         the_Box : my_Box.View := new my_Box.item;
      begin
         the_Box.bone_Sprites            := bone_Sprites;
         the_Box.joint_inv_bind_Matrices := joint_inv_bind_Matrices;
         the_Box.joint_site_Offets       := joint_site_Offets;
         the_Box.Model                   := Model;

         return the_Box;
      end;

   end Forge;



   --- skin_program_Parameters
   --

   procedure enable (Self : in out skin_program_Parameters)
   is
      use Math;
   begin
--        for Each in the_bone_transform_Uniforms'range
--        loop
--           put_Line ("AAAAAAAAAAAAAAAAAAAAAA");
--           put_Line (Image (Matrix (Self.bone_Transforms (Each))));
--
--           the_bone_transform_Uniforms (Each).Value_is (Self.bone_Transforms (Each));
--
--  --           the_bone_transform_Uniforms (Each).Value_is (math.Algebra.linear.d3.to_rotate_Matrix (Rotation => math.Algebra.linear.d3.x_Rotation_from (0.1)));
--
--        end loop;


      for Each in Self.bone_Transforms'Range
      loop
         openGL.Program.lit_textured_skinned.view (Self.Program)
           .bone_Transform_is (which => collada_joint_Id'Pos (Each) + 1,
                               now   => Self.bone_Transforms (Each));
      end loop;


   end;




   --- Animation
   --

   procedure set_global_Transform_for (Self : in out Item'Class;   the_Joint : in collada.Library.visual_scenes.Node_view)
   is
      use collada.Library;

      which_Joint         : scene_joint_Id       := to_joint_Id (+the_Joint.Name);
      child_Joints        : visual_scenes.Nodes  := the_Joint.Children;
   begin
      declare

      begin

--           Self.joint_global_Transforms (which_Joint) := to_Math (the_Joint.global_Transform);
         Self.joint_global_Transforms (which_Joint) := math.Transpose (to_Math (the_Joint.global_Transform));   -- Transpose to convert to row-major.
      end;

      Self.collada_Joints (which_Joint) := the_Joint;

      for Each in child_Joints'range
      loop
         set_global_Transform_for (Self, child_Joints (Each));      -- recurse over children
      end loop;
   end set_global_Transform_for;



   procedure set_x_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Real)
   is
   begin
      Self.collada_Joints (for_Joint).set_x_rotation_Angle (collada.math.Real (To));
      set_global_Transform_for (Self, Self.root_Joint);                              -- re-determine all joint transforms, recursively
   end;


   procedure set_y_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Real)
   is
   begin
      Self.collada_Joints (for_Joint).set_y_rotation_Angle (collada.math.Real (To));
      set_global_Transform_for (Self, Self.root_Joint);                              -- re-determine all joint transforms, recursively
   end;


   procedure set_z_rotation_Angle (Self : in out Item'Class;   for_Joint : in scene_joint_Id;
                                                               To        : in math.Real)
   is
   begin
      Self.collada_Joints (for_Joint).set_z_rotation_Angle (collada.math.Real (To));
      set_global_Transform_for (Self, Self.root_Joint);                              -- re-determine all joint transforms, recursively
   end;






   --- my_Box item
   --

   procedure define (Self : in out Item;   in_World     : in mmi.World.view;
                                           Model        : access openGL.Model.item'class;
                                           Mass         : in     math.Real           := 0.0;
                                           is_Kinematic : in     Boolean             := False)
   is
      use Collada.Document,  collada.Library,  collada.Library.geometries, collada.Library.visual_scenes,
          math.Vectors,  math.Algebra.linear.d3,
          ada.Strings.fixed, ada.Strings.unbounded, ada.Strings;

      type gl_Model_view is access all openGL.Model.open_gl.item;

      the_Model      : gl_Model_view            := gl_Model_view (Model);

      the_Document   : collada.Document.item    := to_Document (openGL.to_String (the_Model.Model));
      the_root_Joint : visual_scenes.Node_view  := the_Document.libraries.visual_Scenes.Contents (1).root_Node.Child (1);


      joint_Sites : array (scene_joint_Id) of math.Vector_3;

      procedure set_Site_for (the_Joint : in visual_scenes.Node_view;   parent_Site : in math.Vector_3)
      is
         use math;

         function get_Joint_Translation return collada.Vector_3
         is
         begin
            return the_Joint.Translation;
         exception
            when Collada.Library.Visual_Scenes.Transform_Not_Found =>
               return get_Translation (the_Joint.full_Transform);
         end get_Joint_Translation;

         function correct_for_Z_up (From : collada.Vector_3) return collada.Vector_3
         is
            use Collada.Asset;
         begin
--              case the_Document.Asset.up_Axis
--              is
--                 when X_up =>
--                    raise Collada.Error with "X_up axis is not supported";
--
--                 when Y_up =>
                  return From;

--                 when Z_up =>
--                    return ( From (1),
--                             From (3),
--                            -From (2));
--              end case;
         end correct_for_Z_up;



         collada_Translation : collada.Vector_3     := correct_for_Z_up (get_Joint_Translation); -- the_Joint.Translation;
         the_Site            : math.Vector_3        := parent_Site + math.Vector_3'(math.Real (collada_Translation (1)),
                                                                                    math.Real (collada_Translation (2)),
                                                                                    math.Real (collada_Translation (3)));
         which_Joint         : scene_joint_Id       := to_joint_Id (+the_Joint.Name);
         child_Joints        : visual_scenes.Nodes := the_Joint.Children;
      begin
         joint_Sites (which_Joint) := the_Site;

         for Each in child_Joints'Range
         loop
            set_Site_for (child_Joints (Each), parent_site => the_Site);      -- recurse over children
         end loop;
      end set_Site_for;


   begin
      set_Site_for             (the_root_Joint,  parent_site => (0.0, 0.0, 0.0));     -- determine all joint sites, recursively
      set_global_Transform_for (Self, the_root_Joint);                              -- determine all joint transforms, recursively

      Self.root_Joint := the_root_Joint;                                            -- remember our root joint.
      Self.Model      := Model.all'unchecked_access;                                -- remember our model


      -- Set the inverse bind matrices for all joints.
      --
      declare
         use collada.Asset, collada.Library.controllers,
             math,          math.algebra.linear.d3;

         the_Skin       : controllers.Skin         := the_Document.libraries.controllers.Contents (1).Skin;
         the_bind_Poses : collada.Matrix_4x4_array := bind_Poses_of (the_Skin);
      begin
         for Each in Self.joint_inv_bind_Matrices'Range
         loop
--              case the_Document.Asset.up_Axis
--              is
--                 when X_up =>
--                    raise Collada.Error with "X_up axis is not supported";
--
--                 when Y_up =>
                  Self.joint_inv_bind_Matrices (Each) := transpose (to_Math (the_bind_Poses (collada_joint_Id'Pos (Each) + 1)));  -- Transpose correct for collada col major.

--                 when Z_up =>
--                    declare
--                       the_Matrix      : math.Matrix_4x4 := transpose (to_Math (the_bind_Poses (collada_joint_Id'Pos (Each) + 1)));  -- transpose correct for collada col major
--                       the_Rotation    : math.Matrix_3x3 := get_Rotation (the_Matrix);
--                       the_Translation : math.Vector_3   := get_Translation (the_Matrix);
--                    begin
--                       the_Translation := ( the_Translation (1),
--                                            the_Translation (3),
--                                           -the_Translation (2));
--
--                       the_Rotation := x_Rotation_from (to_Radians (-90.0)) * the_Rotation;
--  --                       the_Rotation := the_Rotation * x_Rotation_from (to_Radians (-90.0));
--
--                       set_Translation (the_Matrix, the_Translation);
--                       set_Rotation    (the_Matrix, the_Rotation);
--
--                       Self.joint_inv_bind_Matrices (Each) := the_Matrix;
--                    end;
--              end case;
         end loop;
      end;


      -- Define a sprite for each bone.
      --
      declare
         use openGL.Model.box, openGL, opengl.Palette;
         use type math.Real;

--           the_joint_Model : access openGL.Model.box.colored.item
--             := openGL.Model.box.colored.Forge.new_Box (Scale => (1.0, 1.0, 1.0),
--                                                        Faces => (front => (colors => (others => (Red,     Opaque))),
--                                                                  rear  => (colors => (others => (Blue,    Opaque))),
--                                                                  upper => (colors => (others => (Green,   Opaque))),
--                                                                  lower => (colors => (others => (Yellow,  Opaque))),
--                                                                  left  => (colors => (others => (Cyan,    Opaque))),
--                                                                  right => (colors => (others => (Magenta, Opaque)))));


         procedure create_Bone (the_Bone    : in bone_Id;
                                start_Joint : in scene_joint_Id;
                                end_Point   : in math.Vector_3;
                                Scale       : in math.Vector_3;
                                Mass        : in math.Real)
         is
            use math;

--              the_Model : access openGL.Model.box.colored.item
--                := openGL.Model.box.colored.Forge.new_Box (Scale,
--                                                           faces => (front => (colors => (others => (Red,     Opaque))),
--                                                                     rear  => (colors => (others => (Blue,    Opaque))),
--                                                                     upper => (colors => (others => (Green,   Opaque))),
--                                                                     lower => (colors => (others => (Yellow,  Opaque))),
--                                                                     left  => (colors => (others => (Cyan,    Opaque))),
--                                                                     right => (colors => (others => (Magenta, Opaque)))));

            the_bone_Site : math.Vector_3 := midPoint (joint_Sites (start_Joint),
                                                       end_Point);   -- joint_Sites (end_Joint));
         begin
--              Self.bone_Sprites (the_Bone)  := mmi.Sprite.forge.new_Sprite (the_Model,
--                                                                            Mass,
--                                                                            is_Kinematic);
            Self.bone_Sprites (the_Bone)  := mmi.forge.new_box_Sprite (in_World => in_World,
                                                                       Mass     => Mass,
                                                                       Size     => Scale,
                                                                       Colors   => (3      => Green,
                                                                                    4      => Blue,
                                                                                    others => Red));

            Self.bone_Sprites (the_Bone).Site_is (the_bone_Site);
            Self.bone_Sprites (the_Bone).Spin_is (get_Rotation (Self.joint_global_Transforms (start_Joint)));
--              Self.bone_Sprites (the_Bone).Spin_is (x_Rotation_from (to_Radians (90.0)) * get_Rotation (Self.joint_global_Transforms (start_Joint)));

--              Self.bone_Sprites (the_Bone).Spin_is (x_Rotation_from (to_Radians (-180.0)));
--              Self.bone_Sprites (the_Bone).Spin_is (x_Rotation_from (to_Radians (0.0)));
--  --              Self.bone_Sprites (the_Bone).Spin_is (get_Rotation (Self.joint_global_Transforms (start_Joint)) * x_Rotation_from (to_Radians (-90.0)));

--              Self.bone_Sprites (the_Bone).is_Visible (False);
            Self.bone_Sprites (the_Bone).is_Visible (True);

--              Self.joint_site_Offets (collada_Joint_for (the_Bone)) := the_bone_Site - joint_Sites (start_Joint);
            Self.joint_site_Offets (collada_Joint_for (the_Bone)) := joint_Sites (start_Joint) - the_bone_Site;
         end create_Bone;


         bone_Extent : math.Real;
         Direction   : math.Vector_3;

         use Math, mmi.physics_Model;
         physics_Model : mmi.physics_Model.View := mmi.physics_Model.Forge.new_physics_Model (shape_Info  => (kind         => cube,
                                                                                                              half_extents => (0.05, 0.05, 0.05)),
                                                                                              mass        => 0.0);
      begin
         --  Skin
         --
--           declare
--              the_human_graphics_Model : aliased openGL.Model.open_gl.view
--                := openGL.Model.open_gl.forge.new_Model (scale   => (1.0, 1.0, 1.0),
--                                                               model   => to_Asset ("./rigged_box.dae"),
--                                                               texture => openGL.null_Asset, -- mmi.to_Asset ("assets/collada/mmi-human-texture.tga"),
--                                                               Texture_is_lucid => False);
--              the_physics_Model : constant mmi.physics_Model.view
--                := mmi.physics_Model.Forge.new_physics_Model  (shape_Info  => (kind         => mmi.physics_Model.Cube,
--                                                                              half_extents => (0.1, 0.1, 0.1)),
--                                                               mass        => 0.0,
--                                                               is_Tangible => False);
--           begin
--              Self.skin_Sprite := mmi.Sprite.forge.new_Sprite ("human.skin_Sprite",
--                                                               in_World,
--                                                               the_human_graphics_Model,
--                                                               the_physics_Model,
--                                                               owns_graphics => True,
--                                                               owns_physics  => True,
--                                                               is_kinematic  => is_Kinematic);
--           end;




--           the_Model.Scale := (0.1, 0.1, 0.1);

         -- the Base sprite
         --
         Self.bone_Sprites (Base) := mmi.Sprite.forge.new_Sprite ("Base Sprite",
                                                                  in_World,
                                                                  Model,
                                                                  physics_Model);

--           Self.bone_Sprites (Base) := mmi.forge.new_box_Sprite (in_World => in_World,
--                                                                 Mass     => 0.0,
--                                                                 Size     => (0.1, 0.1, 0.1),
--                                                                 Colors   => (others => Green));

--           Self.bone_Sprites (Base).Site_is ((0.0, 0.0, -1.0));

         Self.bone_Sprites (Base).Transform_is (math.Identity_4x4);


         -- Bone
         --
--  --           create_Bone (Bone,  Bone, joint_Sites (Bone) + (0.0, -0.5, 0.0),  (0.1, 0.5, 0.1),  1.0);
--           create_Bone (Bone,  Bone, joint_Sites (Bone) + (0.0, 1.0, 0.0),  (0.1, 1.0, 0.1),  1.0);

--           create_Bone (Bone,  Bone, joint_Sites (Bone) + (0.0, 0.0, -1.0),  (0.1, 0.1, 1.0),  1.0);
--           create_Bone (Bone,  Bone, joint_Sites (Bone) + (0.0, 0.0, -1.0),  (0.1, 0.1, 1.0),  1.0);


         create_Bone (Bone_1,  Bone_1, joint_Sites (Bone_1) + (0.0, 0.0, 1.0),  (0.1, 1.0 * 0.2, 0.1),  0.0);

         Self.bone_Sprites (Base).attach_via_ball_Socket (Self.bone_Sprites (Bone_1),
                                                          pivot_Axis   => x_Rotation_from (0.0),
                                                          pivot_anchor => joint_Sites (Bone_1),
                                                          pitch_limits => (-0.0, 0.0),
                                                          yaw_limits   => (-0.0, 0.0),
                                                          roll_limits  => (-0.0, 0.0),
                                                          new_joint    => Self.Joints (base_To_bone_1));

         create_Bone (Bone_2,  Bone_2, joint_Sites (Bone_2) + (0.0, 0.0, 2.0),  (0.1, 2.0 * 0.2, 0.1),  0.0);

         Self.bone_Sprites (Bone_1).attach_via_ball_Socket (Self.bone_Sprites (Bone_2),
                                                            pivot_Axis   => x_Rotation_from (0.0),
                                                            pivot_anchor => joint_Sites (Bone_2),
                                                            pitch_limits => (-0.0, 0.0),
                                                            yaw_limits   => (-0.0, 0.0),
                                                            roll_limits  => (-0.0, 0.0),
                                                            new_joint    => Self.Joints (bone_1_To_bone_2));
      end;


      -- set animations transmofrms
      --

--        declare
--           use Math;
--        begin
--           null;
--           math.algebra.linear.d4.set_Rotation (Self.bone_rest_Rotations  (bone_1),  x_rotation_from (to_Radians (90.0)));
--
--           math.algebra.linear.d4.set_Rotation (Self.bone_rest_Rotations  (bone_2),    -- math.algebra.linear.d4.to_Rotation (Self.bone_rest_Rotations  (bone_1))
--                                                                                       z_Rotation_from (to_Radians  (82.0))
--                                                                                     * x_Rotation_from (to_Radians (180.0))
--                                                                                     * math.algebra.linear.d4.to_Rotation (Self.bone_rest_Rotations  (bone_1))
--                                               );
--
--  --           math.algebra.linear.d4.set_Rotation (Self.bone_rest_Rotations  (bone_2),   y_Rotation_from (to_Radians (180.0)));
--        end;


   end define;





   procedure enable_Graphics (Self : in out Item)
   is
      use ada.Strings.fixed, ada.Strings;
   begin
      -- Define the opengl shader program, if required.
      --
--        if not the_Program.is_Defined
--        then
--           the_Program.define ("./assets/shader/skin.vert",                 -- tbd: how best to make this portable ?
--                               "./assets/shader/skin.frag");

--           for Each in the_bone_transform_Uniforms'range
--           loop
--              the_bone_transform_Uniforms (Each).define (the_Program'access,
--                                                         "bone_Matrices[" & Trim (Integer'image (collada_joint_Id'Pos (Each)), Left) & "]");
--           end loop;
--        end if;

--        the_Program := openGL.Model.open_gl.forge.new_Model (scale   => (1.0, 1.0, 1.0),
--                                                               model   => openGL.to_Asset (model_Name.all),
--                                                               texture => openGL.null_Asset, -- mmi.to_Asset ("assets/collada/mmi-human-texture.tga"),
--                                                               Texture_is_lucid => False);

--        Self.program_Parameters.Program_is (the_Program'unchecked_access);
      null;


      Self.program_Parameters.Program_is (opengl.Program.view (opengl.Geometry.lit_textured_skinned.Program));
      Self.base_Sprite.program_Parameters_are (Self.program_Parameters'Unchecked_Access);
--        Self.skin_Sprite.program_Parameters_are (Self.program_Parameters'Unchecked_Access);


   end enable_Graphics;





   procedure attach_program_Parameters_to_model_Faces (Self : in out Item)
   is
      use mmi.Sprite;
   begin
--        if Self.Model.opaque_Geometries'Length > 0
--        then
--           Self.Model.opaque_Geometries (1).Program_Parameters_are  (Self.program_Parameters'unchecked_access);
--           Self.Model.opaque_Geometries (1).Geometry.Attribute_1_is (the_Program.Attribute (named => "bone_Ids"));
--           Self.Model.opaque_Geometries (1).Geometry.Attribute_2_is (the_Program.Attribute (named => "bone_Weights"));
--        else
--           Self.Model.lucid_Geometries (1).Program_Parameters_are (Self.program_Parameters'unchecked_access);
--           Self.Model.lucid_Geometries (1).Geometry.Attribute_1_is (the_Program.Attribute (named => "bone_Ids"));
--           Self.Model.lucid_Geometries (1).Geometry.Attribute_2_is (the_Program.Attribute (named => "bone_Weights"));
--        end if;
--
--        Self.base_Sprite.program_Parameters_are (Self.program_Parameters'unchecked_access);

      null;
   end;





   function  joint_inv_bind_Matrices (Self : in Item'Class) return joint_inverse_bind_Matrices
   is
   begin
      return Self.joint_inv_bind_Matrices;
   end;



   procedure joint_inv_bind_Matrices_are (Self : in out Item'Class;   Now : in joint_inverse_bind_Matrices)
   is
   begin
      Self.joint_inv_bind_Matrices := Now;
   end;






   function  joint_site_Offets       (Self : in Item'Class) return joint_to_bone_site_Offets
   is
   begin
      return Self.joint_site_Offets;
   end;




   --- Attributes
   --

   function Sprite (Self : in Item'Class;   for_Bone : in bone_Id) return mmi.Sprite.view
   is
   begin
      return Self.bone_Sprites (for_Bone);
   end;



   function  base_Sprite (Self : in Item'Class) return mmi.Sprite.view
   is
   begin
      return Self.bone_Sprites (Base);
   end;


   function  skin_Sprite (Self : in Item'Class) return mmi.Sprite.view
   is
   begin
      return Self.skin_Sprite;
   end;



   procedure set_Transform (Self : in out Item'Class;   for_Bone : in collada_joint_Id;
                                                        To       : in math.Matrix_4x4)
   is
      use mmi.Conversions;
   begin
      Self.program_Parameters.bone_Transforms (for_Bone) := to_GL (To);
   end;



   procedure animation_Transforms_are (Self : in out Item'Class;   Now : in animation_Transforms)
   is
   begin
      Self.animation_Transforms := Now;
   end;






   --- Operations
   --


   procedure evolve (Self : in out Item'Class)
   is
      use math,  math.Algebra.linear.d3;

      subtype as_Hinge is mmi.hinge_Joint.view;

--        root_Transform     : math.Matrix_4x4 := math.Identity_4x4;
      root_Transform     : math.Matrix_4x4 := Self.base_Sprite.Transform;
--  --        root_Transform     : math.Matrix_4x4 := Self.base_Sprite.Transform * as_Hinge (Self.Joints (base_To_hips)).Frame_A * Self.bone_inv_bind_Matrices (Root);
--  --        root_Transform     : math.Matrix_4x4 := Self.Sprites (Hips).Transform;

      inv_root_Transform : math.Matrix_4x4 := Inverse (root_Transform);


      function joint_Transform_for (the_collada_Joint : in collada_joint_Id) return math.Matrix_4x4
      is
         the_bone_Transform    : math.Matrix_4x4 := Self.bone_Sprites (Bone_for (the_collada_Joint)).Transform;
--           the_bone_Transform    : math.Matrix_4x4 :=   Self.animation_Transforms (Bone_for (the_collada_Joint))
--                                                      * Self.bone_rest_Rotations  (Bone_for (the_collada_Joint));

--           the_bone_Transform    : math.Matrix_4x4 :=   Self.animation_Transforms (Bone_for (the_collada_Joint))
--                                                      * Self.bone_rest_Rotations  (Bone_for (the_collada_Joint));

--           the_bone_Transform    : math.Matrix_4x4 :=   Self.animation_Transforms    (Bone_for (the_collada_Joint))
--  --                                                      * Inverse (Self.joint_global_Transforms (my_Bone_for (the_collada_Joint)));
--                                                      * Self.joint_global_Transforms (my_Bone_for (the_collada_Joint));

--           the_bone_Transform    : math.Matrix_4x4 :=    Self.joint_global_Transforms (my_Bone_for (the_collada_Joint));
--                                                       * Self.animation_Transforms    (Bone_for (the_collada_Joint));
--                                                      * Inverse (Self.joint_global_Transforms (my_Bone_for (the_collada_Joint)));



         the_bone_Rotation     : math.Matrix_3x3 := get_Rotation (the_bone_Transform);
         the_inv_bone_Rotation : math.Matrix_3x3 := get_Rotation (Inverse (the_bone_Transform));
         the_joint_site_Offset : math.Vector_3   := Self.joint_site_Offets (the_collada_Joint) * the_inv_bone_Rotation;

         the_joint_Transform   : math.Matrix_4x4 := the_bone_Transform;

      begin

         declare
            the_bone_Transform    : constant math.Matrix_4x4
--                := math.Identity_4x4;
              := Self.bone_Sprites (Bone_for (the_collada_Joint)).Transform;

            the_bone_Rotation     : constant math.Matrix_3x3
              := get_Rotation (the_bone_Transform);
            --                 the_inv_bone_Rotation : math.Matrix_3x3 := Inverse (the_bone_Rotation);

            the_joint_site_Offset : constant math.Vector_3
--                := Self.joint_site_Offets (the_collada_Joint);
              := Self.joint_site_Offets (the_collada_Joint) * the_bone_Rotation;
--                := the_inv_bone_Rotation * Self.joint_site_Offets (the_collada_Joint);
--  --  --                               := Self.controller_Joints (the_collada_Joint).joint_to_bone_site_Offet * the_bone_Rotation;

            the_joint_Transform   : math.Matrix_4x4;
         begin
            put_Line ("ZZZZZZZZZZZZZZZZZZZZZZZ: " & Image (get_Translation (the_bone_Transform) + the_joint_site_Offset));
--              set_Translation (the_joint_Transform,  (0.0, 0.0, 0.0));
            set_Translation (the_joint_Transform,  get_Translation (the_bone_Transform) + the_joint_site_Offset);
            set_Rotation    (the_joint_Transform,  get_Rotation    (the_bone_Transform));
--              set_Rotation    (the_joint_Transform,  x_Rotation_from (to_Radians (90.0)) * get_Rotation    (the_bone_Transform));
--              set_Rotation    (the_joint_Transform,  get_Rotation    (the_bone_Transform) * x_Rotation_from (to_Radians (-90.0)));

            return the_joint_Transform;
         end;



--              declare
--                 the_bone_Transform    : constant math.Matrix_4x4
--                   := Self.bone_Sprites (bone_Id (the_collada_Joint)).Transform;
--
--                 the_bone_Rotation     : constant math.Matrix_3x3
--                   := get_Rotation (the_bone_Transform);
--  --                 the_inv_bone_Rotation : math.Matrix_3x3 := Inverse (the_bone_Rotation);
--
--                 the_joint_site_Offset : constant math.Vector_3
--                   := Self.controller_Joints (the_collada_Joint).joint_to_bone_site_Offet * the_bone_Rotation;
--
--                 the_joint_Transform   : math.Matrix_4x4;
--              begin
--                 set_Translation (the_joint_Transform,  get_Translation (the_bone_Transform) + the_joint_site_Offset);
--                 set_Rotation    (the_joint_Transform,  get_Rotation    (the_bone_Transform));
--
--                 return the_joint_Transform;
--              end;





--           set_Translation (the_joint_Transform,  get_Translation (the_bone_Transform) - the_joint_site_Offset);
--           return the_joint_Transform;

--           return Inverse (Self.joint_global_Transforms (my_Bone_for (the_collada_Joint)));
--           return Self.joint_global_Transforms (my_Bone_for (the_collada_Joint));

           return math.Identity_4x4;

--             * Self.bone_rest_Rotations  (Bone_for (the_collada_Joint));
      end;



      procedure set_Transform_for (the_Bone : in collada_joint_Id)
      is
      begin
         new_line (2);
--           put_Line ("JJJ: " & math.Image (to_Translation (root_Transform)));
--           put_Line ("MMM: ");
--           put_Line (math.Image (get_Translation (joint_Transform_for (the_Bone))));
--           new_line;
         put_Line ("ZZZ: ");
--           put_line (math.Image (math.Matrix (inv_root_Transform)));
         put_line (math.Image (math.Matrix (joint_Transform_for (the_Bone))));

         Self.set_Transform (for_bone => the_Bone,
                             to       => Self.joint_inv_bind_Matrices (the_Bone)
--                               to       =>   to_rotate_Matrix (x_Rotation_from (to_Radians (-90.0))) * Self.joint_inv_bind_Matrices (the_Bone)
--                               to       =>   Self.joint_inv_bind_Matrices (the_Bone) --  * to_rotate_Matrix (x_Rotation_from (to_Radians (90.0)))
                                         * joint_Transform_for (the_Bone)
                                         * inv_root_Transform);

--           Self.set_Transform (for_bone => the_Bone,
--                               to       =>
--                                   joint_Transform_for (the_Bone)
--                                 * Self.joint_inv_bind_Matrices (the_Bone)
--                                           * inv_root_Transform);
      end;



      procedure set_proxy_Transform_for (the_Bone : in collada_joint_Id;   the_Proxy : in collada_joint_Id)
      is
      begin
         Self.set_Transform (for_bone => the_Bone,
                             to       =>   Self.joint_inv_bind_Matrices (the_Proxy)
                                         * joint_Transform_for (the_Proxy)
                                         * inv_root_Transform);
      end;


   begin
--        new_Line;
--        put_Line (math.Image (math.Matrix (inv_root_Transform)));
--        new_Line;
--        put_Line (math.Image (math.Matrix (the_hinge_Joint.Frame_A)));
--        put_Line (math.Image (math.Matrix (Self.Sprites (Spine1).Transform * the_hinge_Joint.Frame_A)));


      set_Transform_for (Bone_1);
      set_Transform_for (Bone_2);
   end;

end my_Box;
