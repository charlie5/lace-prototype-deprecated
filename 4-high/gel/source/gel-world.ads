with
     gel.remote.World,
     gel.Sprite,
     gel.Joint,

     openGL.Model,

     physics.Space,
     --  physics.Engine,
     physics.Model,

     lace.Event,
     lace.Observer,
     lace.Subject,
     lace.Subject_and_deferred_Observer,
     lace.Any,

     ada.Tags.generic_dispatching_Constructor,
     ada.unchecked_Conversion,
     ada.Containers.Vectors,
     ada.Containers.hashed_Maps;

limited
with
     openGL.Renderer.lean;

package gel.World
--
--  Provides a gel world.
--
is
   type Item  is limited new lace.Subject_and_deferred_Observer.item
                         and gel.remote.World.item
   with private;

   type View  is access all Item'Class;
   type Views is array (math.Index range <>) of View;     -- TODO: Use a Positive type instead of 'math.Index'.

   use Math;


   ---------
   --  Forge
   --

   package Forge
   is
      function to_World  (Name       : in     String;
                          Id         : in     world_Id;
                          space_Kind : in     physics.space_Kind;
                          Renderer   : access openGL.Renderer.lean.item'Class) return gel.World.item;

      function new_World (Name       : in     String;
                          Id         : in     world_Id;
                          space_Kind : in     physics.space_Kind;
                          Renderer   : access openGL.Renderer.lean.item'Class) return gel.World.view;
   end Forge;

   overriding
   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);


   --------------
   --  Attributes
   --

   function  local_Observer  (Self : in     Item)     return lace.Observer.view;
   function  local_Subject   (Self : in     Item)     return lace.Subject .view;

   function  Id              (Self : in     Item)     return world_Id;

   function  Age             (Self : in     Item)     return Duration;
   procedure Age_is          (Self : in out Item;   Now : in Duration);

   procedure Gravity_is      (Self : in out Item;   Now : in Vector_3);

   function  space_Kind      (Self : in     Item)     return physics.space_Kind;
   function  Space           (Self : in     Item)     return physics.Space.view;

   procedure update_Bounds   (Self : in out Item;   of_Sprite : in gel.Sprite.view);
   procedure update_Site     (Self : in out Item;   of_Sprite : in gel.Sprite.view;
                                                    To        : in Vector_3);
   procedure update_Scale    (Self : in out Item;   of_Sprite : in gel.Sprite.view;
                                                    To        : in Vector_3);

   procedure set_Speed       (Self : in out Item;   of_Sprite : in gel.Sprite.view;
                                                    To        : in Vector_3);
   procedure set_xy_Spin     (Self : in out Item;   of_Sprite : in gel.Sprite.view;
                                                    To        : in Radians);

   procedure apply_Force     (Self : in out Item;   to_Sprite : in gel.Sprite.view;
                                                    Force     : in Vector_3);

   -----------
   --  Sprites
   --
   function  new_sprite_Id   (Self : access Item)                              return sprite_Id;
   function  free_sprite_Set (Self : access Item)                              return gel.Sprite.views;
   function  Sprites         (Self : in     Item)                              return gel.Sprite.views;
   function  fetch_Sprite    (Self : in     Item;   Id         : in sprite_Id) return gel.Sprite.view;
   procedure destroy         (Self : in out Item;   the_Sprite : in gel.Sprite.view);
   procedure set_Scale       (Self : in out Item;   for_Sprite : in gel.Sprite.view;
                                                    To         : in Vector_3);

   type sprite_transform_Pair is
      record
         Sprite    : gel.Sprite.view;
         Transform : Matrix_4x4;
      end record;

   type sprite_transform_Pairs is array (Positive range <>) of sprite_transform_Pair;

   function  sprite_Transforms (Self : in Item) return sprite_transform_Pairs;


   ----------
   --- Joints
   --

   procedure destroy               (Self : in out Item;   the_Joint : in gel.Joint.view);

   procedure set_local_Anchor_on_A (Self : in out Item;   for_Joint : in gel.Joint.view;
                                                          To        : in Vector_3);
   procedure set_local_Anchor_on_B (Self : in out Item;   for_Joint : in gel.Joint.view;
                                                          To        : in Vector_3);

   --------------
   --- Collisions
   --

   type a_Contact is
      record
         Site : Vector_3;
      end record;

   type Contacts is array (Positive range 1 .. 4) of a_Contact;


   type a_Manifold is
      record
         Sprites : Sprite.views (1 .. 2);
         Contact : a_Contact;
      end record;

   type Manifold_array is array (Positive range <>) of a_Manifold;


   function manifold_Count (Self : in     Item)                           return Natural;
   function Manifold       (Self : in     Item;   Index : in    Positive) return a_Manifold;
   function Manifolds      (Self : in     Item)                           return Manifold_array;


   type impact_Filter   is access function  (the_Manifold : in a_Manifold) return Boolean;
   --
   --  Returns True if the impact is of interest and requires a response.

   type impact_Response is access procedure (the_Manifold : in a_Manifold;
                                             the_World    : in World.view);

   procedure add_impact_Response (Self : in out Item;   Filter   : in impact_Filter;
                                                        Response : in impact_Response);

   --------------
   --- Operations
   --

   procedure is_a_Mirror          (Self : access Item'Class;   of_World : in gel.remote.World.view);

   procedure add                  (Self : in out Item;   the_Model    : in openGL .Model.view);
   procedure add                  (Self : in out Item;   the_Model    : in physics.Model.view);

   procedure add                  (Self : access Item;   the_Sprite   : in gel.Sprite.view;
                                                         and_Children : in Boolean := False);

   procedure add                  (Self : in out Item;   the_Joint    : in gel.Joint.view);

   procedure rid                  (Self : in out Item;   the_Sprite   : in gel.Sprite.view;
                                                         and_Children : in Boolean := False);
   procedure rid                  (Self : in out Item;   the_Joint    : in gel.Joint.view);

   procedure start                (Self : access Item);
   procedure evolve               (Self : in out Item;   By           : in Duration);

   procedure  allow_broken_Joints (Self :    out Item);
   procedure handle_broken_Joints (Self : in out Item;   the_Joints   : in Joint.views);
   --
   -- Detaches any broken joints from associated sprites.
   -- Override this to do custom handling of broken joints.
   -- tbd: This should be in private section and only available to child packages.


   ---------------
   --- Ray Casting
   --

   type ray_Collision is
      record
         near_Sprite  : gel.Sprite.view;
         hit_Fraction : Real;
         Normal_world : Vector_3;
         Site_world   : Vector_3;
      end record;


   type Any_limited_view is access all lace.Any.limited_item'Class;

   type raycast_collision_Event is new lace.Event.item with
      record
         near_Sprite : gel.Sprite.view;
         Context     : Any_limited_view;
         Site_world  : Vector_3;
      end record;

   overriding
   procedure destruct (Self : in out raycast_collision_Event);


   type no_Parameters is null record;

   function to_raycast_collision_Event (Params : not null access no_Parameters) return raycast_collision_Event;

   function raycast_collision_Event_dispatching_Constructor is new ada.Tags.generic_dispatching_Constructor (raycast_collision_Event,
                                                                                                             Parameters  => no_Parameters,
                                                                                                             Constructor => to_raycast_collision_Event);
   procedure cast_Ray (Self : in Item;   From, To   : in     Vector_3;
                                         Observer   : in     lace.Observer.view;
                                         Context    : access lace.Any.limited_Item'Class;
                                         Event_Kind : in     raycast_collision_Event'Class);
   --
   -- Casts a ray between From and To.
   -- The Observer is informed of the 1st collision with a Sprite via a raycast_collision_Event.
   -- Context is optional and is passed back to the Observer within the Context field of the raycast_collision_Event
   -- for use by the raycast_collision_Event response.


   --------------------
   ---  World Mirroring
   --

   overriding
   procedure   register (Self : access Item;   the_Mirror         : in remote.World.view;
                                               Mirror_as_observer : in lace.Observer.view);
   overriding
   procedure deregister (Self : access Item;   the_Mirror         : in remote.World.view);


   overriding
   procedure motion_Updates_are (Self : in Item;   Now : in remote.World.motion_Updates);
   --
   --  'Self' must use 'in' as mode to ensure async transmission with DSA.


   overriding
   function  graphics_Models (Self : in Item) return remote.World.graphics_Model_Set;
   overriding
   function  physics_Models  (Self : in Item) return remote.World.physics_Model_Set;
   overriding
   function  Sprites         (Self : in Item) return remote.World.sprite_model_Pairs;


   ----------
   --- Models
   --

   --  Graphics Models
   --
   use type openGL.Model.view;
   use type gel.graphics_model_Id;
   function Hash             is new ada.unchecked_Conversion   (gel.graphics_model_Id,  ada.Containers.Hash_type);
   package  id_Maps_of_model is new ada.Containers.hashed_Maps (gel.graphics_model_Id,  openGL.Model.view,
                                                                Hash,                   "=");

   function local_graphics_Models (Self : in Item) return id_Maps_of_model.Map;


   --  Physics Models
   --
   use type Standard.physics.Model.view,
            Standard.physics.model_Id;
   function Hash                     is new ada.unchecked_Conversion   (physics.model_Id,  ada.Containers.Hash_type);
   package  id_Maps_of_physics_model is new ada.Containers.hashed_Maps (physics.model_Id,  physics.Model.view,
                                                                        Hash,              "=");

   function local_physics_Models (Self : in Item) return id_Maps_of_physics_model.Map;


   ---------
   ---  Misc
   --
   procedure wait_on_Evolve (Self : in out Item);


   ------------------
   ---  Testing/Debug
   --
   overriding
   procedure kick_Sprite (Self : in out Item;   sprite_Id : in gel.Sprite_Id);



private

   ----------
   --- Engine
   --

   --  task
   --  type Engine (the_World : access gel.World.item'Class)
   --  is
   --     entry start (space_Kind : in physics.space_Kind);
   --     entry stop;
   --
   --     entry reset_Age;
   --
   --     pragma storage_Size (20_000_000);
   --  end Engine;
   --
   --  type Engine_view is access all Engine;


   -----------------
   --- Signal Object
   --
   protected
   type signal_Object
   is
      entry     wait;
      procedure signal;

   private
      Open : Boolean := False;
   end signal_Object;

   type signal_Object_view is access all signal_Object;


   ----------------------------
   --- sprite_transform_Updater
   --

   --  task
   --  type sprite_transform_Updater (the_World : access gel.World.item'Class)
   --  is
   --     entry stop;
   --  end sprite_transform_Updater;
   --
   --  type sprite_transform_Updater_view is access all sprite_transform_Updater;


   ---------------------
   --- id_Maps_of_sprite
   --
   use type Sprite.view;
   function Hash              is new ada.unchecked_Conversion   (gel.sprite_Id, ada.Containers.Hash_type);
   package  id_Maps_of_sprite is new ada.Containers.hashed_Maps (gel.sprite_Id,  gel.Sprite.view,
                                                                 Hash            => Hash,
                                                                 equivalent_Keys => "=");
   -----------------------------
   --- sprite_Maps_of_transforms
   --
   function Hash is new ada.unchecked_Conversion (gel.Sprite.view, ada.Containers.Hash_type);
   package  sprite_Maps_of_transforms is new ada.Containers.hashed_Maps (Sprite.view,  Matrix_4x4,
                                                                         Hash            => Hash,
                                                                         equivalent_Keys => "=");
   -------------------------
   --- all_sprite_Transforms
   --
   protected
   type all_sprite_Transforms
   is
      procedure add (the_Sprite : in Sprite.view;
                     Transform  : in Matrix_4x4);

      procedure set (To : in sprite_Maps_of_transforms.Map);
      function  fetch return sprite_Maps_of_transforms.Map;

   private
      sprite_Map_of_transforms : sprite_Maps_of_transforms.Map;
   end all_sprite_Transforms;


   -----------------
   --- Duration_safe
   --
   protected
   type Duration_safe
   is
      procedure Duration_is (Now : in Duration);
      function  Duration       return Duration;

   private
      the_Duration : standard.Duration;
   end Duration_safe;


   -----------
   --- Mirrors
   --
   use type remote.World.View;

   package world_Vectors is new ada.Containers.Vectors (Positive, remote.World.view);
   subtype world_Vector  is     world_Vectors.Vector;


   -------------------
   --- Engine Commands
   --

   --  type command_Kind is (add_Sprite,
   --                        rid_Sprite,
--  --                           scale_Sprite,
--                           destroy_Sprite,
--  --                           update_Bounds,
--                           update_Site,
--  --                           set_Speed,
--  --                           apply_Force,
--  --                           set_xy_Spin,
--  --                           add_Joint,
--  --                           rid_Joint,
--  --                           set_Joint_local_Anchor,
--                           free_Joint,
--                           cast_Ray,
--                           new_impact_Response);
--  --                           set_Gravity);
--
--     type Command (Kind : command_Kind := command_Kind'First) is
--        record
--           Sprite : gel.Sprite.view;
--
--           case Kind
--           is
--              when add_Sprite =>
--                 add_Children : Boolean;
--
--              when rid_Sprite =>
--                 rid_Children : Boolean;
--
--              when update_Site =>
--                 Site : Vector_3;
--
--  --              when scale_Sprite =>
--  --                 Scale  : math.Vector_3;
--
--  --              when apply_Force =>
--  --                 Force  : math.Vector_3;
--
--  --              when set_Speed =>
--  --                 Speed  : math.Vector_3;
--
--  --              when set_xy_Spin =>
--  --                 xy_Spin : math.Radians;
--
--              when --add_Joint |
--  --                   rid_Joint |
--                   free_Joint =>
--                 Joint : gel.Joint.view;
--
--  --              when set_Joint_local_Anchor =>
--  --                 anchor_Joint : gel.Joint.view;
--  --                 is_Anchor_A  : Boolean;         -- When false, is anchor B.
--  --                 local_Anchor : math.Vector_3;
--
--              when cast_Ray =>
--                 From, To : Vector_3;
--                 Observer : lace.Observer.view;
--                 Context  : Any_limited_view;
--                 event_Kind : ada.Tags.Tag;
--
--              when new_impact_Response =>
--                 Filter   : impact_Filter;
--                 Response : impact_Response;
--
--  --              when set_Gravity =>
--  --                 Gravity : math.Vector_3;
--
--              when others =>
--                 null;
--           end case;
--        end record;
--
--     type Commands is array (Positive range 1 .. 200_000) of Command;
--
--
--     protected
--     type safe_command_Set
--     is
--        function  is_Empty return Boolean;
--
--        procedure add   (the_Command : in     Command);
--        procedure Fetch (To          :    out Commands;
--                         Count       :    out Natural);
--     private
--        Set       : Commands;
--        the_Count : Natural := 0;
--     end safe_command_Set;
--
--     type safe_command_Set_view is access all safe_command_Set;


   type free_Set is
      record
         Sprites  : gel.Sprite.views (1 .. 10_000);
         Count    : Natural := 0;
      end record;

   type free_Sets is array (1 .. 2) of free_Set;


   ---------------
   --- safe_Joints
   --

   subtype safe_Joints is gel.Joint.views (1 .. 10_000);

   protected
   type safe_joint_Set
   is
      function  is_Empty return Boolean;

      procedure add   (the_Joint : in     gel.Joint.view);
      procedure Fetch (To        :    out safe_Joints;
                       Count     :    out Natural);
   private
      Set       : safe_Joints;
      the_Count : Natural := 0;
   end safe_joint_Set;


   --------------
   --- World Item
   --

   -- TODO: refactor into two subclasses 'local' and 'mirror'.

   type Item is limited new lace.Subject_and_deferred_Observer.item
                        and gel.remote.World.item with
      record
         local_Subject_and_deferred_Observer : lace.Subject_and_deferred_Observer.view;

         Id : world_Id;

         space_Kind      :         physics.space_Kind;
         physics_Space   : aliased physics.Space.view;
         --  physics_Engine  : aliased physics.Engine.item;

         Renderer        : access  openGL.Renderer.lean.item'Class;         -- Is *not* owned by Item.

         Age             :         Duration := 0.0;

         graphics_Models : aliased id_Maps_of_model        .Map;
         physics_Models  : aliased id_Maps_of_physics_model.Map;

         Sprites         : gel.Sprite.views (1 .. 100_000);
         sprite_Count    : Index;

         all_sprite_Transforms           : World.all_sprite_Transforms;
         new_sprite_transforms_Available : Signal_Object;
         --  sprite_transform_Updater        : World.sprite_transform_Updater (Item'Access);

         evolver_Done : Signal_Object;

         --  Mirrors
         --
         is_a_Mirror                : Boolean  := False;
         Age_at_last_mirror_update  : Duration := 0.0;
         Mirrors                    : World_vector;             -- Used by a master world.
         id_Map_of_Sprite           : id_Maps_of_sprite.Map;

         --  Ids
         --
         last_used_sprite_Id        : gel.sprite_Id         := 0;
         last_used_model_Id         : gel.graphics_model_Id := 0;
         last_used_physics_model_Id : physics     .model_Id := 0;

         --  Command sets
         --
         --  Commands         : safe_command_Set_view := new safe_command_Set;
         free_Sets        : World.free_Sets;
         current_free_Set : Integer := 2;

         --  Collisions
         --
         Manifolds      : Manifold_array (1 .. 50_000);
         manifold_Count : Natural := 0;

         -- Broken Joints
         --
         broken_Joints         : safe_joint_Set;
         broken_joints_Allowed : Boolean := False;

         --  Engine
         --
         --  Engine : World.Engine (Item'Access);
      end record;


end gel.World;
