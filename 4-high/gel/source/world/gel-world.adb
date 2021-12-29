with
     gel.Events,

     physics.Object,
     physics.Forge,

     openGL.Renderer.lean,

     lace.Response,

     ada.Text_IO,
     ada.Exceptions,
     ada.unchecked_Deallocation,
     ada.Containers.hashed_Sets;


package body gel.World
is
   use gel.Sprite,
       linear_Algebra_3D,

       --  lace.Event,

       ada.Exceptions,
       ada.Text_IO;


   procedure log (Message : in String) renames ada.Text_IO.put_Line;


   ---------
   --- Forge
   --

   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      deallocate (Self);
   end free;

   procedure free is new ada.unchecked_Deallocation (lace.Any.limited_item'Class, Any_limited_view);


   procedure define (Self : in out Item'Class;   Name       : in     String;
                                                 Id         : in     world_Id;
                                                 space_Kind : in     physics.space_Kind;
                                                 Renderer   : access openGL.Renderer.lean.item'Class);

   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      --  Free record components.
      --
      physics.Space.free (Self.physics_Space);

      lace.Subject_and_deferred_Observer.item (Self).destroy;     -- Destroy base class.
      lace.Subject_and_deferred_Observer.free (Self.local_Subject_and_deferred_Observer);
   end destroy;



   function local_Observer (Self : in Item) return lace.Observer.view
   is
   begin
      return lace.Observer.view (Self.local_Subject_and_deferred_Observer);
   end local_Observer;



   function local_Subject (Self : in Item) return lace.Subject.view
   is
   begin
      return lace.Subject.view (Self.local_Subject_and_deferred_Observer);
   end local_Subject;



   function Id (Self : in Item) return world_Id
   is
   begin
      return Self.Id;
   end Id;



   function to_Sprite (the_Pair           : in remote.World.sprite_model_Pair;
                       the_Models         : in Id_Maps_of_Model        .Map;
                       the_physics_Models : in Id_Maps_of_physics_Model.Map;
                       the_World          : in gel.World.view) return gel.Sprite.view
   is
      the_graphics_Model : access openGL .Model.item'Class;
      the_physics_Model  : access physics.Model.item'Class;
      the_Sprite         :        gel.Sprite.view;

      use openGL;
   begin
      the_graphics_Model := openGL .Model.view (the_Models        .Element (the_Pair.graphics_Model_Id));
      the_physics_Model  := physics.Model.view (the_physics_Models.Element (the_Pair. physics_Model_Id));

      the_Sprite := gel.Sprite.forge.new_Sprite ("Sprite" & the_Pair.sprite_Id'Image,
                                                 sprite.World_view (the_World),
                                                 get_Translation (the_Pair.Transform),
                                                 the_graphics_Model,
                                                 the_physics_Model,
                                                 owns_Graphics => False,
                                                 owns_Physics  => False,
                                                 is_Kinematic  => the_Pair.Mass /= 0.0);

      the_Sprite.Id_is      (Now => the_Pair.sprite_Id);
      the_Sprite.is_Visible (Now => the_Pair.is_Visible);

      the_Sprite.Site_is    (get_Translation (the_Pair.Transform));
      the_Sprite.Spin_is    (get_Rotation    (the_Pair.Transform));

      the_Sprite.desired_Dynamics_are (Site => the_Sprite.Site,
                                       Spin => to_Quaternion (get_Rotation (the_Sprite.Transform)));
      return the_Sprite;
   end to_Sprite;


   --------------------------------
   --- 'create_new_Sprite' Response
   --

   type create_new_Sprite is new lace.Response.item with
      record
         World          :        gel.World.view;
         Models         : access id_Maps_of_model        .Map;
         physics_Models : access id_Maps_of_physics_model.Map;
      end record;


   overriding
   function Name (Self : in create_new_Sprite) return String;



   overriding
   procedure respond (Self : in out create_new_Sprite;   to_Event : in lace.Event.item'Class)
   is
   begin
      declare
         the_Event  : constant gel.Events.new_sprite_Event := gel.Events.new_sprite_Event (to_Event);
         the_Sprite : constant gel.Sprite.view             := to_Sprite (the_Event.Pair,
                                                                         Self.Models.all,
                                                                         Self.physics_Models.all,
                                                                         Self.World);
      begin
         Self.World.add (the_Sprite, and_children => False);
      end;
   end respond;



   procedure define (Self : in out create_new_Sprite;    World  : in     gel.World.view;
                                                         Models : access id_Maps_of_model.Map)
   is
   begin
      Self.World  := World;
      Self.Models := Models;
   end define;



   overriding
   function Name (Self : in create_new_Sprite) return String
   is
      pragma Unreferenced (Self);
   begin
      return "create_new_Sprite";
   end Name;


   ----------
   --- Define
   --

   procedure define  (Self : in out Item'Class;   Name       : in     String;
                                                  Id         : in     world_Id;
                                                  space_Kind : in     physics.space_Kind;
                                                  Renderer   : access openGL.Renderer.lean.Item'Class)
   is
      use lace.Subject_and_deferred_Observer.Forge;
   begin
      Self.local_Subject_and_deferred_Observer := new_Subject_and_Observer (name => Name & " world" & Id'Image);

      Self.Id            := Id;
      Self.space_Kind    := space_Kind;
      Self.Renderer      := Renderer;
      Self.physics_Space := physics.Forge.new_Space (space_Kind);
   end define;


   -------------------------
   --- all_sprite_Transforms
   --

   function to_Integer is new ada.unchecked_Conversion (gel.Sprite.view, Integer);


   protected
   body all_sprite_Transforms
   is
      procedure add (the_Sprite : in Sprite.view;
                     Transform  : in Matrix_4x4)
      is
      begin
         sprite_Map_of_transforms.insert (the_Sprite, Transform);
      end add;


      procedure set (To : in sprite_Maps_of_transforms.Map)
      is
      begin
         sprite_Map_of_transforms := To;
      end set;


      function fetch return sprite_Maps_of_transforms.Map
      is
      begin
         return sprite_Map_of_transforms;
      end Fetch;

   end all_sprite_Transforms;


   -----------------
   --- Duration_safe
   --

   protected
   body Duration_safe
   is
      procedure Duration_is (Now : in standard.Duration)
      is
      begin
         the_Duration := Now;
      end Duration_is;

      function Duration return standard.Duration
      is
      begin
         return the_Duration;
      end Duration;

   end Duration_safe;


   --------------------
   --- Breakable Joints
   --

   protected body safe_joint_Set
   is
      function is_Empty return Boolean
      is
      begin
         return the_Count = 0;
      end is_Empty;


      procedure add (the_Joint : in gel.Joint.view)
      is
      begin
         the_Count       := the_Count + 1;
         Set (the_Count) := the_Joint;
      end add;


      procedure fetch (To    : out safe_Joints;
                       Count : out Natural)
      is
      begin
         To (1 .. the_Count) := Set (1 .. the_Count);
         Count               := the_Count;
         the_Count           := 0;
      end Fetch;

   end safe_joint_Set;


   --------------
   --- Collisions
   --

   task
   type impact_Responder
   is
      entry start (the_World      : in gel.World.view;
                   Filter         : in impact_Filter;
                   Response       : in impact_Response;
                   responses_Done : in Signal_Object_view);
      entry stop;
      entry respond;     -- Filter and do responses.
   end impact_Responder;


   type impact_Responder_view is access all impact_Responder;

   procedure free (Self : in out impact_Responder_view)
   is
      procedure deallocate is new ada.unchecked_Deallocation (impact_Responder, impact_Responder_view);
   begin
      deallocate (Self);
   end free;



   type filtered_impact_Response is
      record
         Filter    : impact_Filter;
         Response  : impact_Response;

         Responder : impact_Responder_view;

         responses_Done : access Signal_Object := new Signal_Object;
      end record;

   function Hash (Self : in filtered_impact_Response) return ada.Containers.Hash_type;

   package filtered_impact_Response_Sets is new ada.Containers.hashed_Sets (filtered_impact_Response,
                                                                            Hash,  "=");


   protected body Signal_Object
   is
      entry Wait
        when Open
      is
      begin
         Open := False;
      end Wait;

      procedure Signal
      is
      begin
         Open := True;
      end Signal;
   end Signal_Object;



   function local_graphics_Models (Self : in Item) return id_Maps_of_model.Map
   is
   begin
      return Self.graphics_Models;
   end local_graphics_Models;



   function local_physics_Models (Self : in Item) return id_Maps_of_physics_model.Map
   is
   begin
      return Self.physics_Models;
   end local_physics_Models;


   --------------
   --- Attributes
   --

   function space_Kind (Self : in Item) return physics.space_Kind
   is
   begin
      return Self.space_Kind;
   end space_Kind;



   function Space (Self : in Item) return physics.Space.view
   is
   begin
      return Self.physics_Space;
   end Space;



   procedure update_Bounds (Self : in out Item;   of_Sprite : in gel.Sprite.view)
   is
   begin
      null;     -- TODO
      --  Self.physics_Engine.update_Bounds (of_Sprite.Solid);
   end update_Bounds;



   procedure update_Site (Self : in out Item;   of_Sprite : in gel.Sprite.view;   -- TODO: Probably obsolete.
                                                To        : in Vector_3)
   is
   begin
      of_Sprite.Solid.Site_is (To);

      --      Self.physics_Engine.update_Site (of_Sprite.Solid, To);
   end update_Site;



   procedure set_Speed (Self : in out Item;   of_Sprite : in gel.Sprite.view;     -- TODO: Probably obsolete.
                                              To        : in Vector_3)
   is
   begin
      null;
      --  Self.physics_Engine.set_Speed (of_Sprite.Solid, To);
   end set_Speed;



   procedure set_xy_Spin (Self : in out Item;   of_Sprite : in gel.Sprite.view;     -- TODO: Probably obsolete.
                                                To        : in Radians)
   is
   begin
      of_Sprite.Solid.xy_Spin_is (To);

      --  Self.physics_Engine.set_xy_Spin (of_Sprite.Solid, To);
   end set_xy_Spin;



   procedure update_Scale (Self : in out Item;   of_Sprite : in gel.Sprite.view;     -- TODO: Probably obsolete.
                                                 To        : in Vector_3)
   is
   begin
      null;
      --  Self.physics_Engine.update_Scale (of_Sprite.Solid, To);

--        Self.physics_Engine.add (std_Physics.Engine.Command' (Kind   => scale_Object,
--                                                              Sprite => the_Command.Sprite.Solid,
--                                                              Scale  => the_Command.Scale));
   end update_Scale;



   procedure apply_Force (Self : in out Item;   to_Sprite : in gel.Sprite.view;     -- TODO: Probably obsolete.
                                                Force     : in Vector_3)
   is
   begin
      null;
      --  Self.physics_Engine.apply_Force (to_Sprite.Solid, Force);
   end apply_Force;



   function Age (Self : in Item) return Duration
   is
   begin
      return Self.Age;
   end Age;



   procedure Age_is (Self : in out Item;   Now : in Duration)
   is
   begin
      Self.Age := Now;
   end Age_is;



   procedure Gravity_is (Self : in out Item;   Now : in Vector_3)
   is
   begin
      Self.physics_Space.Gravity_is (Now);
   end Gravity_is;



   procedure cast_Ray (Self : in Item;   From, To   : in     Vector_3;
                                         Observer   : in     lace.Observer.view;
                                         Context    : access lace.Any.limited_item'Class;
                                         event_Kind : in     raycast_collision_Event'Class)
   is
   begin
      null;
      --  Self.Commands.add ((Kind     => cast_Ray,
      --                      Sprite   => null,
      --                      From     => From,
      --                      To       => To,
      --                      Observer => Observer,
      --                      Context  => Context,
      --                      event_Kind => event_Kind'Tag));
   end cast_Ray;


   --------------
   --- Collisions
   --

   function manifold_Count (Self : in Item) return Natural
   is
   begin
      return Self.manifold_Count;
   end manifold_Count;



   function Manifold (Self : in Item;   Index : in Positive) return a_Manifold
   is
   begin
      return Self.Manifolds (Index);
   end Manifold;



   function Manifolds (Self : in Item) return Manifold_array
   is
   begin
      return Self.Manifolds (1 .. Self.manifold_Count);
   end Manifolds;


   -----------
   --- Sprites
   --

   function new_sprite_Id (Self : access Item) return sprite_Id
   is
   begin
      Self.last_used_sprite_Id := Self.last_used_sprite_Id + 1;

      return Self.last_used_sprite_Id;
   end new_sprite_Id;



   procedure destroy (Self : in out Item;   the_Sprite : in gel.Sprite.view)
   is
   begin
      null;     -- TODO
      --  Self.Commands.add ((Kind   => destroy_Sprite,
      --                      Sprite => the_Sprite));
   end destroy;



   function free_sprite_Set (Self : access Item) return gel.Sprite.views
   is
      prior_set_Index : Integer;
   begin
      if Self.current_free_Set = 1
      then   prior_set_Index := 2;
      else   prior_set_Index := 1;
      end if;

      declare
         the_Set : constant gel.Sprite.views
           := Self.free_Sets (prior_set_Index).Sprites (1 .. Self.free_Sets (prior_set_Index).Count);
      begin
         Self.free_Sets (prior_set_Index).Count := 0;
         Self.current_free_Set := prior_set_Index;

         return the_Set;
      end;
   end free_sprite_Set;



   function fetch_Sprite (Self : in out Item'Class;   Id : in sprite_Id) return gel.Sprite.view
   is
   begin
      return Self.all_Sprites.fetch.Element (Id);
   end fetch_Sprite;



   procedure set_Scale (Self : in out Item;   for_Sprite : in gel.Sprite.view;
                                              To         : in Vector_3)
   is
      Pad : constant Vector_3 := for_Sprite.Site;
   begin
      Self.rid (for_Sprite, and_children => False);
      for_Sprite.Scale_is (To);
      Self.add (for_Sprite, and_children => False);

      for_Sprite.Site_is (Pad);   -- TODO: Fix this hack !
   end set_Scale;



   function sprite_Transforms (Self : in out Item'Class) return sprite_transform_Pairs
   is
      use id_Maps_of_sprite;

      all_Sprites : constant  id_Maps_of_sprite.Map    := Self.all_Sprites.fetch;
      Cursor      :           id_Maps_of_sprite.Cursor := all_Sprites.First;

      the_sprite_Transforms : sprite_transform_Pairs (1 .. Natural (all_Sprites.Length)) := (others => <>);
      Count                 : Natural := 0;

      the_Sprite : Sprite.view;

   begin
      while has_Element (Cursor)
      loop
         the_Sprite := Element (Cursor);

         if not the_Sprite.is_Destroyed
         then
            Count                         := Count + 1;
            the_sprite_Transforms (Count) := (Sprite    => the_Sprite,
                                              Transform => the_Sprite.Transform);
         end if;

         next (Cursor);
      end loop;

      return the_sprite_Transforms (1 .. Count);
   end sprite_Transforms;


   ----------
   --- Joints
   --

   procedure destroy (Self : in out Item;   the_Joint : in gel.Joint.view)
   is
   begin
      null;     -- TODO
      --  Self.Commands.add ((kind   => free_Joint,
      --                      sprite => null,
      --                      joint  => the_Joint));
   end destroy;



   procedure set_local_Anchor_on_A (Self : in out Item;   for_Joint : in gel.Joint.view;
                                                          To        : in Vector_3)
   is
   begin
      null;     -- TODO
      --  Self.physics_Engine.set_local_Anchor (for_Joint.Physics.all'Access,
      --                                        to          => To,
      --                                        is_Anchor_A => True);

--        the_World.physics_Space.set_Joint_local_Anchor (the_Command.anchor_Joint.Physics.all'Access,
--                                                        the_Command.is_Anchor_A,
--                                                        the_Command.local_Anchor);
--
--
--        Self.Commands.add ((Kind         => set_Joint_local_Anchor,
--                            Sprite       => null,
--                            anchor_Joint => for_Joint,
--                            is_Anchor_A  => True,
--                            local_Anchor => To));
   end set_local_Anchor_on_A;



   procedure set_local_Anchor_on_B (Self : in out Item;   for_Joint : in gel.Joint.view;
                                                          To        : in Vector_3)
   is
   begin
      null;     -- TODO
      --  Self.physics_Engine.set_local_Anchor (for_Joint.Physics.all'Access,
      --                                        To          => To,
      --                                        is_Anchor_A => False);

--        Self.Commands.add ((Kind         => set_Joint_local_Anchor,
--                            Sprite       => null,
--                            anchor_Joint => for_Joint,
--                            is_Anchor_A  => False,
--                            local_Anchor => To));
   end set_local_anchor_on_B;



--     type graphics_Model_iface_view is access all openGL.remote_Model.item'Class;
--     type graphics_Model_view       is access all openGL.       Model.item'Class;
--
--     type physics_Model_iface_view is access all Standard.physics.remote.Model.item'Class;
--     type physics_Model_view       is access all Standard.physics.Model       .item'Class;


   procedure add (Self : access Item;   the_Sprite   : in gel.Sprite.view;
                                        and_Children : in Boolean := False)
   is
      procedure add_single_Sprite (Single : in out Sprite.item'Class)
      is
      begin
         if Single.Id = null_sprite_Id
         then
            raise Error with "Null sprite detected.";
         end if;

         Self.add (Single.graphics_Model);
         Self.add (Single. physics_Model);

         Single.Solid.user_Data_is (Single'Access);
         Single.Solid.    Model_is (Single.physics_Model);

         if Single.physics_Model.is_Tangible
         then
            Self.physics_Space.add (physics.Object.view (Single.Solid));
         end if;

         Item'Class (Self.all).all_Sprites.add (Single'unchecked_Access);
      end add_single_Sprite;

   begin
      pragma assert (the_Sprite.World = Self, "Trying to add sprite to the wrong world.");     -- TODO: Use an exception.

      if and_Children
      then
         declare
            procedure add_the_Joint (the_Sprite : in out Sprite.item'Class)
            is
               use type gel.Joint.view;
               the_Joint : constant gel.Joint.view := the_Sprite.parent_Joint;
            begin
               if the_Joint /= null
               then
                  Self.physics_Space.add         (the_Joint.Physics.all'Access);
                  the_Joint.Physics.user_Data_is (the_Joint);
               end if;
            end add_the_Joint;

         begin
            the_Sprite.apply (add_single_Sprite'unrestricted_Access);
            the_Sprite.apply (add_the_Joint    'unrestricted_Access);
         end;
      else
         add_single_Sprite (the_Sprite.all);
      end if;
   end add;



   procedure rid (Self : in out Item'Class;   the_Sprite   : in gel.Sprite.view;
                                              and_Children : in Boolean := False)
   is
      procedure rid_single_Sprite (Single : in out Sprite.item'Class)
      is
      begin
         --  Self.Commands.add ((Kind         => rid_Sprite,
         --                      Sprite       => the_Sprite'unchecked_Access,
         --                      rid_Children => False));

         Self.all_Sprites.rid (Single'unchecked_Access);     -- TODO: Handle grandchildren and so on.
      end rid_single_Sprite;

   begin
      if and_Children
      then
         the_Sprite.apply (rid_single_Sprite'unrestricted_Access);
      else
         rid_single_Sprite (the_Sprite.all);
      end if;
   end rid;



   procedure add (Self : in out Item;   the_Model : in openGL.Model.view)
   is
   begin
      if the_Model.Id = null_graphics_model_Id
      then
         Self.last_used_model_Id := Self.last_used_model_Id + 1;
         the_Model.Id_is (Self.last_used_model_Id);
      end if;

      if not Self.graphics_Models.contains (the_Model.Id)
      then
         Self.graphics_Models.insert (the_Model.Id, the_Model);

         --  Emit a new model event.
         --
         declare
            the_Event : remote.World.new_model_Event;
         begin
            the_Event.Model := the_Model;
            Self.emit (the_Event);
         end;
      end if;
   end add;



   procedure add (Self : in out Item;   the_Model : in physics.Model.view)
   is
   begin
      if the_Model.Id = Physics.null_model_Id
      then
         Self.last_used_physics_model_Id := Self.last_used_physics_model_Id + 1;
         the_Model.Id_is (Self.last_used_physics_model_Id);
      end if;

      if not Self.physics_Models.contains (the_Model.Id)
      then
         Self.physics_Models.insert (the_Model.Id, the_Model);
      end if;
   end add;



   procedure add (Self : in out Item;   the_Joint : in gel.Joint.view)
   is
   begin
      Self.physics_Space.add         (the_Joint.Physics.all'Access);
      the_Joint.Physics.user_Data_is (the_Joint);
   end add;



   procedure rid (Self : in out Item;   the_Joint : in gel.Joint.view)
   is
   begin
      null;     -- TODO
      --  Self.physics_Engine.rid (the_Joint.Physics.all'Access);

--        Self.Commands.add ((kind   => rid_Joint,
--                            sprite => null,
--                            joint  => the_Joint));
   end rid;



   --------------
   --- Operations
   --

   procedure start (Self : access Item)
   is
   begin
      null;
   end start;


   --------------------
   ---  World Mirroring
   --

   overriding
   procedure   register (Self : access Item;   the_Mirror         : in remote.World.view;
                                               Mirror_as_observer : in lace.Observer.view) is null;
   overriding
   procedure deregister (Self : access Item;   the_Mirror         : in remote.World.view) is null;

   overriding
   procedure motion_Updates_are (Self : in Item;   Now : in remote.World.motion_Updates) is null;


   ----------
   --- Joints
   --

   procedure allow_broken_Joints (Self : out Item)
   is
   begin
      Self.broken_joints_Allowed := True;
   end allow_broken_Joints;



   procedure handle_broken_Joints (Self : in out Item;   the_Joints :in Joint.views)
   is
   begin
      for i in the_Joints'Range
      loop
         begin
            if         (    the_Joints (i).Sprite_A /= null
                        and the_Joints (i).Sprite_B /= null)
              and then (    not the_Joints (i).Sprite_A.is_Destroyed
                        and not the_Joints (i).Sprite_B.is_Destroyed)
            then
               begin
                  the_Joints (i).Sprite_A.detach (the_Joints (i).Sprite_B);
               exception
                  when no_such_Child =>
                     put_Line ("handle_broken_Joints: Cannot detach sprite:  no_such_Child." );
               end;
            end if;

         exception
            when storage_Error =>
               put_Line ("handle_broken_Joints: Cannot tell if sprite exists:  storage_Error." );
         end;
      end loop;
   end handle_broken_Joints;



   procedure evolve (Self : in out Item)
   is
   begin
      Self.Age := Self.Age + evolve_Period;

      --  Evolve the physics.
      --
      Self.physics_Space.evolve (evolve_Period);     -- Evolve the physics space.

      --  Handle evnts.
      --
      Self.respond;
      Self.local_Subject_and_deferred_Observer.respond;

      -- Broken joints.
      --
      declare
         the_Joints : safe_Joints;
         Count      : Natural;
      begin
         Self.broken_Joints.fetch  (the_Joints, Count);
         Self.handle_broken_Joints (the_Joints (1 .. Count));
      end;

      --  Perform responses to events for all sprites.
      --
      declare
         use id_Maps_of_sprite;

         all_Sprites : constant id_Maps_of_sprite.Map    := Item'Class (Self).all_Sprites.fetch;
         Cursor      :          id_Maps_of_sprite.Cursor := all_Sprites.First;
         the_Sprite  :          Sprite.view;
      begin
         while has_Element (Cursor)
         loop
            the_Sprite := Element (Cursor);

            begin
               if not the_Sprite.is_Destroyed
               then
                  the_Sprite.respond;
               end if;

            exception
               when E : others =>
                  new_Line (2);
                  put_Line ("Error in 'gel.World.evolve' sprite response.");
                  new_Line;
                  put_Line (ada.Exceptions.exception_Information (E));
                  new_Line (2);
            end;

            next (Cursor);
         end loop;
      end;

   end evolve;



   overriding
   function graphics_Models (Self : in Item) return remote.World.graphics_Model_Set
   is
      use id_Maps_of_model;

      the_Models  : remote.World.graphics_Model_Set;
      Cursor      : id_Maps_of_model.Cursor := Self.graphics_Models.First;
   begin
      while has_Element (Cursor)
      loop
         the_Models.include (Element (Cursor).Id,
                             Element (Cursor).all);
         next (Cursor);
      end loop;

      return the_Models;
   end graphics_Models;



   overriding
   function physics_Models (Self : in Item) return remote.World.physics_model_Set
   is
      use id_Maps_of_physics_model;

      the_Models  : remote.World.physics_model_Set;
      Cursor      : id_Maps_of_physics_model.Cursor := Self.physics_Models.First;
   begin
      while has_Element (Cursor)
      loop
         the_Models.include (Element (Cursor).Id,
                             Element (Cursor).all);
         next (Cursor);
      end loop;

      return the_Models;
   end physics_Models;



   overriding
   function Sprites (Self : in out Item) return remote.World.sprite_model_Pairs
   is
      use id_Maps_of_sprite;

      all_Sprites : constant id_Maps_of_sprite.Map    := Item'Class (Self).all_Sprites.fetch;
      Cursor      :          id_Maps_of_sprite.Cursor := all_Sprites.First;
      the_Pairs   :          remote.World.sprite_model_Pairs (1 .. Natural (all_Sprites.Length));
      the_Sprite  :          Sprite.view;
      i           :          Natural := 0;
   begin
      while has_Element (Cursor)
      loop
         i          := i + 1;
         the_Sprite := Element (Cursor);

         the_Pairs (i) := (sprite_Id         => the_Sprite.Id,
                           graphics_model_Id => the_Sprite.graphics_Model.Id,
                           physics_model_Id  => the_Sprite. physics_Model.Id,
                           Mass              => the_Sprite.Mass,
                           Transform         => the_Sprite.Transform,
                           is_Visible        => the_Sprite.is_Visible);
         next (Cursor);
      end loop;

      return the_Pairs;
   end Sprites;



   --------------
   --- Collisions
   --

   function Hash (Self : in filtered_impact_Response) return ada.Containers.Hash_type
   is
      use type ada.Containers.Hash_type;

      function to_Hash is new ada.unchecked_Conversion (impact_Filter,   ada.Containers.Hash_type);
      function to_Hash is new ada.unchecked_Conversion (impact_Response, ada.Containers.Hash_type);
   begin
      return   to_Hash (Self.Filter)
             + to_Hash (Self.Response);
   end Hash;



   procedure add_impact_Response (Self : in out Item;   Filter   : in impact_Filter;
                                                        Response : in impact_Response)
   is
   begin
      null;   -- TODO
      --  Self.Commands.add ((new_impact_Response,
      --                      null,
      --                      Filter,
      --                      Response));
   end add_impact_Response;



   task body impact_Responder
   is
      the_World : gel.World.view;
      Done      : Boolean := False;

      Filters_through : impact_Filter;
      the_Response    : impact_Response;

      the_responses_Done : access Signal_Object;

   begin
      accept start (the_World      : in gel.World.view;
                    Filter         : in impact_Filter;
                    Response       : in impact_Response;
                    responses_Done : in Signal_Object_view)
      do
         impact_Responder.the_World := the_World;
         Filters_through            := Filter;
         the_Response               := Response;
         the_responses_Done         := responses_Done;
      end start;

      loop
         begin
            select
               accept stop
               do
                  Done := True;
               end stop;
            or
               accept respond;
            end select;

            exit when Done;

            --  Filter and call response.
            --
            for i in 1 .. the_World.manifold_Count
            loop
               if         not the_World.Manifolds (i).Sprites (1).is_Destroyed
                 and then not the_World.Manifolds (i).Sprites (2).is_Destroyed
                 and then     Filters_through (the_World.Manifolds (i))
               then
                  the_Response (the_World.Manifolds (i),
                                the_World);
               end if;
            end loop;

            the_responses_Done.signal;

         exception
            when E : others =>
               put_Line ("Exception in impact_Responder.");
               put_Line (Exception_Information (E));
               the_responses_Done.signal;
         end;
      end loop;

   end impact_Responder;


   ----------
   --- Events
   --

   function to_raycast_collision_Event (Params : not null access no_Parameters) return raycast_collision_Event
   is
   begin
      return raycast_collision_Event' (others => <>);
   end to_raycast_collision_Event;



   overriding
   procedure destruct (Self : in out raycast_collision_Event)
   is
   begin
      free (Self.Context);
   end destruct;


   -----------
   --  Testing
   --

   overriding
   procedure kick_Sprite (Self : in out Item;   sprite_Id : in gel.sprite_Id)
   is
      the_Sprite : constant gel.Sprite.view := Item'Class (Self).all_Sprites.fetch.Element (sprite_Id);
   begin
      the_Sprite.Speed_is ((0.0, 10.0, 0.0));
   end kick_Sprite;


end gel.World;





-- Old engine code left for reference ...

   ----------
   --- Engine
   --

   --  task body Engine
   --  is
   --     use type gel.Joint.view,
   --              ada.Containers.Count_type;
   --
   --     Stopped          : Boolean                   := True;
   --     Cycle            : ada.Containers.Count_type := 0;
   --     next_render_Time : ada.Calendar.Time;
   --
   --     the_filtered_impact_Response_Set : filtered_impact_Response_Sets.Set;
   --
   --     max_joint_Force,
   --     max_joint_Torque : Real := 0.0;


      --  procedure free_Sprites
      --  is
      --     the_free_Sprites : gel.Sprite.views := the_World.free_sprite_Set;
      --  begin
      --     for i in the_free_Sprites'Range
      --     loop
      --        log ("Engine is freeing sprite id:" & the_free_Sprites (i).Id'Image);
      --
      --        if the_free_Sprites (i).owns_Graphics
      --        then
      --           the_World.Renderer.free (the_free_Sprites (i).Visual.Model);
      --        end if;
      --
      --        gel.Sprite.free (the_free_Sprites (i));
      --     end loop;
      --  end free_Sprites;



      --  procedure free_graphics_Models
      --  is
      --     use id_Maps_of_model;
      --     Cursor : id_Maps_of_model.Cursor := the_World.graphics_Models.First;
      --  begin
      --     while has_Element (Cursor)
      --     loop
      --        the_World.Renderer.free (Element (Cursor));
      --        next (Cursor);
      --     end loop;
      --  end free_graphics_Models;



      --  procedure evolve
      --  is
      --     the_sprite_Transforms : sprite_Maps_of_transforms.Map := the_World.all_sprite_Transforms.Fetch;
      --  begin
      --     Cycle := Cycle + 1;

         --  do_engine_Commands:
         --  declare
            --  the_Commands  : World.Commands;
            --  Count         : Natural;
            --  command_Count : array (command_Kind) of Natural := (others => 0);

         --  begin
            --  the_World.Commands.fetch (the_Commands, Count);

            --  for Each in 1 .. Count
            --  loop
            --     declare
            --        use Physics.Engine;
                  --  the_Command : World.Command renames the_Commands (Each);
               --  begin
                  --  command_Count (the_Command.Kind) := command_Count (the_Command.Kind) + 1;

                  --  case the_Command.Kind
                  --  is
--                       when scale_Sprite =>
--                          the_World.physics_Engine.add (std_Physics.Engine.Command' (Kind   => scale_Object,
--                                                                                     Sprite => the_Command.Sprite.Solid,
--                                                                                     Scale  => the_Command.Scale));
--                          the_Command.Sprite.Solid.activate;
--                          the_Command.Sprite.Shape.Scale_is (the_Command.Scale);
--                          the_Command.Sprite.Solid.Scale_is (the_Command.Scale);
--
--                          the_World.physics_Space.update_Bounds (std_physics.Object.view (the_Command.Sprite.Solid));


--                       when update_Bounds =>
--                          the_World.physics_Space.update_Bounds (std_physics.Object.view (the_Command.Sprite.Solid));


                     --  when update_Site =>
                     --     the_World.physics_Engine.update_Site (the_Command.Sprite.Solid,
                     --                                           the_Command.Site);
--                          std_physics.Object.view (the_Command.Sprite.Solid).Site_is (the_Command.Site);


--                       when set_Speed =>
--                          std_physics.Object.view (the_Command.Sprite.Solid).Speed_is (the_Command.Speed);


--                       when set_xy_Spin =>
--                          std_physics.Object.view (the_Command.Sprite.Solid).xy_Spin_is (the_Command.xy_Spin);


                     --  when add_Sprite =>
                     --     declare
                     --        procedure add (the_Sprite : in Sprite.view)
                     --        is
                     --        begin
                     --           if the_Sprite.Id = null_sprite_Id
                     --           then
                     --              raise Program_Error;
                     --           end if;
                     --
                     --           the_World.add (the_Sprite.graphics_Model);
                     --           the_World.add (the_Sprite. physics_Model);
                     --
                     --           the_sprite_Transforms.insert  (the_Sprite, Identity_4x4);
                     --
                     --           the_Sprite.Solid.user_Data_is (the_Sprite);
                     --           the_Sprite.Solid.Model_is (the_Sprite.physics_Model);
                     --
                     --           if the_Sprite.physics_Model.is_Tangible
                     --           then
                     --              the_World.physics_Engine.add (physics.Object.view (the_Sprite.Solid));
                     --           end if;
                     --
                     --           the_World.sprite_Count                     := the_World.sprite_Count + 1;
                     --           the_World.Sprites (the_World.sprite_Count) := the_Sprite;
                     --        end add;
                     --
                     --     begin
                     --        add (the_Command.Sprite);
                     --     end;


                     --  when rid_Sprite =>
                     --     declare
                     --        function find (the_Sprite : in Sprite.view) return Index
                     --        is
                     --        begin
                     --           for i in 1 .. the_World.sprite_Count
                     --           loop
                     --              if the_World.Sprites (i) = the_Sprite
                     --              then
                     --                 return i;
                     --              end if;
                     --           end loop;
                     --
                     --           raise constraint_Error with "No such sprite in world.";
                     --           return 0;
                     --        end find;
                     --
                     --
                     --        procedure rid (the_Sprite : in Sprite.view)
                     --        is
                     --           use type physics.Object.view;
                     --        begin
                     --           if the_Sprite.Solid /= null
                     --           then
                     --              if the_Sprite.physics_Model.is_Tangible
                     --              then
                     --                 the_World.physics_Engine.rid (the_Sprite.Solid);
                     --              end if;
                     --
                     --              if the_sprite_Transforms.contains (the_Sprite) then
                     --                 the_sprite_Transforms.delete   (the_Sprite);
                     --              end if;
                     --
                     --           else
                     --              raise program_Error;
                     --           end if;
                     --
                     --           declare
                     --              Id : Index;
                     --           begin
                     --              Id := find (the_Sprite);
                     --
                     --              if Id <= the_World.sprite_Count
                     --              then
                     --                 the_World.Sprites (1 .. the_World.sprite_Count - 1)
                     --                   :=   the_World.Sprites (     1 .. Id - 1)
                     --                      & the_World.Sprites (Id + 1 .. the_World.sprite_Count);
                     --              end if;
                     --
                     --              the_World.sprite_Count := the_World.sprite_Count - 1;
                     --           end;
                     --        end rid;
                     --
                     --     begin
                     --        rid (the_Command.Sprite);
                     --     end;


--                       when apply_Force =>
--                          the_Command.Sprite.Solid.apply_Force (the_Command.Force);


                     --  when destroy_Sprite =>
                     --     declare
                     --        the_free_Set : free_Set renames the_World.free_Sets (the_World.current_free_Set);
                     --     begin
                     --        the_free_Set.Count                        := the_free_Set.Count + 1;
                     --        the_free_Set.Sprites (the_free_Set.Count) := the_Command.Sprite;
                     --     end;


--                       when add_Joint =>
--                          the_World.physics_Space.add            (the_Command.Joint.Physics.all'Access);
--                          the_Command.Joint.Physics.user_Data_is (the_Command.Joint);


--                       when rid_Joint =>
--                          the_World.physics_Space.rid (the_Command.Joint.Physics.all'Access);


--                       when set_Joint_local_Anchor =>
--                          the_World.physics_Space.set_Joint_local_Anchor (the_Command.anchor_Joint.Physics.all'Access,
--                                                                          the_Command.is_Anchor_A,
--                                                                          the_Command.local_Anchor);

                     --  when free_Joint =>
                     --     gel.Joint.free (the_Command.Joint);


                     --  when cast_Ray =>
                     --     declare
                     --        function cast_Ray (Self : in Item'Class;   From, To : in Vector_3) return ray_Collision
                     --        is
                     --           use type physics.Object.view;
                     --
                     --           physics_Collision : constant physics.Space.ray_Collision := Self.physics_Space.cast_Ray (From, To);
                     --        begin
                     --           if physics_Collision.near_Object = null
                     --           then
                     --              return ray_Collision' (near_Sprite => null,
                     --                                     others      => <>);
                     --           else
                     --              return ray_Collision' (to_GEL (physics_Collision.near_Object),
                     --                                     physics_Collision.hit_Fraction,
                     --                                     physics_Collision.Normal_world,
                     --                                     physics_Collision.  Site_world);
                     --           end if;
                     --        end cast_Ray;
                     --
                     --        the_Collision : constant ray_Collision := cast_Ray (the_World.all,
                     --                                                            the_Command.From,
                     --                                                            the_Command.To);
                     --     begin
                     --        if        the_Collision.near_Sprite = null
                     --          or else the_Collision.near_Sprite.is_Destroyed
                     --        then
                     --           free (the_Command.Context);
                     --
                     --        else
                     --           declare
                     --              no_Params : aliased no_Parameters;
                     --              the_Event :         raycast_collision_Event'Class
                     --                := raycast_collision_Event_dispatching_Constructor (the_Command.event_Kind,
                     --                                                                    no_Params'Access);
                     --           begin
                     --              the_Event.Context     := the_Command.Context;
                     --              the_Event.near_Sprite := the_Collision.near_Sprite;
                     --              the_Event.Site_world  := the_Collision.Site_world;
                     --
                     --              the_Command.Observer.receive (the_Event, from_Subject => the_World.Name);
                     --           end;
                     --        end if;
                     --     end;


                     --  when new_impact_Response =>
                     --     declare
                     --        the_impact_Responder      : constant impact_Responder_view := new impact_Responder;
                     --        the_responses_done_Signal : constant Signal_Object_view    := new signal_Object;
                     --     begin
                     --        the_filtered_impact_Response_Set.insert ((the_Command.Filter,
                     --                                                  the_Command.Response,
                     --                                                  the_impact_Responder,
                     --                                                  the_responses_done_Signal));
                     --        the_impact_Responder.start (the_World,
                     --                                    the_Command.Filter,
                     --                                    the_Command.Response,
                     --                                    the_responses_done_Signal);
                     --     end;


--                       when set_Gravity =>
--                          the_World.physics_Space.Gravity_is (the_Command.Gravity);
                  --  end case;
               --  end;
            --  end loop;
         --  end do_engine_Commands;


         --  Evolve the physics.
         --
--           if not the_World.is_a_Mirror
--           then
--              the_World.physics_Space.evolve (by => 1.0 / 60.0);     -- Evolve the world.
--           end if;


         --  --  Contact Manifolds
         --  --
         --  declare
         --     Count : Natural := 0;
         --  begin
         --     for i in 1 .. the_World.physics_Space.manifold_Count
         --     loop
         --        declare
         --           function to_Integer is new ada.unchecked_Conversion (physics_Object_view, Integer);
         --
         --           the_physics_Manifold : constant physics.Space.a_Manifold
         --             := the_World.physics_Space.Manifold (i);
         --        begin
         --           Count                       := Count + 1;
         --           the_World.Manifolds (Count) := (sprites => (to_GEL (the_physics_Manifold.Objects (1)),
         --                                                       to_GEL (the_physics_Manifold.Objects (2))),
         --                                           contact => (Site => the_physics_Manifold.Contact.Site));
         --        exception
         --           when others =>
         --              put_Line ("Error in 'gel.world.Engine.evolve' contact manifolds.");
         --              Count := Count - 1;
         --        end;
         --     end loop;
         --
         --     the_World.manifold_Count := the_World.physics_Space.manifold_Count;
         --
         --  exception
         --     when E : others =>
         --        put_Line ("'gel.World.local.Engine.Contact Manifolds' has an unhandled exception ...");
         --        put_Line (exception_Information (E));
         --  end;


         --  --  For each registered impact response, tell the associated responder task to respond.
         --  --
         --  declare
         --     use filtered_impact_Response_Sets;
         --     Cursor : filtered_impact_Response_Sets.Cursor := the_filtered_impact_Response_Set.First;
         --
         --  begin
         --     while has_Element (Cursor)
         --     loop
         --        Element (Cursor).Responder.respond;
         --        next (Cursor);
         --     end loop;
         --
         --     --  Wait for all responders to complete.
         --     --
         --     Cursor := the_filtered_impact_Response_Set.First;
         --
         --     while has_Element (Cursor)
         --     loop
         --        select
         --           Element (Cursor).responses_Done.wait;
         --        or
         --           delay Duration'Last;
         --        end select;
         --
         --        next (Cursor);
         --     end loop;
         --
         --  exception
         --     when E : others =>
         --        put_Line ("'gel.World.local.Engine.impact response' has an unhandled exception ...");
         --        put_Line (exception_Information (E));
         --  end;


         --  --  Update sprite transforms.
         --  --
         --  declare
         --     use sprite_Maps_of_transforms;
         --
         --     Cursor     : sprite_Maps_of_transforms.Cursor := the_sprite_Transforms.First;
         --     the_Sprite : gel.Sprite.view;
         --  begin
         --     while has_Element (Cursor)
         --     loop
         --        the_Sprite := Key (Cursor);
         --        declare
         --           the_Transform : constant Matrix_4x4 := the_Sprite.Solid.get_Dynamics;
         --        begin
         --           the_sprite_Transforms.replace_Element (Cursor, the_Transform);
         --        end;
         --        next (Cursor);
         --     end loop;
         --  end;
         --
         --  the_World.all_sprite_Transforms.set (To => the_sprite_Transforms);
         --
         --  free_Sprites;
      --  end evolve;


      --  use type physics.Space.view;
      --
   --  begin
   --     accept start (space_Kind : in physics.space_Kind)
   --     do
   --        Stopped                 := False;
   --        the_World.physics_Space := physics.Forge.new_Space (space_Kind);
   --     end start;

      --  next_render_Time := ada.Calendar.Clock;

      --  loop
      --     select
      --        accept stop
      --        do
      --           Stopped := True;
      --
      --           -- Add 'destroy' commands for all sprites.
      --           --
      --           declare
      --              the_Sprites : Sprite.views renames the_World.Sprites;
      --           begin
      --              for i in 1 .. the_World.sprite_Count
      --              loop
      --                 the_Sprites (i).destroy (and_Children => False);
      --              end loop;
      --           end;

               -- Evolve the world til there are no commands left.
               --
               --  while not the_World.Commands.is_Empty
               --  loop
               --     evolve;
               --  end loop;

               --  Stop all impact responders tasks.
               --
               --  declare
               --     use filtered_impact_Response_Sets;
               --
               --     procedure free is new ada.unchecked_Deallocation (Signal_Object,
               --                                                       Signal_Object_view);
               --
               --     Cursor        : filtered_impact_Response_Sets.Cursor := the_filtered_impact_Response_Set.First;
               --
               --     the_Responder : impact_Responder_view;
               --     the_Signal    : Signal_Object_view;
               --
               --  begin
               --     while has_Element (Cursor)
               --     loop
               --        the_Signal    := Element (Cursor).responses_Done;
               --        the_Responder := Element (Cursor).Responder;
               --        the_Responder.stop;
               --
               --        while not the_Responder.all'Terminated
               --        loop
               --           delay 0.01;
               --        end loop;
               --
               --        free (the_Responder);
               --        free (the_Signal);
               --
               --        next (Cursor);
               --     end loop;
               --  end;

               -- Free both sets of freeable sprites.
               --
               --  free_Sprites;
               --  free_Sprites;
            --  end stop;

            --  exit when Stopped;
            --
         --  or
         --     accept reset_Age
         --     do
         --        the_World.Age_is (0.0);
         --     end reset_Age;
         --
         --  else
         --     null;
         --  end select;
         --
         --
         --  if not the_World.is_a_Mirror
         --  then
         --     evolve;
         --  end if;
         --
         --
         --  the_World.new_sprite_transforms_Available.signal;
         --  the_World.evolver_Done                   .signal;
         --
         --
         --  -- Check for joint breakage.
         --  --
         --  if the_World.broken_joints_Allowed
         --  then
         --     declare
         --        use gel.Joint,
         --            physics.Space;
         --
         --        the_Joint       : gel.Joint.view;
         --        reaction_Force,
         --        reaction_Torque : Real;
         --
         --        Cursor          : physics.Space.joint_Cursor'Class := the_World.physics_Space.first_Joint;
         --     begin
         --        while has_Element (Cursor)
         --        loop
         --           the_Joint := to_GEL (Element (Cursor));
         --
         --           if the_Joint /= null
         --           then
         --              reaction_Force  := abs (the_Joint.reaction_Force);
         --              reaction_Torque := abs (the_Joint.reaction_Torque);
         --
         --              if   reaction_Force  >  50.0 / 8.0
         --                or reaction_Torque > 100.0 / 8.0
         --              then
         --                 begin
         --                    the_World.physics_Space      .rid (the_Joint.Physics.all'Access);
         --                    the_World.broken_Joints.add (the_Joint);
         --
         --                 exception
         --                    when no_such_Child =>
         --                       put_Line ("Error when breaking joint due to reaction Force:  no_such_Child.");
         --                 end;
         --              end if;
         --
         --              if reaction_Force > max_joint_Force
         --              then
         --                 max_joint_Force := reaction_Force;
         --              end if;
         --
         --              if reaction_Torque > max_joint_Torque
         --              then
         --                 max_joint_Torque := reaction_Torque;
         --              end if;
         --           end if;
         --
         --           next (Cursor);
         --        end loop;
         --     end;
         --  end if;
         --
         --  next_render_Time := next_render_Time + Duration (1.0 / 60.0);
      --  end loop;
      --
   --  exception
   --     when E : others =>
   --        new_Line (2);
   --        put_Line ("Error in gel.World.Engine");
   --        new_Line;
   --        put_Line (exception_Information (E));
   --        put_Line ("Engine has terminated !");
   --        new_Line (2);
   --  end Engine;
