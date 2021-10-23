with
     gel.Events,

     physics.remote.Model,
     physics.Object,
     physics.Forge,

     openGL.remote_Model,
     openGL.Renderer.lean,

     lace.Response,
     lace.Event.utility,

     ada.Text_IO,
     ada.Exceptions,
     ada.unchecked_Deallocation;


package body gel.World.server
is
   use gel.Sprite,
       linear_Algebra_3D,

       lace.Event.utility,
       lace.Event,

       ada.Text_IO;


   procedure log (Message : in String)
                  renames ada.text_IO.put_Line;


   ---------
   --- Forge
   --

   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      deallocate (Self);
   end free;



   procedure define (Self : in out Item'Class;   Name       : in     String;
                                                 Id         : in     world_Id;
                                                 space_Kind : in     physics.space_Kind;
                                                 Renderer   : access openGL.Renderer.lean.item'Class);

   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      physics.Space.free (Self.physics_Space);

      lace.Subject_and_deferred_Observer.item (Self).destroy;     -- Destroy base class.
      lace.Subject_and_deferred_Observer.free (Self.local_Subject_and_deferred_Observer);
   end destroy;



   package body Forge
   is

      function to_World (Name       : in     String;
                         Id         : in     world_Id;
                         space_Kind : in     physics.space_Kind;
                         Renderer   : access openGL.Renderer.lean.item'Class) return gel.World.server.item
      is
         use lace.Subject_and_deferred_Observer.Forge;
      begin
         return Self : gel.World.server.item := (to_Subject_and_Observer (Name => Name & " world" & Id'Image)
                                                 with others => <>)
         do
            Self.define (Name, Id, space_Kind, Renderer);
         end return;
      end to_World;



      function new_World (Name       : in     String;
                          Id         : in     world_Id;
                          space_Kind : in     physics.space_Kind;
                          Renderer   : access openGL.Renderer.lean.item'Class) return gel.World.server.view
      is
         use lace.Subject_and_deferred_Observer.Forge;

         Self : constant gel.World.server.view
           := new gel.World.server.item' (to_Subject_and_Observer (name => Name & " world" & Id'Image)
                                   with others => <>);
      begin
         Self.define (Name, Id, space_Kind, Renderer);
         return Self;
      end new_World;

   end Forge;



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

      the_Sprite.desired_Site_is (the_Sprite.Site);
      the_Sprite.desired_Spin_is (to_Quaternion (get_Rotation (the_Sprite.Transform)));

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

      Self.Id           := Id;
      Self.space_Kind   := space_Kind;
      Self.Renderer     := Renderer;
      Self.sprite_Count := 0;

      Self.physics_Space := physics.Forge.new_Space (space_Kind);
   end define;


   ----------------------
   --- new_model_Response
   --

   type new_model_Response is new lace.Response.item with
      record
         World : gel.World.view;
      end record;


   overriding
   function Name (Self : in new_model_Response) return String;


   overriding
   procedure respond (Self : in out new_model_Response;   to_Event : in lace.Event.Item'Class)
   is
      the_Event : constant remote.World.new_model_Event := remote.World.new_model_Event (to_Event);
   begin
      Self.World.add (new openGL.Model.item'Class' (openGL.Model.item'Class (the_Event.Model.all)));
   end respond;


   overriding
   function Name (Self : in new_model_Response) return String
   is
      pragma unreferenced (Self);
   begin
      return "new_model_Response";
   end Name;


   the_new_model_Response : aliased new_model_Response;


   --------------------------
   --- my_new_sprite_Response
   --
   type my_new_sprite_Response is new lace.Response.item with
      record
         World          :        gel.World.view;
         Models         : access id_Maps_of_model        .Map;
         physics_Models : access id_Maps_of_physics_model.Map;
      end record;


   overriding
   function Name (Self : in my_new_sprite_Response) return String;


   overriding
   procedure respond (Self : in out my_new_sprite_Response;   to_Event : in lace.Event.Item'Class)
   is
      the_Event  : constant gel.Events.my_new_sprite_added_to_world_Event
        := gel.events.my_new_sprite_added_to_world_Event (to_Event);

      the_Sprite : constant gel.Sprite.view
        := to_Sprite (the_Event.Pair,
                      Self.Models.all,
                      Self.physics_Models.all,
                      Self.World);
   begin
      Self.World.add (the_Sprite, and_Children => False);
   end respond;



   procedure define (Self : in out my_new_sprite_Response;   World  : in     gel.World.view;
                                                             Models : access id_Maps_of_model.Map)
   is
   begin
      Self.World  := World;
      Self.Models := Models;
   end define;



   overriding
   function Name (Self : in my_new_sprite_Response) return String
   is
      pragma unreferenced (Self);
   begin
      return "my_new_sprite_Response";
   end Name;

   the_my_new_sprite_Response : aliased my_new_sprite_Response;



   --------------
   --- Operations
   --

   overriding
   procedure evolve (Self : in out Item;   By : in Duration)
   is
      use sprite_Maps_of_transforms;
   begin
      gel.World.item (Self).evolve (by);     -- Evolve the base class.

      --  Update dynamics in client worlds.
      --
      declare
         use id_Maps_of_sprite,
             remote.World;

         all_Sprites            : constant id_Maps_of_sprite.Map    := Self.id_Map_of_sprite;
         Cursor                 :          id_Maps_of_sprite.Cursor := all_Sprites.First;


         the_Sprite             :          gel.Sprite.view;

         is_a_mirrored_World    : constant Boolean := not Self.Clients.Is_Empty;
         mirror_Updates_are_due : constant Boolean := Self.Age >= Self.Age_at_last_Clients_update + 0.25;
         updates_Count          :          Natural := 0;

         the_motion_Updates     :          remote.World.motion_Updates (1 .. Integer (all_Sprites.Length));

      begin
         if    is_a_mirrored_World
           and mirror_Updates_are_due
         then
            while has_Element (Cursor)
            loop
               the_Sprite := Sprite.view (Element (Cursor));

               updates_Count                      := updates_Count + 1;
               the_motion_Updates (updates_Count) := (Id   => the_Sprite.Id,
                                                      Site => coarsen (the_Sprite.Site),
                                                      Spin => coarsen (to_Quaternion (the_Sprite.Spin)));
               next (Cursor);
            end loop;

            --  Send updated sprite motions to all registered client worlds.
            --
            Self.Age_at_last_clients_update := Self.Age;

            if updates_Count > 0
            then
               declare
                  use World.server.world_Vectors;

                  Cursor     : world_Vectors.Cursor := Self.Clients.First;
                  the_Mirror : remote.World.view;
               begin
                  while has_Element (Cursor)
                  loop
                     the_Mirror := Element (Cursor);
                     the_Mirror.motion_Updates_are (the_motion_Updates (1 .. updates_Count));

                     next (Cursor);
                  end loop;
               end;
            end if;

         end if;
      end;

   end evolve;


   -----------------------
   --- Client Registration
   --

   overriding
   procedure register (Self : access Item;   the_Mirror         : in remote.World.view;
                                             Mirror_as_observer : in lace.Observer.view)
   is
   begin
      Self.Clients.append (the_Mirror);

      Self.register (Mirror_as_observer,  to_Kind (remote.World.                 new_model_Event'Tag));
      Self.register (Mirror_as_observer,  to_Kind (gel.events.                  new_sprite_Event'Tag));
      Self.register (Mirror_as_observer,  to_Kind (gel.events.my_new_sprite_added_to_world_Event'Tag));
   end register;



   overriding
   procedure deregister (Self : access Item;   the_Mirror : in remote.World.view)
   is
   begin
      Self.Clients.delete (Self.Clients.find_Index (the_Mirror));
   end deregister;


end gel.World.server;
