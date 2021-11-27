with
     gel.Events,

     physics.remote.Model,
     physics.Forge,

     openGL.remote_Model,
     openGL.Renderer.lean,

     lace.Response,
     lace.Event.utility,

     ada.unchecked_Deallocation,
     ada.Text_IO;


package body gel.World.client
is
   use linear_Algebra_3D,
       lace.Event.utility;


   procedure log (Message : in String)
                  renames ada.text_IO.put_Line;
   pragma Unreferenced (log);


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
                         Renderer   : access openGL.Renderer.lean.item'Class) return gel.World.client.item
      is
         use lace.Subject_and_deferred_Observer.Forge;
      begin
         return Self : gel.World.client.item := (to_Subject_and_Observer (Name => Name & " world" & Id'Image)
                                          with others => <>)
         do
            Self.define (Name, Id, space_Kind, Renderer);
         end return;
      end to_World;



      function new_World (Name       : in     String;
                          Id         : in     world_Id;
                          space_Kind : in     physics.space_Kind;
                          Renderer   : access openGL.Renderer.lean.item'Class) return gel.World.client.view
      is
         use lace.Subject_and_deferred_Observer.Forge;

         Self : constant gel.World.client.view
           := new gel.World.client.item' (to_Subject_and_Observer (name => Name & " world" & Id'Image)
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


      the_Sprite.desired_Dynamics_are (Site => the_Sprite.Site,
                                       Spin => to_Quaternion (get_Rotation (the_Sprite.Transform)));

      --  the_Sprite.desired_Site_is (the_Sprite.Site);
      --  the_Sprite.desired_Spin_is (to_Quaternion (get_Rotation (the_Sprite.Transform)));

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
         Self.World.add (the_Sprite);
      end;
   end respond;



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
      Self.World.add (the_Sprite);
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



   type graphics_Model_iface_view is access all openGL.remote_Model.item'Class;

   type physics_Model_iface_view is access all Standard.physics.remote.Model.item'Class;



   procedure is_a_Mirror (Self : access Item'Class;   of_World : in remote.World.view)
   is
   begin
      the_new_model_Response.World := Self.all'Access;

      Self.add (the_new_model_Response'Access,
                to_Kind (remote.World.new_model_Event'Tag),
                of_World.Name);

      define   (the_my_new_sprite_Response, World  => Self.all'Access,
                                            Models => Self.graphics_Models'Access);

      Self.add (the_my_new_sprite_Response'Access,
                to_Kind (gel.Events.my_new_sprite_added_to_world_Event'Tag),
                of_World.Name);

      --  Obtain and make a local copy of models, sprites and humans from the mirrored world.
      --
      declare
         use remote.World.id_Maps_of_model_plan;

         the_server_Models         : constant remote.World.graphics_Model_Set := of_World.graphics_Models;    -- Fetch graphics models from the server.
         the_server_physics_Models : constant remote.World.physics_model_Set  := of_World.physics_Models;     -- Fetch physics  models from the server.
      begin
         --  Create our local graphics models.
         --
         declare
            Cursor    : remote.World.Id_Maps_of_Model_Plan.Cursor := the_server_Models.First;
            new_Model : graphics_Model_iFace_view;
         begin
            while has_Element (Cursor)
            loop
               new_Model := new openGL.remote_Model.item'Class' (Element (Cursor));
               Self.add (openGL.Model.view (new_Model));

               next (Cursor);
            end loop;
         end;

         --  Create our local physics models.
         --
         declare
            use remote.World.id_Maps_of_physics_model_plan;

            Cursor    : remote.World.id_Maps_of_physics_model_plan.Cursor := the_server_physics_Models.First;
            new_Model : physics_Model_iFace_view;

         begin
            while has_Element (Cursor)
            loop
               new_Model := new physics.remote.Model.item'Class' (Element (Cursor));
               Self.add (physics.Model.view (new_Model));

               next (Cursor);
            end loop;
         end;

         --  Fetch sprites from the server.
         --
         declare
            the_Sprite         :          gel.Sprite.view;
            the_server_Sprites : constant remote.World.sprite_model_Pairs := of_World.Sprites;
         begin
            for i in the_server_Sprites'Range
            loop
               the_Sprite := to_Sprite (the_server_Sprites (i),
                                        Self.graphics_Models,
                                        Self. physics_Models,
                                        gel.World.view (Self));
               Self.add (the_Sprite);
            end loop;
         end;
      end;
   end is_a_Mirror;



   --------------
   --- Operations
   --

   overriding
   procedure add (Self : access Item;   the_Sprite   : in gel.Sprite.view;
                                        and_Children : in Boolean := False)
   is
   begin
      Self.all_Sprites.Map.add (the_Sprite);
   end add;



   overriding
   procedure motion_Updates_are (Self : in Item;   Now : in remote.World.motion_Updates)
   is
      all_Sprites : constant id_Maps_of_sprite.Map := Self.all_Sprites.Map.fetch_all;

   begin
      for i in Now'Range
      loop
         declare
            use remote.World;

            the_Id             : constant gel.sprite_Id := Now (i).Id;
            the_Sprite         : constant Sprite.view   := all_Sprites.Element (the_Id);

            new_Site           : constant Vector_3      := refined (Now (i).Site);
            --  site_Delta         :          Vector_3;
            --  min_teleport_Delta : constant               := 20.0;

            new_Spin           : constant Quaternion    := refined (Now (i).Spin);
            --  new_Spin           : constant Matrix_3x3        := Now (i).Spin;

         begin
            --  site_Delta := new_Site - the_Sprite.desired_Site;
            --
            --  if        abs site_Delta (1) > min_teleport_Delta
            --    or else abs site_Delta (2) > min_teleport_Delta
            --    or else abs site_Delta (3) > min_teleport_Delta
            --  then
            --     log ("Teleport.");
            --     the_Sprite.Site_is (new_Site);   -- Sprite has been 'teleported', so move it now
            --  end if;                             -- to prevent later interpolation.

            null;

            --  the_Sprite.Site_is (new_Site);
            --  the_Sprite.Spin_is (to_Rotation (Axis  => new_Spin.V,
            --                                   Angle => new_Spin.R));

            --  the_Sprite.Spin_is (to_Matrix (to_Quaternion (new_Spin)));

            --  the_Sprite.desired_Dynamics_are (Site => new_Site,
            --                                   Spin => to_Quaternion (new_Spin));

            the_Sprite.desired_Dynamics_are (Site => new_Site,
                                             Spin => new_Spin);

            --  the_Sprite.desired_Site_is (new_Site);
            --  the_Sprite.desired_Spin_is (new_Spin);
         end;
      end loop;
   end motion_Updates_are;



   overriding
   procedure evolve (Self : in out Item)
   is
   begin
      Self.Age := Self.Age + evolve_Period;

      Self.respond;
      Self.local_Subject_and_deferred_Observer.respond;

      --  Interpolate sprite transforms.
      --
      declare
         use id_Maps_of_sprite;

         --  all_Sprites   : constant id_Maps_of_sprite.Map    := Self.id_Map_of_sprite;
         all_Sprites   : constant id_Maps_of_sprite.Map    := Self.all_Sprites.Map.fetch_all;
         Cursor        :          id_Maps_of_sprite.Cursor := all_Sprites.First;
         the_Sprite    :          gel.Sprite.view;

      begin
         while has_Element (Cursor)
         loop
            the_Sprite := Sprite.view (Element (Cursor));
            the_Sprite.interpolate_Motion;

            next (Cursor);
         end loop;
      end;
   end evolve;



   overriding
   function fetch (From : in sprite_Map) return id_Maps_of_sprite.Map
   is
   begin
      return From.Map.fetch_all;
   end fetch;



   overriding
   procedure add (To : in out sprite_Map;   the_Sprite : in Sprite.view)
   is
   begin
      To.Map.add (the_Sprite);
   end add;



   overriding
   procedure rid (To : in out sprite_Map;   the_Sprite : in Sprite.view)
   is
   begin
      To.Map.rid (the_Sprite);
   end rid;



   overriding
   function all_Sprites (Self : access Item) return access World.sprite_Map'Class
   is
   begin
      return Self.all_Sprites'Access;
   end all_Sprites;



   --------------
   --  Containers
   --

   protected
   body safe_id_Map_of_sprite
   is
      procedure add (the_Sprite : in Sprite.view)
      is
      begin
         Map.insert (the_Sprite.Id,
                     the_Sprite);
      end add;


      procedure rid (the_Sprite : in Sprite.view)
      is
      begin
         Map.delete (the_Sprite.Id);
      end rid;


      function fetch (Id : in sprite_Id) return Sprite.view
      is
      begin
         return Map.Element (Id);
      end fetch;


      function fetch_all return id_Maps_of_sprite.Map
      is
      begin
         return Map;
      end fetch_all;

   end safe_id_Map_of_sprite;


end gel.World.client;
