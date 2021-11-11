with
     gel.World,
     lace.Observer,
     ada.unchecked_Conversion,
     ada.Containers.Vectors;

limited
with
     openGL.Renderer.lean;


package gel.World.server
--
--  Provides a gel world server.
--
is
   type Item  is limited new gel.World.item
   with private;

   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;


   ---------
   --  Forge
   --

   package Forge
   is
      function to_World  (Name       : in     String;
                          Id         : in     world_Id;
                          space_Kind : in     physics.space_Kind;
                          Renderer   : access openGL.Renderer.lean.item'Class) return gel.World.server.item;

      function new_World (Name       : in     String;
                          Id         : in     world_Id;
                          space_Kind : in     physics.space_Kind;
                          Renderer   : access openGL.Renderer.lean.item'Class) return gel.World.server.view;
   end Forge;

   overriding
   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);


   --------------
   --- Operations
   --

   overriding
   procedure   register (Self : access Item;   the_Mirror         : in remote.World.view;
                                               Mirror_as_observer : in lace.Observer.view);
   overriding
   procedure deregister (Self : access Item;   the_Mirror         : in remote.World.view);

   overriding
   procedure evolve     (Self : in out Item);



private

   -----------
   --- Clients
   --
   use type remote.World.view;
   package world_Vectors is new ada.Containers.Vectors (Positive, remote.World.view);
   subtype world_Vector  is     world_Vectors.Vector;


   --------------
   --- sprite_Map
   --
   type sprite_Map is limited new World.sprite_Map with
      record
         Map : id_Maps_of_sprite.Map;
      end record;

   overriding
   function  fetch (From : in     sprite_Map) return id_Maps_of_sprite.Map;

   overriding
   procedure add   (To   : in out sprite_Map;   the_Sprite : in Sprite.view);

   overriding
   procedure rid   (From : in out sprite_Map;   the_Sprite : in Sprite.view);


   --------------
   --- World Item
   --
   type Item is limited new gel.World.item with
      record
         Age_at_last_Clients_update : Duration := 0.0;
         Clients                    : World_vector;

         all_Sprites : aliased sprite_Map;
      end record;


   overriding
   function all_Sprites (Self : access Item) return access World.sprite_Map'Class;


end gel.World.server;
