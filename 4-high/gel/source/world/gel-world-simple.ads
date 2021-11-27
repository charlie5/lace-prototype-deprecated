with
     gel.World,
     ada.unchecked_Conversion;

limited
with
     openGL.Renderer.lean;


package gel.World.simple
--
--  Provides a simple gel world.
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
                          Renderer   : access openGL.Renderer.lean.item'Class) return gel.World.simple.item;

      function new_World (Name       : in     String;
                          Id         : in     world_Id;
                          space_Kind : in     physics.space_Kind;
                          Renderer   : access openGL.Renderer.lean.item'Class) return gel.World.simple.view;
   end Forge;



private

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
         all_Sprites : aliased sprite_Map;
      end record;


   overriding
   function all_Sprites (Self : access Item) return access World.sprite_Map'Class;


end gel.World.simple;
