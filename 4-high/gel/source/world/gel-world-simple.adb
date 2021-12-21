with
     physics.Forge,
     openGL.Renderer.lean;


package body gel.World.simple
is
   --  procedure log (Message : in String)
   --                 renames ada.text_IO.put_Line;


   ---------
   --- Forge
   --

   --  procedure free (Self : in out View)
   --  is
   --     procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   --  begin
   --     deallocate (Self);
   --  end free;



   procedure define (Self : in out Item'Class;   Name       : in     String;
                                                 Id         : in     world_Id;
                                                 space_Kind : in     physics.space_Kind;
                                                 Renderer   : access openGL.Renderer.lean.item'Class);

   package body Forge
   is

      function to_World (Name       : in     String;
                         Id         : in     world_Id;
                         space_Kind : in     physics.space_Kind;
                         Renderer   : access openGL.Renderer.lean.item'Class) return gel.World.simple.item
      is
         use lace.Subject_and_deferred_Observer.Forge;
      begin
         return Self : gel.World.simple.item := (to_Subject_and_Observer (Name => Name & " world" & Id'Image)
                                                 with others => <>)
         do
            Self.define (Name, Id, space_Kind, Renderer);
         end return;
      end to_World;



      function new_World (Name       : in     String;
                          Id         : in     world_Id;
                          space_Kind : in     physics.space_Kind;
                          Renderer   : access openGL.Renderer.lean.item'Class) return gel.World.simple.view
      is
         use lace.Subject_and_deferred_Observer.Forge;

         Self : constant gel.World.simple.view
           := new gel.World.simple.item' (to_Subject_and_Observer (name => Name & " world" & Id'Image)
                                              with others => <>);
      begin
         Self.define (Name, Id, space_Kind, Renderer);
         return Self;
      end new_World;

   end Forge;



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
      --  Self.sprite_Count := 0;

      Self.physics_Space := physics.Forge.new_Space (space_Kind);
   end define;



   --------------
   --- sprite_Map
   --

   overriding
   function fetch (From : in sprite_Map) return id_Maps_of_sprite.Map
   is
   begin
      return From.Map;
   end fetch;



   overriding
   procedure add (To : in out sprite_Map;   the_Sprite : in Sprite.view)
   is
   begin
      To.Map.insert (the_Sprite.Id, the_Sprite);
   end add;



   overriding
   procedure rid (From : in out sprite_Map;   the_Sprite : in Sprite.view)
   is
   begin
      From.Map.delete (the_Sprite.Id);
   end rid;



   overriding
   function all_Sprites (Self : access Item) return access World.sprite_Map'Class
   is
   begin
      return Self.all_Sprites'unchecked_Access;
   end all_Sprites;


end gel.World.simple;
