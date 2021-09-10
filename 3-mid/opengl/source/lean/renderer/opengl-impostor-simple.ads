package openGL.Impostor.simple
--
-- Can impostor any 'visual'.
--
is
   type Item is new Impostor.item with private;
   type View is access all Item'Class;


   overriding
   function current_Camera_look_at_Rotation (Self : in Item) return Matrix_3x3;

   overriding
   function  update_Required (Self : access Item;   the_Camera   : access Camera.item'Class) return Boolean;

   overriding
   procedure pre_update      (Self : in out Item;   the_Camera   : access Camera.item'Class);

   overriding
   procedure update          (Self : in out Item;   the_Camera   : access Camera.item'Class;
                                                    texture_Pool : in     Texture.Pool_view);
   overriding
   procedure post_update     (Self : in out Item;   the_Camera   : access Camera.item'Class);

   procedure free            (Self : in out View);



private

   type Item is new Impostor.item with
      record
         current_Camera_look_at_Rotation : Matrix_3x3;
         camera_world_Rotation_original  : Matrix_3x3;
      end record;

end openGL.Impostor.simple;
