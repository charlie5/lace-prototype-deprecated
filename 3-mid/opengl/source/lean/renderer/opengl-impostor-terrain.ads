package openGL.Impostor.terrain
--
-- Handles impostoring of terrain 'visuals', which has greater image precision needs, to help avoid border cracks.
--
is
   type Item is new Impostor.item with private;
   type View is access all Item'Class;


   overriding
   procedure set_Target      (Self : in out Item;   Target       : in     openGL.Visual.view);

   overriding
   function current_Camera_look_at_Rotation
                             (Self : in Item) return Matrix_3x3;

   overriding
   function  update_Required (Self : access Item;   the_Camera   : access Camera.item'Class) return Boolean;

   overriding
   procedure update          (Self : in out Item;   the_Camera   : access Camera.item'Class;
                                                    texture_Pool : in     Texture.Pool_view);
   overriding
   procedure pre_update      (Self : in out Item;   the_Camera   : access Camera.item'Class);

   overriding
   procedure post_update     (Self : in out Item;   the_Camera   : access Camera.item'Class);

   procedure free            (Self : in out View);



private

   type Item is new Impostor.item with
      record
         current_Complete  : Boolean;

         prior_copy_Width  : gl.glSizeI := 0;
         prior_copy_Height : gl.glSizeI := 0;
         prior_Complete    : Boolean    := False;

         current_Camera_look_at_Rotation : Matrix_3x3 := Identity_3x3;
      end record;

end openGL.Impostor.terrain;
