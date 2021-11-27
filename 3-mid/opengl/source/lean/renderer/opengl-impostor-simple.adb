with
     openGL.Camera,
     openGL.Texture,

     ada.unchecked_Deallocation;

package body openGL.Impostor.simple
is

   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      if Self /= null
      then
         destroy    (Self.all);
         deallocate (Self);
      end if;
   end free;



   overriding
   function current_Camera_look_at_Rotation (Self : in Item) return Matrix_3x3
   is
   begin
      return Self.current_Camera_look_at_Rotation;
   end current_Camera_look_at_Rotation;



   overriding
   function update_Required (Self : access Item;   the_Camera : access Camera.item'Class) return Boolean
   is
      use linear_Algebra_3d;
   begin
      -- Look directly at target so it will be rendered in the centre of the viewport.
      --
      Self.current_Camera_look_at_Rotation := get_Rotation (look_at (the_Camera.Site,
                                                                     get_Translation (Self.Target.Transform),
                                                                     --  get_Translation (Self.Target.model_Transform),
                                                                     (0.0, 1.0, 0.0)));
      Self.current_pixel_Region := Self.get_pixel_Region (camera_Spin                 => Self.current_Camera_look_at_Rotation,
                                                          camera_Site                 => the_Camera.Site,
                                                          camera_projection_Transform => the_Camera.projection_Transform,
                                                          camera_Viewport             => the_Camera.Viewport);
      declare
         update_Required : Boolean := Self.general_Update_required (the_Camera.Site,
                                                                    Self.current_pixel_Region);
      begin
         if         not update_Required
           and then Self.size_Update_required (Self.current_pixel_Region)
         then
            update_Required := True;
         end if;

         if Update_required
         then
            Self.current_Width_pixels  := Self.current_pixel_Region.Width;       -- Cache current state.
            Self.current_Height_pixels := Self.current_pixel_Region.Height;

            Self.current_copy_X        := Self.current_pixel_Region.X;
            Self.current_copy_Y        := Self.current_pixel_Region.Y;

            Self.current_copy_Width    := Self.current_pixel_Region.Width;
            Self.current_copy_Height   := Self.current_pixel_Region.Height;
         end if;

         return update_Required;
      end;
   end update_Required;



   overriding
   procedure pre_update (Self : in out Item;   the_Camera : access Camera.item'Class)
   is
   begin
      Self.camera_world_Rotation_original := the_Camera.Spin;

      the_Camera.Spin_is (Self.current_Camera_look_at_Rotation);
   end pre_update;



   overriding
   procedure update (Self : in out Item;   the_Camera   : access Camera.item'Class;
                                           texture_Pool : in     Texture.Pool_view)
   is
      world_Rotation_original : constant Matrix_3x3 := the_Camera.Spin;
   begin
      the_Camera.Spin_is (Self.current_Camera_look_at_Rotation);

      Impostor.item (Self).update (the_Camera, texture_Pool);   -- Base class 'update'.

      the_Camera.Spin_is (world_Rotation_original);
   end update;



   overriding
   procedure post_update (Self : in out Item;   the_Camera : access Camera.item'Class)
   is
   begin
      the_Camera.Spin_is (Self.camera_world_Rotation_original);
   end post_update;


end openGL.Impostor.simple;
