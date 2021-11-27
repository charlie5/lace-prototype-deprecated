with
     openGL.Camera,
     openGL.Model.billboard.textured,

     ada.unchecked_Deallocation;

package body openGL.Impostor
is
   ---------
   --- Forge
   --

   procedure destroy (Self : in out Item)
   is
      use openGL.Visual,
          Model,
          Texture;

      the_Model   : Model.view     := Self.Visual.Model;
      the_Texture : Texture.Object := Model.billboard.textured.view (the_Model).Texture;
   begin
      free (the_Texture);
      free (the_Model);
      free (Self.Visual);
   end destroy;


   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      if Self /= null then
         destroy (Self.all);
         deallocate (Self);
      end if;
   end free;



   --------------
   --- Attributes
   --

   procedure Visual_is (Self : in out Item;   Now : in openGL.Visual.view)
   is
   begin
      Self.Visual := Now;
   end Visual_is;


   function Visual (Self : access Item) return openGL.Visual.view
   is
   begin
      return Self.Visual;
   end Visual;



   function get_Target (Self : in Item) return openGL.Visual.view
   is
   begin
      return Self.Target;
   end get_Target;


   procedure set_Target (Self : in out Item;   Target : in openGL.Visual.view)
   is
      use type openGL.Visual.view;

      Width  : constant Real := Target.Model.Bounds.Ball * 2.00;
      Height : constant Real := Target.Model.Bounds.Ball * 2.00;

   begin
      if Self.Visual = null
      then
         Self.Visual := new openGL.Visual.item;
      end if;

      Self.Target     := Target;
      Self.is_Terrain := Target.is_Terrain;

      Self.Visual.Model_is (Model.billboard.textured.Forge.new_Billboard (Size    => (Width  => Width,
                                                                                      Height => Height),
                                                                          Plane   => Model.billboard.xy,
                                                                          Texture => null_Asset).all'Access);
      Self.Visual.Transform_is (Target.Transform);
      --  Self.Visual.model_Transform_is (Target.model_Transform);
   end set_Target;



   function target_camera_Distance (Self : in Item'Class) return Real
   is
   begin
      return Self.target_camera_Distance;
   end target_camera_Distance;


   function is_Valid (Self : in Item'Class) return Boolean
   is
   begin
      return Self.is_Valid;
   end is_Valid;


   function never_Updated (Self : in Item'Class) return Boolean
   is
   begin
      return Self.never_Updated;
   end never_Updated;


   function frame_Count_since_last_update (Self : in Item'Class) return Natural
   is
   begin
      return Natural (Self.freshen_Count);
   end frame_Count_since_last_update;


   function face_Count (Self : in Item) return Natural
   is
      pragma unreferenced (Self);
   begin
      return 1;
   end face_Count;


   procedure set_Alpha (Self : in out Item;   Alpha : in Real)
   is
   begin
      null;   -- TODO
   end set_Alpha;


   function Bounds (Self : in Item) return openGL.Bounds
   is
      pragma Unreferenced (Self);
   begin
      return (others => <>);     -- TODO
   end Bounds;
   pragma Unreferenced (Bounds);


   function is_Transparent (Self : in Item) return Boolean
   is
      pragma unreferenced (Self);
   begin
      return True;
   end is_Transparent;


   -- Update trigger configuration.
   --

   procedure set_freshen_count_update_trigger_Mod (Self : in out Item;   To : in Positive)
   is
   begin
      Self.freshen_count_update_trigger_Mod := Counter (To);
   end set_freshen_count_update_trigger_Mod;


   function get_freshen_count_update_trigger_Mod (Self : in Item) return Positive
   is
   begin
      return Positive (Self.freshen_count_update_trigger_Mod);
   end get_freshen_count_update_trigger_Mod;



   procedure set_size_update_trigger_Delta (Self : in out Item;   To : in Positive)
   is
   begin
      Self.size_update_trigger_Delta := gl.glSizeI (To);
   end set_size_update_trigger_Delta;


   function get_size_update_trigger_Delta (Self : in Item) return Positive
   is
   begin
      return Positive (Self.size_update_trigger_Delta);
   end get_size_update_trigger_Delta;



   function general_Update_required (Self : access Item;   the_Camera_Site  : in Vector_3;
                                                           the_pixel_Region : in pixel_Region) return Boolean
   is
      pragma unreferenced (the_pixel_Region);

      use linear_Algebra_3d;
      use type gl.GLsizei;

      Camera_has_moved : constant Boolean :=  the_Camera_Site                         /= Self.prior_camera_Position;
      Target_has_moved : constant Boolean :=  get_Translation (Self.Target.Transform) /= Self.prior_target_Position;
      --  Target_has_moved : constant Boolean :=  get_Translation (Self.Target.model_Transform) /= Self.prior_target_Position;

   begin
      Self.freshen_Count := Self.freshen_Count + 1;

      if Self.freshen_Count > Self.freshen_count_update_trigger_Mod
      then
         return True;
      end if;

      if         Camera_has_moved
        and then abs (Angle (the_Camera_Site,
                             Self.prior_target_Position,
                             Self.prior_camera_Position)) > to_Radians (15.0)
      then
         return True;
      end if;

      if         Target_has_moved
        and then abs (Angle (get_Translation (Self.Target.Transform),
        --  and then abs (Angle (get_Translation (Self.Target.model_Transform),
                             Self.prior_camera_Position,
                             Self.prior_target_Position)) > to_Radians (15.0)
      then
         return True;
      end if;


      if         Self.prior_pixel_Region.Width  >  40                -- Ignore target rotation triggered updates when target is small on screen.
        and then Self.prior_pixel_Region.Height >  40                --
        and then Self.prior_target_Rotation    /= get_Rotation (Self.Target.Transform)
        --  and then Self.prior_target_Rotation    /= get_Rotation (Self.Target.model_Transform)
      then
         return True;
      end if;

      return False;
   end general_Update_required;



   function size_Update_required (Self : access Item;   the_pixel_Region : in pixel_Region) return Boolean
   is
      use      GL;
      use type gl.GLsizei;
   begin
      return     abs (the_pixel_Region.Width  - Self.prior_Width_Pixels)  > Self.size_update_trigger_Delta
             or  abs (the_pixel_Region.Height - Self.prior_Height_pixels) > Self.size_update_trigger_Delta;
   end size_Update_required;



   function get_pixel_Region (Self : access Item'Class;   camera_Spin                 : in Matrix_3x3;
                                                          camera_Site                 : in Vector_3;
                                                          camera_projection_Transform : in Matrix_4x4;
                                                          camera_Viewport             : in linear_Algebra_3d.Rectangle) return pixel_Region
   is
      use linear_Algebra_3D;

      --  target_Centre           : constant Vector_3 := camera_Spin * (  get_Translation (Self.Target.model_Transform)
      target_Centre           : constant Vector_3 := camera_Spin * (  get_Translation (Self.Target.Transform)
                                                                    - camera_Site);

      target_lower_Left       : constant Vector_3 := target_Centre - (Self.Target.Model.Bounds.Ball,
                                                                      Self.Target.Model.Bounds.Ball,
                                                                      0.0);

      target_Centre_proj      : constant Vector_4 := target_Centre     * camera_projection_Transform;
      target_Lower_Left_proj  : constant Vector_4 := target_lower_Left * camera_projection_Transform;

      target_Centre_norm      : constant Vector_3 := (target_Centre_proj (1)     / target_Centre_proj (4),
                                                      target_Centre_proj (2)     / target_Centre_proj (4),
                                                      target_Centre_proj (3)     / target_Centre_proj (4));

      target_Lower_Left_norm  : constant Vector_3 := (target_Lower_Left_proj (1) / target_Lower_Left_proj (4),
                                                      target_Lower_Left_proj (2) / target_Lower_Left_proj (4),
                                                      target_Lower_Left_proj (3) / target_Lower_Left_proj (4));

      target_Centre_norm_0to1 : constant Vector_3 := (target_Centre_norm (1)     * 0.5 + 0.5,
                                                      target_Centre_norm (2)     * 0.5 + 0.5,
                                                      target_Centre_norm (3)     * 0.5 + 0.5);

      target_Lower_Left_norm_0to1 : constant Vector_3 := (target_Lower_Left_norm (1) * 0.5 + 0.5,
                                                          target_Lower_Left_norm (2) * 0.5 + 0.5,
                                                          target_Lower_Left_norm (3) * 0.5 + 0.5);

      viewport_Width  : constant Integer    := camera_Viewport.Max (1) - camera_Viewport.Min (1) + 1;
      viewport_Height : constant Integer    := camera_Viewport.Max (2) - camera_Viewport.Min (2) + 1;

      Width           : constant Real       := 2.0  *  Real (viewport_Width) * (  target_Centre_norm_0to1     (1)
                                                                                - target_Lower_Left_norm_0to1 (1));

      Width_pixels   : constant gl.glSizei := gl.glSizei (  Integer (Real (viewport_Width) * target_Lower_Left_norm_0to1 (1) + Width)
                                                          - Integer (Real (viewport_Width) * target_Lower_Left_norm_0to1 (1))
                                                          + 1);

      Height         : constant Real       := 2.0  *  Real (viewport_Height) * (  target_Centre_norm_0to1     (2)
                                                                                - target_Lower_Left_norm_0to1 (2));

      Height_pixels  : constant gl.glSizei := gl.glSizei (  Integer (Real (viewport_Height) * target_Lower_Left_norm_0to1 (2) + Height)
                                                          - Integer (Real (viewport_Height) * target_Lower_Left_norm_0to1 (2))
                                                          + 1);
      use type gl.GLsizei;

   begin
      Self.all.target_camera_Distance := abs (target_Centre);   -- NB: Cache distance from camera to target.

      return (X      => gl.glInt (target_Lower_Left_norm_0to1 (1) * Real (Viewport_Width))  - 0,
              Y      => gl.glInt (target_Lower_Left_norm_0to1 (2) * Real (viewport_Height)) - 0,
              Width  => Width_pixels + 0,
              Height => Height_pixels + 0);
   end get_pixel_Region;


   --------------
   --  Operations
   --

   procedure update (Self : in out Item;   the_Camera   : access Camera.item'Class;
                                           texture_Pool : in     texture.Pool_view)
   is
      pragma unreferenced (the_Camera, texture_Pool);
      use openGL.Visual;

--        Width_size     : constant openGL.texture.Size := to_Size (Natural (Self.current_Width_pixels));
--        Height_size    : constant openGL.texture.Size := to_Size (Natural (Self.current_Height_pixels));

--        texture_Width  : constant gl.glSizei          := power_of_2_Ceiling (Natural (Self.current_Width_pixels ));
--        texture_Height : constant gl.glSizei          := power_of_2_Ceiling (Natural (Self.current_Height_pixels));

      the_Model      : constant Model.billboard.textured.view := Model.billboard.textured.view (Self.Visual.Model);
--        GL_Error       :          Boolean;

   begin
      Self.Visual.all := Self.Target.all;
      Self.Visual.Model_is (the_Model.all'Access);
   end update;


end openGL.Impostor;
