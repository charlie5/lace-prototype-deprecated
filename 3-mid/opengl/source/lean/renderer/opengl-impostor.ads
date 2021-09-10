with
     openGL.Texture,
     openGL.Visual;

limited
with
     openGL.Camera;

package openGL.Impostor
--
-- Contains a 2D image of another openGL visual.
--
is
   type Counter is mod 2**32;

   type pixel_Region is
      record
         X, Y          : gl.glInt;
         Width, Height : gl.glSizeI;
      end record;


   type Item is abstract tagged     -- TODO: Make private.
      record
         Target                           : openGL.Visual.view;
         Visual                           : openGL.Visual.view;

         freshen_Count                    : Counter    := 0;
         freshen_count_update_trigger_Mod : Counter    := 150;

         size_update_trigger_Delta        : gl.glSizeI := 2;
         expand_X, expand_Y               : Real       := 0.03;

         never_Updated                    : Boolean    := True;
         is_Valid                         : Boolean    := True;     -- True when rendered target has both width and height > 0.
                                                                    -- (NB: Always true for simple impostors.)
         -- Current state.
         --
         current_pixel_Region   : pixel_Region;

         current_Width_pixels,
         current_Height_pixels  : gl.glSizei;
         current_copy_X_Offset,
         current_copy_Y_Offset  : gl.glInt := 0;
         current_copy_X,
         current_copy_Y         : gl.glInt;
         current_copy_Width,
         current_copy_Height    : gl.glSizeI;

         target_camera_Distance                  : Real;
         target_camera_Distance_less_frame_Count : Real;

         -- Prior state.
         --
         prior_pixel_Region    : pixel_Region := (X => 0, Y => 0, Width => gl.glSizeI'First, Height => gl.glSizeI'First);
         prior_Width_pixels    : gl.glSizei   := 0;
         prior_Height_pixels   : gl.glSizei   := 0;

         prior_target_Rotation : Matrix_3x3   := Identity_3x3;
         prior_target_Position : Vector_3     := (0.0,  0.0,  0.0);

         prior_camera_Position : Vector_3     := (1.0,  1.0,  1.0);

         is_Terrain            : Boolean      := False;
      end record;

   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;


   ---------
   --- Forge
   --

   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);


   --------------
   --- Attributes
   --

   procedure set_Target (Self : in out Item;   Target : in Visual.view);
   function  get_Target (Self : in     Item)        return Visual.view;

   procedure Visual_is  (Self : in out Item;   Now    : in Visual.view);
   function  Visual     (Self : access Item)        return Visual.view;


   function current_Camera_look_at_Rotation (Self : in     Item) return Matrix_3x3
                                             is abstract;

   function update_Required                 (Self : access Item;    the_Camera : access Camera.item'Class) return Boolean
                                             is abstract;
   --
   -- NB: Caches current pixel_Region as a side-effect.


   function is_Valid                        (Self : in     Item'Class) return Boolean;
   --
   -- True when rendered target has width and height greater than 0.


   function never_Updated                   (Self : in     Item'Class) return Boolean;
   --
   -- True when 'update' has never been called for the impostor.


   function frame_Count_since_last_update   (Self : in     Item'Class) return Natural;


   function target_camera_Distance          (Self : in     Item'Class) return Real;
   --
   -- Returns the distance from the camera to the target, when 'update_required' was last called.


   -- Update trigger configuration.
   --

   procedure set_freshen_count_update_trigger_Mod (Self : in out Item;   To : in Positive);
   function  get_freshen_count_update_trigger_Mod (Self : in     Item)    return Positive;
   --
   -- Periodic freshen trigger.

   procedure set_size_update_trigger_Delta        (Self : in out Item;   To : in Positive);
   function  get_size_update_trigger_Delta        (Self : in     Item)    return Positive;
   --
   -- Update due to change in size of targets pixel rectangle.


   -- Base class subprograms
   --

   function  is_Transparent (Self : in     Item) return Boolean;
   procedure set_Alpha      (Self : in out Item;   Alpha        : in Real);

   function  face_Count     (Self : in     Item) return Natural;


   procedure pre_update     (Self : in out Item;   the_Camera   : access Camera.item'Class)
   is abstract;

   procedure update         (Self : in out Item;   the_Camera   : access Camera.item'Class;
                                                   texture_Pool : in     texture.Pool_view);
   --
   -- Renders the impostor to a cleared framebuffer and copies the image to the impostors texture.

   procedure post_update    (Self : in out Item;   the_Camera   : access Camera.item'Class)
   is abstract;



private

   function get_pixel_Region (Self : access Item'Class;   camera_Spin                 : in Matrix_3x3;
                                                          camera_Site                 : in Vector_3;
                                                          camera_projection_Transform : in Matrix_4x4;
                                                          camera_Viewport             : in linear_Algebra_3d.Rectangle) return pixel_Region;
   --
   -- Calculate and return the smallest rectangular screen region which encloses the target, when rendered by the_Camera.


   function general_Update_required (Self : access Item;   the_Camera_Site  : in Vector_3;
                                                           the_pixel_Region : in pixel_Region) return Boolean;
   function size_Update_required    (Self : access Item;   the_pixel_Region : in pixel_Region) return Boolean;


end openGL.Impostor;
