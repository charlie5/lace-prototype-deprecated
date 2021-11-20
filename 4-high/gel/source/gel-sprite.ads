with
     gel.Joint,

     openGL.Model,
     openGL.Visual,
     openGL.Program,

     physics.Model,
     physics.Object,
     physics.Shape,
     physics.Space,

     lace.Subject_and_deferred_Observer,
     lace.Response,

     ada.Containers.Vectors;

limited
with
     gel.World;


package gel.Sprite
--
--  Combines a graphics 'visual' and a physics 'solid'.
--
is
   type Item  is limited new lace.Subject_and_deferred_Observer.item with private;
   type View  is access all Item'Class;

   type Items is array (math.Index range <>) of aliased Item;
   type Views is array (math.Index range <>) of View;

   null_Sprites : constant Sprite.views;


   type physics_Space_view is access all physics.Space.item'Class;
   type World_view         is access all gel.World.item'Class;


   use Math;


   --------------
   --- Containers
   --

   type Grid       is array (math.Index range <>,
                             math.Index range <>) of Sprite.view;
   type Grid_view  is access all Grid;

   package Vectors is new ada.Containers.Vectors (Positive, Sprite.view);


   ----------
   --- Forge
   --

   procedure define  (Self : access Item;   World          : in     World_view;
                                            at_Site        : in     Vector_3;
                                            graphics_Model : access openGL. Model.item'Class;
                                            physics_Model  : access physics.Model.item'Class;
                                            owns_Graphics  : in     Boolean;
                                            owns_Physics   : in     Boolean;
                                            is_Kinematic   : in     Boolean := False);

   procedure destroy      (Self : access Item;   and_Children : in Boolean);
   function  is_Destroyed (Self : in     Item) return Boolean;
   procedure free         (Self : in out View);


   package Forge
   is
      function  to_Sprite (Name           : in     String;
                           World          : in     World_view;
                           at_Site        : in     Vector_3;
                           graphics_Model : access openGL. Model.item'Class;
                           physics_Model  : access physics.Model.item'Class;
                           owns_Graphics  : in     Boolean;
                           owns_Physics   : in     Boolean;
                           is_Kinematic   : in     Boolean := False) return Item;

      function new_Sprite (Name           : in     String;
                           World          : in     World_view;
                           at_Site        : in     Vector_3;
                           graphics_Model : access openGL. Model.item'Class;
                           physics_Model  : access physics.Model.item'Class;
                           owns_Graphics  : in     Boolean := True;
                           owns_Physics   : in     Boolean := True;
                           is_Kinematic   : in     Boolean := False) return View;
   end Forge;


   ---------------
   --- Attributes
   --

   function  World                 (Self : in     Item)     return  access gel.World.item'Class;

   function  Id                    (Self : in     Item)     return gel.sprite_Id;
   procedure Id_is                 (Self : in out Item;   Now : in gel.sprite_Id);

   function  Visual                (Self : access Item)     return openGL.Visual.view;

   function  graphics_Model        (Self : in     Item)     return openGL.Model.view;
   procedure Model_is              (Self : in out Item;   Now : in openGL.Model.view);
   function  owns_Graphics         (Self : in     Item)           return Boolean;

   function  physics_Model         (Self : in     Item)     return access physics.Model.item'Class;
   procedure physics_Model_is      (Self : in out Item;   Now : in physics.Model.view);

   function  Scale                 (Self : in     Item)     return Vector_3;
   procedure Scale_is              (Self : in out Item;   Now : in Vector_3);

   function  Mass                  (Self : in     Item)     return Real;
   function  is_Static             (Self : in     Item)     return Boolean;
   function  is_Kinematic          (Self : in     Item)     return Boolean;

   function  Depth_in_camera_space (Self : in     Item)     return Real;

   procedure mvp_Matrix_is         (Self : in out Item;   Now : in Matrix_4x4);
   function  mvp_Matrix            (Self : in     Item)     return Matrix_4x4;

   procedure is_Visible            (Self : in out Item;   Now : in Boolean);
   function  is_Visible            (Self : in     Item)     return Boolean;

   procedure key_Response_is       (Self : in out Item;   Now : in lace.Response.view);
   function  key_Response          (Self : in     Item)     return lace.Response.view;


   subtype physics_Object_view is physics.Object.view;
   subtype physics_Shape_view  is physics.Shape .view;

   function  Solid                 (Self : in     Item)     return physics_Object_view;
   procedure Solid_is              (Self : in out Item;   Now : in physics_Object_view);

   function  Shape                 (Self : in     Item)     return physics_Shape_view;


   function  to_GEL (the_Solid : in physics_Object_view) return gel.Sprite.view;


   -------------
   --- Dynamics
   --

   --- Bounds
   --

   function Bounds (Self : in Item) return Geometry_3d.bounding_Box;


   --- Site
   --

   function  Site    (Self : in     Item)         return Vector_3;
   procedure Site_is (Self : in out Item;   Now     : in Vector_3);
   procedure move    (Self : in out Item;   to_Site : in Vector_3);
   --
   --  Moves the sprite to a new site and recursively move children such that
   --  relative positions are maintained.


   --- Spin
   --

   function  Spin       (Self : in     Item)         return Matrix_3x3;
   procedure Spin_is    (Self : in out Item;   Now     : in Matrix_3x3);

   function  xy_Spin    (Self : in     Item)         return Radians;
   procedure xy_Spin_is (Self : in out Item;   Now     : in Radians);

   procedure rotate     (Self : in out Item;   to_Spin : in Matrix_3x3);
   --
   --  Rotates the sprite to a new spin and recursively moves and rotates children such that
   --  relative positions/orientations are maintained.


   --- Transform
   --

   function  Transform    (Self : in     Item)     return Matrix_4x4;
   procedure Transform_is (Self : in out Item;   Now : in Matrix_4x4);


   --- Speed
   --

   function  Speed     (Self : in     Item)          return Vector_3;
   procedure Speed_is  (Self : in out Item;   Now      : in Vector_3);

   procedure set_Speed (Self : in out Item;   to_Speed : in Vector_3);
   --
   --  Set Self and all children to given value.


   --- Gyre
   --

   function  Gyre     (Self : in     Item)         return Vector_3;
   procedure Gyre_is  (Self : in out Item;   Now     : in Vector_3);
   procedure set_Gyre (Self : in out Item;   to_Gyre : in Vector_3);
   --
   --  Set Self and all children to given value.


   --- Forces
   --

   procedure apply_Torque         (Self : in out Item;   Torque : in Vector_3);
   procedure apply_Torque_impulse (Self : in out Item;   Torque : in Vector_3);
   procedure apply_Force          (Self : in out Item;   Force  : in Vector_3);


   --- Mirrored Dynamics
   --

   procedure desired_Dynamics_are (Self : in out Item;   Site : in Vector_3;
                                                         Spin : in Quaternion);
   procedure interpolate_Motion   (Self : in out Item);


   --- Hierachy
   --

   type DoF_Limits is
      record
         Low  : math.Real;
         High : math.Real;
      end record;

   function  parent_Joint (Self : in     Item) return gel.Joint.view;
   function  child_Joints (Self : in     Item) return gel.Joint.views;

   function  top_Parent   (Self : access Item) return gel.Sprite.view;
   function  Parent       (Self : in     Item) return gel.Sprite.view;
   function  tree_Depth   (Self : in     Item) return Natural;

   procedure detach       (Self : in out Item;   the_Child : gel.Sprite.view);

   no_such_Child : exception;


   type Action is access procedure (the_Sprite : in out Item'Class);

   procedure apply (Self : in out Item;   do_Action : Action);
   --
   -- Applies an action to a sprite and its children recursively.


   ---  Hinge
   --
   procedure attach_via_Hinge (Self : access Item;   the_Child         : in      Sprite.view;
                                                     pivot_Axis        : in      Vector_3;
                                                     Anchor            : in      Vector_3;
                                                     child_Anchor      : in      Vector_3;
                                                     low_Limit         : in      Real;
                                                     high_Limit        : in      Real;
                                                     collide_Connected : in      Boolean;
                                                     new_joint         :     out gel.Joint.view);


   procedure attach_via_Hinge (Self : access Item;   the_Child    : in      Sprite.view;
                                                     pivot_Axis   : in      Vector_3;
                                                     pivot_Anchor : in      Vector_3;
                                                     low_Limit    : in      Real;
                                                     high_Limit   : in      Real;
                                                     new_joint    :     out gel.Joint.view);

   procedure attach_via_Hinge (Self : access Item;   the_Child  : in      Sprite.view;
                                                     pivot_Axis : in      Vector_3;
                                                     low_Limit  : in      Real;
                                                     high_Limit : in      Real;
                                                     new_joint  :     out gel.Joint.view);
   --
   --  Uses midpoint between Self and the_Child sprite as pivot_Anchor.


   procedure attach_via_Hinge (Self : access Item;   the_Child         : in     Sprite.view;
                                                     Frame_in_parent   : in     Matrix_4x4;
                                                     Frame_in_child    : in     Matrix_4x4;
                                                     Limits            : in     DoF_Limits;
                                                     collide_Connected : in     Boolean;
                                                     new_joint         :    out gel.Joint.view);


   ---  Ball/Socket
   --
   procedure attach_via_ball_Socket (Self : access Item;   the_Child    : in     Sprite.view;
                                                           pivot_Anchor : in     Vector_3;
                                                           pivot_Axis   : in     Matrix_3x3;
                                                           pitch_Limits : in     DoF_Limits;
                                                           yaw_Limits   : in     DoF_Limits;
                                                           roll_Limits  : in     DoF_Limits;
                                                           new_joint    :    out gel.Joint.view);

   procedure attach_via_ball_Socket (Self : access Item;   the_Child       : in     Sprite.view;
                                                           Frame_in_parent : in     Matrix_4x4;
                                                           Frame_in_child  : in     Matrix_4x4;
                                                           pitch_Limits    : in     DoF_Limits;
                                                           yaw_Limits      : in     DoF_Limits;
                                                           roll_Limits     : in     DoF_Limits;
                                                           new_joint       :    out gel.Joint.view);

   --- Graphics
   --
   procedure program_Parameters_are  (Self : in out Item;   Now : in opengl.Program.Parameters_view);
   function  program_Parameters      (Self : in     Item)  return opengl.Program.Parameters_view;


   --- Physics
   --
   procedure rebuild_Shape (Self : in out Item);
   procedure rebuild_Solid (Self : in out Item;   at_Site : in Vector_3);



private

   type access_Joint_views is access all Joint.views;

   use type Joint.view;
   package joint_Vectors is new ada.Containers.Vectors (Positive, Joint.view);


   --  protected
   --  type safe_Matrix_4x4
   --  is
   --     function  Value       return Matrix_4x4;
   --     procedure Value_is (Now : in Matrix_4x4);
   --     procedure Site_is  (Now : in Vector_3);
   --
   --  private
   --     the_Value : Matrix_4x4 := Identity_4x4;
   --  end safe_Matrix_4x4;


   -----------------
   --- Interpolation
   --

   type site_Interpolation is
      record
         Initial : Vector_3;
         Desired : Vector_3;
      end record;

   type spin_Interpolation is
      record
         Initial : Quaternion;
         Desired : Quaternion;
      end record;

   type Interpolation is
      record
         Site    : site_Interpolation;
         Spin    : spin_Interpolation;
         Percent : unit_Percentage;
      end record;


   protected
   type safe_Interpolation
   is
      procedure set (desired_Site : in Vector_3;
                     desired_Spin : in Quaternion);
      procedure get (Site : out Vector_3;
                     Spin : out Quaternion);
   private
      Safe : Interpolation := (Site    => (Initial => Origin_3D,
                                           Desired => Origin_3D),
                               Spin    => (Initial => (R =>  0.0,
                                                       V => (0.0, 1.0, 0.0)),
                                           Desired => (R =>  0.0,
                                                       V => (0.0, 1.0, 0.0))),
                               Percent => 0.0);
   end safe_Interpolation;


   ---------------
   --- Sprite Item
   --

   type Item is limited new lace.Subject_and_deferred_Observer.item with
      record
         Id                      : gel.sprite_Id := null_sprite_Id;

         Visual                  : openGL.Visual.view := new openGL.Visual.item;
         program_Parameters      : openGL.program.Parameters_view;
         owns_Graphics           : Boolean;

         physics_Model           : physics.Model.view;
         owns_Physics            : Boolean;

         World                   : World_view;
         Shape                   : physics_Shape_view;
         Solid                   : physics_Object_view;
         is_Kinematic            : Boolean;

         Depth_in_camera_space   : Real;

         Interpolation           : safe_Interpolation;

         parent_Joint            : gel.Joint.view;
         child_Joints            : joint_Vectors.Vector;

         is_Visible              : Boolean := True;
         key_Response            : lace.Response.view;

         is_Destroyed            : Boolean := False;
      end record;


   null_Sprites : constant Sprite.views (1 .. 0) := (others => null);

end gel.Sprite;
