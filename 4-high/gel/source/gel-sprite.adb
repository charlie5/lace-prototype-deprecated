with
     gel.hinge_Joint,
     gel.  any_Joint,
     gel.World,

     ada.Tags,
     ada.unchecked_Deallocation,
     ada.unchecked_Conversion;

package body gel.Sprite
is
   use ada.Tags,
       linear_Algebra_3D;


   procedure log (Message : in String)
--                    renames ada.text_IO.put_Line;
   is null;


   ------------------
   --- Initialisation
   --

   procedure rebuild_Shape (Self : in out Item)
   is
      use type physics.Model.shape_Kind,
               physics.Model.View;

      the_Scale : aliased Vector_3;

   begin
--        Self.Shape := Self.World.Space.new_Shape (Self.physics_Model);

      -- Old
      if Self.physics_Model = null then
         return;
      end if;

      the_Scale := Self.physics_Model.Scale;

      case Self.physics_Model.shape_Info.Kind
      is
         when physics.Model.Cube =>
            Self.Shape := physics_Shape_view (Self.World.Space.        new_box_Shape (Self.physics_Model.shape_Info.half_Extents));

         when physics.Model.a_Sphere =>
            Self.Shape := physics_Shape_view (Self.World.Space.     new_sphere_Shape (Self.physics_Model.shape_Info.sphere_Radius));

         when physics.Model.multi_Sphere =>
            Self.Shape := physics_Shape_view (Self.World.Space.new_multisphere_Shape (Self.physics_Model.shape_Info.Sites.all,
                                                                                      Self.physics_Model.shape_Info.Radii.all));
         when physics.Model.Cone =>
            Self.Shape := physics_Shape_view (Self.World.Space.       new_cone_Shape (Radius => Real (Self.physics_Model.Scale (1) / 2.0),
                                                                                      Height => Real (Self.physics_Model.Scale (2))));
         when physics.Model.a_Capsule =>
            Self.Shape := physics_Shape_view (Self.World.Space.    new_capsule_Shape (Self.physics_Model.shape_Info.lower_Radius,
                                                                                      Self.physics_Model.shape_Info.Height));
         when physics.Model.Cylinder =>
            Self.Shape := physics_Shape_view (Self.World.Space.   new_cylinder_Shape (Self.physics_Model.shape_Info.half_Extents));

         when physics.Model.Hull =>
            Self.Shape := physics_Shape_view (Self.World.Space.new_convex_hull_Shape (Self.physics_Model.shape_Info.Points.all));

         when physics.Model.Mesh =>
            Self.Shape := physics_Shape_view (Self.World.Space       .new_mesh_Shape (Self.physics_Model.shape_Info.Model));

         when physics.Model.Plane =>
            Self.Shape := physics_Shape_view (Self.World.Space.      new_plane_Shape (Self.physics_Model.Shape_Info.plane_Normal,
                                                                                      Self.physics_Model.Shape_Info.plane_Offset));
         when physics.Model.Heightfield =>
            Self.Shape := physics_Shape_view (Self.World.Space.new_heightfield_Shape (Self.physics_Model.shape_Info.Heights.all,
                                                                                      Self.physics_Model.Scale));
         when physics.Model.Circle =>
            Self.Shape := physics_Shape_view (Self.World.Space.     new_circle_Shape (Self.physics_Model.shape_Info.circle_Radius));

         when physics.Model.Polygon =>
            Self.Shape := physics_Shape_view (Self.World.Space.    new_polygon_Shape (physics.space.polygon_Vertices (Self.physics_Model.shape_Info.Vertices (1 .. Self.physics_Model.shape_Info.vertex_Count))));
      end case;

   end rebuild_Shape;



   procedure rebuild_Solid (Self : in out Item;   at_Site : Vector_3)
   is
      use Physics.Object;
   begin
      if Self.Solid /= null
      then
         raise Program_Error;
      end if;

      Self.Solid := physics_Object_view (Self.World.Space.new_Object (physics.Shape.view (Self.Shape),
                                                                      Self.physics_Model.Mass,
                                                                      Self.physics_Model.Friction,
                                                                      Self.physics_Model.Restitution,
                                                                      at_Site,
                                                                      Self.is_Kinematic));
   end rebuild_Solid;



   procedure define (Self : access Item;   World          : in     World_view;
                                           at_Site        : in     Vector_3;
                                           graphics_Model : access openGL. Model.item'Class;
                                           physics_Model  : access physics.Model.item'Class;
                                           owns_Graphics  : in     Boolean;
                                           owns_Physics   : in     Boolean;
                                           is_Kinematic   : in     Boolean := False)
   is
      use type physics.Model.view;
   begin
      Self.Id             := World.new_sprite_Id;
      Self.World          := World;

      Self.Visual.Model_is (graphics_Model.all'unchecked_Access);
      Self.physics_Model  := physics.Model.view (physics_Model);
      Self.owns_Graphics  := owns_Graphics;
      Self.owns_Physics   := owns_Physics;

      Self.is_Kinematic   := is_Kinematic;
      --  set_Translation (Self.Transform, To => physics_Model.Site);

      --  Physics
      --
      if Self.physics_Model /= null
      then
         Self.rebuild_Shape;
         Self.rebuild_Solid (at_Site);
         null;
      end if;
   end define;



   procedure destroy (Self : access Item;   and_Children : in Boolean)
   is
      use gel.Joint;
   begin
      if Self.is_Destroyed
      then
         raise Error with "Sprite is already destroyed.";
      end if;

      -- Detach parent, if any.
      --
      if Self.parent_Joint /= null then
         Self.parent_Joint.Sprite_A.detach (Sprite.view (Self));
      end if;

      -- Detach children, if any.
      --
      while not Self.child_Joints.is_Empty
      loop
         declare
            child_Sprite : constant Sprite.view := Self.child_Joints.Last_Element.Sprite_B.all'Access;
         begin
            Self.detach (child_Sprite);

            if and_Children
            then
               destroy (child_Sprite, and_Children);   -- Recurse.
            end if;
         end;
      end loop;

      Self.is_Destroyed := True;
      Self.World.destroy (Sprite.view (Self));

      lace.Subject_and_deferred_Observer.item (Self.all).destroy;   -- Destroy base class.
   end destroy;



   function is_Destroyed (Self : in Item) return Boolean
   is
   begin
      return Self.is_Destroyed;
   end is_Destroyed;



   procedure free (Self : in out View)
   is
      pragma assert (Self.is_Destroyed);

      use gel.Joint,
          physics.Model,
          physics.Object,
          physics.Shape;

      procedure deallocate is new ada.unchecked_Deallocation (Sprite.item'Class, Sprite.view);
      procedure deallocate is new ada.unchecked_Deallocation (Joint.views,       access_Joint_views);
      pragma Unreferenced (deallocate);

      child_Joint : Joint.view;

   begin
      for Each in 1 .. Integer (Self.child_Joints.Length)
      loop
         child_Joint := Self.child_Joints.Element (Each);
         free (child_Joint);
      end loop;

      if Self.owns_Physics
      then
         free (Self.physics_Model);
      end if;

      free (Self.Shape);
      free (Self.Solid);

      deallocate (Self);
   end free;


   ----------
   --- Forge
   --
   package body Forge
   is

      function to_Sprite (Name           : in     String;
                          World          : in     World_view;
                          at_Site        : in     Vector_3;
                          graphics_Model : access openGL. Model.item'Class;
                          physics_Model  : access physics.Model.item'Class;
                          owns_Graphics  : in     Boolean;
                          owns_Physics   : in     Boolean;
                          is_Kinematic   : in     Boolean := False) return Item
      is
      begin
         return Self : Item := (lace.Subject_and_deferred_Observer.forge.to_Subject_and_Observer (Name)
                                with others => <>)
         do
            Self.define (World, at_Site, graphics_Model, physics_Model, owns_Graphics, owns_Physics, is_Kinematic);
         end return;
      end to_Sprite;



      function new_Sprite (Name           : in     String;
                           World          : in     World_view;
                           at_Site        : in     Vector_3;
                           graphics_Model : access openGL. Model.item'Class;
                           physics_Model  : access physics.Model.item'Class;
                           owns_Graphics  : in     Boolean := True;
                           owns_Physics   : in     Boolean := True;
                           is_Kinematic   : in     Boolean := False) return View
      is
         Self : constant View := new Item' (to_Sprite (Name,
                                                       World,
                                                       at_Site,
                                                       graphics_Model,
                                                       physics_Model,
                                                       owns_Graphics,
                                                       owns_Physics,
                                                       is_Kinematic));
      begin
         return Self;
      end new_Sprite;

   end Forge;


   --------------
   --- Attributes
   --

   function World (Self : in Item) return access gel.World.item'Class
   is
   begin
      return Self.World;
   end World;



   function Id (Self : in Item) return gel.sprite_Id
   is
   begin
      return Self.Id;
   end Id;



   procedure Id_is (Self : in out Item;   Now : in gel.sprite_Id)
   is
   begin
      Self.Id := Now;
   end Id_is;



   function Depth_in_camera_space (Self : in Item) return Real
   is
   begin
      return Self.Depth_in_camera_space;
   end Depth_in_camera_space;



   function Mass (Self : in Item) return Real
   is
   begin
      return Self.physics_Model.Mass;
   end Mass;



   function is_Static (Self : in Item) return Boolean
   is
   begin
      return Self.Mass = 0.0;
   end is_Static;



   function is_Kinematic (Self : in Item) return Boolean
   is
   begin
      return Self.is_Kinematic;
   end is_Kinematic;



   procedure mvp_Matrix_is (Self : in out Item;   Now : in Matrix_4x4)
   is
   begin
      Self.Visual.mvp_Transform_is (Now);
      Self.Depth_in_camera_space := Now (4, 3);
   end mvp_Matrix_is;



   function mvp_Matrix (Self : in Item) return Matrix_4x4
   is
   begin
      return Self.Visual.mvp_Transform;
   end mvp_Matrix;



   procedure is_Visible (Self : in out Item;   Now : in Boolean)
   is
   begin
      Self.is_Visible := Now;
   end is_Visible;



   function is_Visible (Self : in Item) return Boolean
   is
   begin
      return Self.is_Visible;
   end is_Visible;



   procedure key_Response_is (Self : in out Item;   Now : in lace.Response.view)
   is
   begin
      Self.key_Response := Now;
   end key_Response_is;



   function key_Response (Self : in Item) return lace.Response.view
   is
   begin
      return Self.key_Response;
   end key_Response;



   function Visual (Self : access Item) return openGL.Visual.view
   is
   begin
      return Self.Visual;
   end Visual;



   function graphics_Model (Self : in Item) return openGL.Model.view
   is
   begin
      return Self.visual.Model;
   end graphics_Model;



   procedure Model_is (Self : in out Item;   Now : in openGL.Model.view)
   is
   begin
      Self.Visual.Model_is (Now);
   end Model_is;



   function owns_Graphics  (Self : in Item) return Boolean
   is
   begin
      return Self.owns_Graphics;
   end owns_Graphics;



   function physics_Model (Self : in Item) return access physics.Model.item'class
   is
   begin
      return Self.physics_Model;
   end physics_Model;



   procedure physics_Model_is (Self : in out Item;   Now : in physics.Model.view)
   is
   begin
      Self.physics_Model := Now;
   end physics_Model_is;



   procedure Scale_is (Self : in out Item;   Now : in math.Vector_3)
   is
   begin
      Self.physics_Model.Scale_is (Now);
      Self.World .update_Scale    (Self'unchecked_Access, +Now);
   end Scale_is;



   function Scale (Self : in Item) return Vector_3
   is
   begin
      return Self.physics_Model.Scale;
   end Scale;



   function Solid (Self : in Item) return physics_Object_view
   is
   begin
      return Self.Solid;
   end Solid;



   procedure Solid_is (Self : in out Item;   Now : in physics_Object_view)
   is
   begin
      Self.Solid := Now;
   end Solid_is;



   function to_GEL (the_Solid : in physics_Object_view) return gel.Sprite.view
   is
   begin
      return gel.Sprite.view (the_Solid.user_Data);
   end to_GEL;



   function Shape (Self : in Item) return physics_Shape_view
   is
   begin
      return Self.Shape;
   end Shape;


   -------------
   --- Dynamics
   --

   --- Bounds
   --

   function Bounds (Self : in Item) return Geometry_3d.bounding_Box
   is
      use Geometry_3d;
   begin
      return Self.graphics_Model.Bounds.Box + Self.Site;
   end Bounds;


   --- Site
   --

   function Site (Self : in Item) return Vector_3
   is
   begin
      return Self.Solid.Site;
   end Site;



   procedure Site_is (Self : in out Item;   Now : in Vector_3)
   is
   begin
      Self.Solid.Site_is (Now);
   end Site_is;



   procedure move (Self : in out Item;   to_Site : in Vector_3)
   is
      the_Offset   : constant Vector_3 := to_Site - Self.Site;
      child_Sprite : Sprite.view;
   begin
      --  Do children.
      --
      for i in 1 .. Integer (Self.child_Joints.Length)
      loop
         child_Sprite := Self.child_Joints.Element (i).Sprite_B;
         child_Sprite.move (to_site => child_Sprite.Site + the_Offset);            -- Recurse.
      end loop;

      Self.Site_is (to_Site);
   end move;



   procedure set_Speed (Self : in out Item;   to_Speed : in Vector_3)
   is
      child_Sprite : Sprite.view;
   begin
      --  Do children.
      --
      for i in 1 .. Integer (Self.child_Joints.Length)
      loop
         child_Sprite := Self.child_Joints.Element (i).Sprite_B;
         child_Sprite.set_Speed (to_Speed);
      end loop;

      Self.Speed_is (to_Speed);
   end set_Speed;



   function Spin (Self : in Item) return Matrix_3x3
   is
   begin
      return Self.Solid.Spin;
   end Spin;



   procedure Spin_is (Self : in out Item;   Now : in Matrix_3x3)
   is
      use type Physics.Object.view;
      --  Transform : Matrix_4x4 := Self.Transform.Value;
   begin
      --  set_Rotation      (Self.Transform, Now);
      --  Self.Transform_is (Transform);

      --  if Self.Solid /= null then
      Self.Solid.Spin_is (Now);
      --  end if;
   end Spin_is;



   function xy_Spin (Self : in Item) return Radians
   is
   begin
      return Self.Solid.xy_Spin;
   end xy_Spin;



   procedure xy_Spin_is (Self : in out Item;   Now : in Radians)
   is
   begin
      Self.World.set_xy_Spin (Self'unchecked_Access, Now);
   end xy_Spin_is;



   procedure rotate (Self : in out Item;   to_Spin : in Matrix_3x3)
   is
      the_spin_Delta : constant Matrix_3x3 := to_Spin * Inverse (Self.Spin);   -- The rotation matrix describing the amount by which Self has rotated.

      procedure spin_Children (the_Sprite : in Sprite.item'class)
      is
      begin
         if the_Sprite.child_Joints.Is_Empty then
            return;
         end if;

         declare
            child_Sprite    : Sprite.view;
            the_site_Offset : Vector_3;
         begin
            for i in 1 .. Integer (the_Sprite.child_Joints.Length)
            loop
               child_Sprite    := the_Sprite.child_Joints.Element (i).Sprite_B;
               the_site_Offset := the_spin_Delta * (child_Sprite.Site - Self.Site) ;

               child_Sprite.Site_is (Self.Site      + the_site_Offset);
               child_Sprite.Spin_is (the_spin_Delta * child_Sprite.Spin);

               spin_Children (child_Sprite.all);     -- Recurse.
            end loop;
         end;
      end spin_Children;

   begin
      spin_Children (Self);      -- Do children.
      Self.Spin_is (to_Spin);
   end rotate;



   function Transform (Self : in Item) return Matrix_4x4
   is
   begin
      --  return Self.Transform.Value;
      return Self.Solid.Transform;
   end Transform;



   procedure Transform_is (Self : in out Item;   Now : in Matrix_4x4)
   is
   begin
      --  Self.Transform.Value_is (Now);
      Self.Solid.Transform_is (Now);
   end Transform_is;



   function Speed (Self : in Item) return Vector_3
   is
   begin
      return Self.Solid.Speed;
   end Speed;



   procedure Speed_is (Self : in out Item;   Now : in Vector_3)
   is
   begin
      Self.World.set_Speed (Self'unchecked_Access, Now);
   end Speed_is;



   function Gyre (Self : in Item) return Vector_3
   is
   begin
      return Self.Solid.Gyre;
   end Gyre;



   procedure Gyre_is (Self : in out Item;   Now : in Vector_3)
   is
   begin
      Self.Solid.Gyre_is (Now);
   end Gyre_is;



   procedure set_Gyre (Self : in out Item;   to_Gyre : in Vector_3)
   is
      child_Sprite : Sprite.view;
   begin
      --  Do children.
      --
      for i in 1 .. Integer (Self.child_Joints.Length)
      loop
         child_Sprite := Self.child_Joints.Element (i).Sprite_B;
         child_Sprite.set_Gyre (to_Gyre);
      end loop;

      Self.Gyre_is (to_Gyre);
   end set_Gyre;


   --- Forces
   --

   procedure apply_Force (Self : in out Item;   Force  : in Vector_3)
   is
      the_Force : aliased constant Vector_3 := Force;
   begin
      Self.World.apply_Force (Self'unchecked_Access, the_Force);
   end apply_Force;



   procedure apply_Torque (Self : in out Item;   Torque : in Vector_3)
   is
      the_Torque : constant Vector_3 := Torque;
   begin
      Self.Solid.apply_Torque (the_Torque);
   end apply_Torque;



   procedure apply_Torque_impulse (Self : in out Item;   Torque : in Vector_3)
   is
      the_Torque : constant Vector_3 := Torque;
   begin
      Self.Solid.apply_Torque_impulse (the_Torque);
   end apply_Torque_impulse;


   --  Mirrored Dynamics
   --

   protected
   body safe_Interpolation
   is
      procedure set (desired_Site : in Vector_3;
                     desired_Spin : in Quaternion)
      is
      begin
         Safe.Site.initial := Safe.Site.desired;
         Safe.Site.desired := desired_Site;

         Safe.Spin.initial := Safe.Spin.desired;
         Safe.Spin.desired := desired_Spin;

         Safe.Percent      := 0.0;
      end set;


      procedure get (Site : out Vector_3;
                     Spin : out Quaternion)
      is
      begin
         Site := Interpolated (Safe.Site.initial,
                               Safe.Site.desired,
                               Safe.Percent);

         Spin := Interpolated (Safe.Spin.initial,
                               Safe.Spin.desired,
                               Safe.Percent);

         if gel.World.interpolation_Steps = 0
         then
            Safe.Percent := 100.0;
         else
            Safe.Percent := Percentage'Min (  Safe.Percent
                                            + to_Percentage (1.0 / Real (gel.World.interpolation_Steps + 1)),
                                              unit_Percentage'Last);
         end if;
      end get;

   end safe_Interpolation;



   procedure desired_Dynamics_are (Self : in out Item;   Site : in Vector_3;
                                                         Spin : in Quaternion)
   is
   begin
      Self.Interpolation.set (desired_Site => Site,
                              desired_Spin => Spin);
   end desired_Dynamics_are;



   procedure interpolate_Motion (Self : in out Item)
   is
   begin
      if Self.is_Static then
         return;
      end if;

      declare
         new_Site : Vector_3;
         new_Spin : Quaternion;
      begin
         Self.Interpolation.get (new_Site,
                                 new_Spin);
         Self.Site_is (new_Site);
         Self.Spin_is (Transpose (to_Matrix (new_Spin)));
      end;
   end interpolate_Motion;



   --------------
   --- Operations
   --

   --- Hierachy
   --

   function parent_Joint (Self : in Item) return gel.Joint.view
   is
   begin
      return Self.parent_Joint;
   end parent_Joint;



   function child_Joints (Self : in Item) return gel.Joint.views
   is
      the_Joints : Joint.views (1 .. Integer (Self.child_Joints.Length));
   begin
      for i in the_Joints'Range
      loop
         the_Joints (i) := Self.child_Joints.Element (i);
      end loop;

      return the_Joints;
   end child_Joints;



   function top_Parent (Self : access Item) return gel.Sprite.view
   is
   begin
      if Self.parent_Joint = null
      then   return gel.Sprite.view (Self);
      else   return Self.parent_Joint.Sprite_A.top_Parent;   -- Recurse.
      end if;
   end top_Parent;



   function Parent (Self : in Item) return gel.Sprite.view
   is
   begin
      if Self.parent_Joint = null
      then   return null;
      else   return Self.parent_Joint.Sprite_A;
      end if;
   end Parent;



   function tree_Depth (Self : in Item) return Natural
   is
      Parent : Sprite.view := Self.Parent;
      Depth  : Natural     := 0;
   begin
      while Parent /= null
      loop
         Depth  := Depth + 1;
         Parent := Parent.Parent;
      end loop;

      return Depth;
   end tree_Depth;



   procedure apply (Self : in out Item;   do_Action : Action)
   is
   begin
      do_Action (Self);

      for i in 1 .. Integer (Self.child_Joints.Length)
      loop
         Self.child_Joints.Element (i).Sprite_B.apply (do_Action);
      end loop;
   end apply;



   procedure attach (Self : access Item;   the_Child : in Sprite.view;
                                           the_Joint : in gel.Joint.view)
   is
   begin
      log ("Attaching " & the_Child.Id'Image & " to " & Self.Id'Image);

      Self.child_Joints.append (the_Joint);

      the_Child.parent_Joint := the_Joint;
      the_Child.relay_responseless_Events (To => Self);
   end attach;



   procedure detach (Self : in out Item;   the_Child : gel.Sprite.view)
   is
      childs_Joint : Joint.view;
   begin
      log ("Detaching " & the_Child.Id'Image & " from " & Self.Id'Image);

      for i in 1 .. Integer (Self.child_Joints.Length)
      loop
         if Self.child_Joints.Element (i).Sprite_B = the_Child
         then
            childs_Joint := Self.child_Joints.Element (i);

            Self.child_Joints.delete (i);
            the_Child.parent_Joint := null;
            Self.World.destroy (childs_Joint);

            return;
         end if;
      end loop;

      raise no_such_Child;
   end detach;



   --  Hinge
   --
   procedure attach_via_Hinge (Self : access Item;   the_Child         : in      Sprite.view;
                                                     pivot_Axis        : in      Vector_3;
                                                     Anchor            : in      Vector_3;
                                                     child_Anchor      : in      Vector_3;
                                                     low_Limit         : in      Real;
                                                     high_Limit        : in      Real;
                                                     collide_Connected : in      Boolean;
                                                     new_joint         :     out gel.Joint.view)
   is
      the_Joint : constant gel.hinge_Joint.view := new gel.hinge_Joint.item;
   begin
      the_Joint.define (Self.World.Space,
                        Self,       the_Child,
                        pivot_Axis,
                        Anchor,     child_Anchor,
                        low_Limit,  high_Limit,
                        collide_Connected);

      the_Joint.Limits_are (low_Limit, high_Limit);

      Self.attach (the_Child, the_Joint.all'Access);

      new_Joint := the_Joint.all'Access;
   end attach_via_Hinge;



   procedure attach_via_Hinge (Self : access Item;   the_Child    : in     Sprite.view;
                                                     pivot_Axis   : in     Vector_3;
                                                     pivot_Anchor : in     Vector_3;
                                                     low_Limit    : in     Real;
                                                     high_Limit   : in     Real;
                                                     new_joint    :    out gel.Joint.view)
   is
      the_Joint : constant gel.hinge_Joint.view := new gel.hinge_Joint.item;
   begin
      the_Joint.define (in_Space     => Self.World.Space,
                        Sprite_A     => Self,
                        Sprite_B     => the_Child,
                        pivot_Axis   => pivot_Axis,
                        pivot_Anchor => pivot_Anchor);

      the_Joint.Limits_are (low_Limit, high_Limit);

      Self.attach (the_Child, the_Joint.all'Access);

      new_Joint := the_Joint.all'Access;
   end attach_via_Hinge;



   procedure attach_via_Hinge (Self : access Item;   the_Child  : in     Sprite.view;
                                                     pivot_Axis : in     Vector_3;
                                                     low_Limit  : in     Real;
                                                     high_Limit : in     Real;
                                                     new_joint  :    out gel.Joint.view)
   is
      the_Joint : constant gel.hinge_Joint.view := new gel.hinge_Joint.item;
   begin
      the_Joint.define (in_Space   => Self.World.Space,
                        Sprite_A   => Self,
                        Sprite_B   => the_Child,
                        pivot_Axis => pivot_Axis);

      the_Joint.Limits_are (low_Limit, high_Limit);

      Self.attach (the_Child, the_Joint.all'Access);

      new_Joint := the_Joint.all'Access;
   end attach_via_Hinge;



   procedure attach_via_Hinge (Self : access Item;   the_Child         : in     Sprite.view;
                                                     Frame_in_parent   : in     Matrix_4x4;
                                                     Frame_in_child    : in     Matrix_4x4;
                                                     Limits            : in     DoF_Limits;
                                                     collide_Connected : in     Boolean;
                                                     new_joint         :    out gel.Joint.view)
   is
      the_Joint : constant gel.hinge_Joint.view := new gel.hinge_Joint.item;
   begin
      the_Joint.define (Self.World.Space,
                        Self,             the_Child,
                        Frame_in_parent,  Frame_in_child,
                        Limits.Low,       Limits.High,
                        collide_Connected);

      the_Joint.Limits_are (limits.Low, limits.High);

      Self.attach (the_Child, the_Joint.all'Access);

      new_Joint := the_Joint.all'Access;
   end attach_via_Hinge;


   --  Ball/Socket
   --

   procedure internal_attach_via_ball_Socket (Self : access Item;   the_Child    : in Sprite.view;
                                                                    pitch_Limits : in DoF_Limits;
                                                                    yaw_Limits   : in DoF_Limits;
                                                                    roll_Limits  : in DoF_Limits;
                                                                    the_Joint    : in gel.any_Joint.view)
   is
      use gel.any_Joint;
   begin
      the_Joint.low_Bound_is  (Pitch, pitch_Limits.Low);
      the_Joint.low_Bound_is  (Yaw,   yaw_Limits  .Low);
      the_Joint.low_Bound_is  (Roll,  roll_Limits .Low);

      the_Joint.high_Bound_is (Pitch, pitch_Limits.High);
      the_Joint.high_Bound_is (Yaw,   yaw_Limits  .High);
      the_Joint.high_Bound_is (Roll,  roll_Limits .High);

      Self.attach (the_Child,  the_Joint.all'Access);
   end internal_attach_via_ball_Socket;



   procedure attach_via_ball_Socket (Self : access Item;   the_Child    : in     Sprite.view;
                                                           pivot_Anchor : in     Vector_3;
                                                           pivot_Axis   : in     Matrix_3x3;
                                                           pitch_Limits : in     DoF_Limits;
                                                           yaw_Limits   : in     DoF_Limits;
                                                           roll_Limits  : in     DoF_Limits;
                                                           new_joint    :    out gel.Joint.view)
   is
      the_Joint : constant gel.any_Joint.view := new gel.any_Joint.item;
   begin
      the_Joint.define (Self.World.Space,
                        Self,         the_Child,
                        pivot_Anchor, pivot_Axis);

      Self.internal_attach_via_ball_Socket (the_Child,
                                            pitch_Limits,
                                            yaw_Limits,
                                            roll_Limits,
                                            the_joint);
      new_Joint := the_Joint.all'Access;
   end attach_via_ball_Socket;



   procedure attach_via_ball_Socket (Self : access Item;   the_Child       : in     Sprite.view;
                                                           Frame_in_parent : in     Matrix_4x4;
                                                           Frame_in_child  : in     Matrix_4x4;
                                                           pitch_Limits    : in     DoF_Limits;
                                                           yaw_Limits      : in     DoF_Limits;
                                                           roll_Limits     : in     DoF_Limits;
                                                           new_joint       :    out gel.Joint.view)
   is
      the_Joint : constant gel.any_Joint.view := new gel.any_Joint.item;
   begin
      the_Joint.define (Self.World.Space,
                        Self,             the_Child,
                        Frame_in_parent,  Frame_in_child);

      Self.internal_attach_via_ball_Socket (the_Child,
                                            pitch_Limits,
                                            yaw_Limits,
                                            roll_Limits,
                                            the_joint);
      new_Joint := the_Joint.all'Access;
   end attach_via_ball_Socket;



   ------------
   --- Graphics
   --

   procedure program_Parameters_are  (Self : in out Item;   Now : in opengl.Program.Parameters_view)
   is
   begin
      Self.program_Parameters := Now;
   end program_Parameters_are;



   function program_Parameters (Self : in Item) return opengl.Program.Parameters_view
   is
   begin
      return Self.program_Parameters;
   end program_Parameters;


   -----------
   --- Utility
   --

   function to_Hash (Self : in ada.Tags.Tag) return ada.Containers.Hash_type
   is
      function Converted is new ada.unchecked_Conversion (ada.Tags.Tag, ada.Containers.Hash_type);
   begin
      return Converted (Self);
   end to_Hash;
   pragma Unreferenced (to_Hash);



   --  protected
   --  body safe_Matrix_4x4
   --  is
   --     function Value return Matrix_4x4
   --     is
   --     begin
   --        return the_Value;
   --     end Value;
   --
   --     procedure Value_is (Now : in Matrix_4x4)
   --     is
   --     begin
   --        the_Value := Now;
   --     end Value_is;
   --
   --     procedure Site_is (Now : in Vector_3)
   --     is
   --     begin
   --        the_Value (4, 1) := Now (1);
   --        the_Value (4, 2) := Now (2);
   --        the_Value (4, 3) := Now (3);
   --     end Site_is;
   --  end safe_Matrix_4x4;


end gel.Sprite;
