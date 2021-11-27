with
     bullet_c.Binding,
     bullet_c.ray_Collision,

     c_math_c.Vector_3,
     c_math_c.Conversion,
     c_math_c.Pointers,

     bullet_physics.Shape,
     bullet_physics.Joint,

     Swig,
     lace.Any,
     interfaces.C,
     ada.unchecked_Conversion;

package body bullet_Physics.Space
is

   use bullet_c.Binding,
       bullet_c.Pointers,
       c_math_c.Conversion,
       Interfaces;

   function to_Object_view is new ada.unchecked_Conversion (swig.void_ptr,
                                                            physics.Object.view);

   ----------
   --- Forge
   --

   function to_Space return Item
   is
   begin
      return Self : Item
      do
         Self.C := bullet_c.Binding.b3d_new_Space;
      end return;
   end to_Space;


   overriding
   procedure destruct (Self : in out Item)
   is
   begin
      bullet_c.Binding.b3d_free_Space (Self.C);
   end destruct;


   ---------
   --- Shape
   --

   overriding
   function new_Shape (Self : access Item;   Model : in physics.Model.view) return physics.Shape.view
   is
   begin
      raise Error with "TODO";
      return null;
   end new_Shape;


   overriding
   function new_sphere_Shape (Self : access Item;   Radius : in Real := 0.5) return physics.Shape.view
   is
      pragma unreferenced (Self);
      the_Sphere : constant physics.Shape .view := bullet_physics.Shape.new_sphere_Shape (Radius);
   begin
      return the_Sphere;
   end new_sphere_Shape;


   overriding
   function new_box_Shape (Self : access Item;   half_Extents : in Vector_3 := (0.5, 0.5, 0.5)) return physics.Shape.view
   is
      pragma Unreferenced (Self);
      the_Box : constant physics.Shape.view := bullet_physics.Shape.new_box_Shape (half_Extents);
   begin
      return the_Box;
   end new_box_Shape;


   overriding
   function new_capsule_Shape (Self : access Item;   Radius : in Real :=  0.5;
                                                     Height : in Real) return physics.Shape.view
   is
      pragma unreferenced (Self);
      the_Capsule : constant physics.Shape .view := bullet_physics.Shape.new_capsule_Shape (Radii  => (Radius, Radius),
                                                                                            Height => Height);
   begin
      return the_Capsule;
   end new_capsule_Shape;


   overriding
   function new_cone_Shape (Self : access Item;   Radius : in Real := 0.5;
                                                  Height : in Real := 1.0) return physics.Shape.view
   is
      pragma unreferenced (Self);
      the_Cone : constant physics.Shape.view := bullet_physics.Shape.new_cone_Shape (Radius, Height);
   begin
      return the_Cone;
   end new_cone_Shape;


   overriding
   function new_cylinder_Shape (Self : access Item;   half_Extents : in Vector_3 := (0.5, 0.5, 0.5)) return physics.Shape.view
   is
      pragma unreferenced (Self);
      the_Cylinder : constant physics.Shape.view := bullet_physics.Shape.new_cylinder_Shape (half_Extents);
   begin
      return the_Cylinder;
   end New_Cylinder_Shape;


   overriding
   function new_heightfield_Shape (Self : access Item;   Heightfield  : in out physics.Heightfield;
                                                         Scale        : in     Vector_3) return physics.Shape.view
   is
      pragma unreferenced (Self);

      function height_Extent (Self : in physics.Heightfield) return Vector_2
      is
         Min : Real := Real'Last;
         Max : Real := Real'First;
      begin
         for Row in Self'Range (1)
         loop
            for Col in Self'Range (2)
            loop
               Min := Real'Min (Min, Self (Row, Col));
               Max := Real'Max (Max, Self (Row, Col));
            end loop;
         end loop;

         return (Min, Max);
      end height_Extent;

      function convert is new ada.unchecked_Conversion (physics.Space.Real_view,
                                                        c_math_c.Pointers.Real_Pointer);

      the_height_Extent : constant Vector_2           := height_Extent (Heightfield);
      the_Heightfield   : constant physics.Shape.view := bullet_physics.Shape.new_heightfield_Shape (Heightfield'Length (1),
                                                                                                     Heightfield'Length (2),
                                                                                                     convert (Heightfield (1, 1)'unchecked_Access),
                                                                                                     the_height_Extent (1),
                                                                                                     the_height_Extent (2),
                                                                                                     Scale);
   begin
      return the_Heightfield;
   end new_heightfield_Shape;


   overriding
   function new_multisphere_Shape (Self : access Item;   Sites   : in physics.vector_3_array;
                                                          Radii  : in Vector) return physics.Shape.view
   is
      pragma unreferenced (Self);
      the_multi_Sphere : constant physics.Shape.view := bullet_physics.Shape.new_multisphere_Shape (Sites, Radii);
   begin
      return the_multi_Sphere;
   end new_multisphere_Shape;


   overriding
   function new_plane_Shape (Self : access Item;   Normal : in Vector_3 := (0.0, 1.0, 0.0);
                                                   Offset : in Real     :=  0.0) return physics.Shape .view
   is
      pragma unreferenced (Self);
      the_Plane : constant physics.Shape.view := bullet_physics.Shape.new_plane_Shape (Normal, Offset);
   begin
      return the_Plane;
   end new_plane_Shape;


   overriding
   function new_convex_hull_Shape (Self : access Item;   Points : in physics.vector_3_array) return physics.Shape.view
   is
      pragma unreferenced (Self);
      the_Hull : constant physics.Shape.view := bullet_physics.Shape.new_convex_hull_Shape (Points);
   begin
      return the_Hull;
   end new_convex_hull_Shape;


   overriding
   function new_mesh_Shape (Self : access Item;   Points : access Physics.Geometry_3D.a_Model) return physics.Shape.view
   is
      pragma unreferenced (Self);
      the_Mesh : constant physics.Shape.view := bullet_physics.Shape.new_mesh_Shape (Points);
   begin
      return the_Mesh;
   end new_mesh_Shape;


   --  2D
   --

   overriding
   function new_circle_Shape (Self : access Item;   Radius : in Real := 0.5) return physics.Shape.view
   is
   begin
      raise physics.Space.unsupported_Shape with "Circle shape not allowed in bullet physics.";
      return null;
   end new_circle_Shape;


   overriding
   function new_polygon_Shape (Self : access Item;   Vertices : in physics.Space.polygon_Vertices) return physics.Shape.view
   is
   begin
      raise physics.Space.unsupported_Shape with "Polygon shape not allowed in bullet physics.";
      return null;
   end new_polygon_Shape;


   ------------
   ---  Objects
   --
   function Hash (the_C_Object : in bullet_c.Pointers.Object_pointer) return ada.Containers.Hash_type
   is
      function convert is new ada.unchecked_Conversion (bullet_c.Pointers.Object_pointer,
                                                        ada.Containers.Hash_type);
   begin
      return convert (the_C_Object);
   end Hash;


   overriding
   function  new_Object (Self : access Item;   of_Shape     : in physics.Shape .view;
                                               of_Mass      : in Real;
                                               Friction     : in Real;
                                               Restitution  : in Real;
                                               at_Site      : in Vector_3;
                                               is_Kinematic : in Boolean) return physics.Object.view
   is
      pragma unreferenced (Self);
      the_b3d_Object : constant bullet_Physics.Object.view := bullet_physics.Object.new_Object (Shape       => of_Shape,
                                                                                                Mass        => of_Mass,
                                                                                                Friction    => Friction,
                                                                                                Restitution => Restitution,
                                                                                                at_Site     => at_Site);
      the_Object : constant physics.Object.view := physics.Object.view (the_b3d_Object);

   begin
      return the_Object;
   end new_Object;


   overriding
   function object_Count (Self : in Item) return Natural
   is
   begin
      raise Error with "TODO";
      return 0;
   end object_Count;


   -----------
   ---  Joints
   --

   overriding
   function new_hinge_Joint (Self : access Item;   Object_A,
                                                   Object_B          : in physics.Object.view;
                                                   Anchor_in_A,
                                                   Anchor_in_B       : in Vector_3;
                                                   pivot_Axis        : in Vector_3;
                                                   low_Limit,
                                                   high_Limit        : in Real;
                                                   collide_Connected : in Boolean) return physics.Joint.hinge.view
   is
   begin
      raise Error with "TODO";
      return null;
   end new_hinge_Joint;


   overriding
   function new_hinge_Joint (Self : access Item;   Object_A : in physics.Object.view;
                                                   Frame_A  : in Matrix_4x4) return physics.Joint.hinge.view
   is
      pragma unreferenced (Self);
      the_Joint : constant physics.Joint.hinge.view := bullet_physics.Joint.new_hinge_Joint (Object_A, Frame_A);
   begin
      return the_Joint;
   end new_hinge_Joint;


   overriding
   function new_hinge_Joint (Self : access Item;   Object_A,
                                                   Object_B          : in physics.Object.view;
                                                   Frame_A,
                                                   Frame_B           : in Matrix_4x4;
                                                   low_Limit,
                                                   high_Limit        : in Real;
                                                   collide_Connected : in Boolean) return physics.Joint.hinge.view
   is
      pragma unreferenced (Self);
      the_Joint : constant physics.Joint.hinge.view := bullet_physics.Joint.new_hinge_Joint (Object_A, Object_B,
                                                                                             Frame_A,  Frame_B);
   begin
      return the_Joint;
   end new_hinge_Joint;


   overriding
   function new_DoF6_Joint (Self : access Item;   Object_A,
                                                  Object_B : in physics.Object.view;
                                                  Frame_A,
                                                  Frame_B  : in Matrix_4x4) return physics.Joint.DoF6.view
   is
      pragma Unreferenced (Self);
      the_Joint : constant physics.Joint.DoF6.view := bullet_physics.Joint.new_DoF6_Joint (Object_A, Object_B,
                                                                                           Frame_A,  Frame_B);
   begin
      return the_Joint;
   end new_DoF6_Joint;


   overriding
   function new_ball_Joint (Self : access Item;   Object_A,
                                                  Object_B   : in physics.Object.view;
                                                  Pivot_in_A,
                                                  Pivot_in_B : in Vector_3) return physics.Joint.ball.view
   is
      pragma unreferenced (Self);
      the_Joint : constant physics.Joint.ball.view := Standard.bullet_physics.Joint.new_ball_Joint (Object_A,    Object_B,
                                                                                                    Pivot_in_A,  Pivot_in_B);
   begin
      return the_Joint;
   end new_ball_Joint;


   overriding
   function new_slider_Joint (Self : access Item;   Object_A,
                                                    Object_B : in physics.Object.view;
                                                    Frame_A,
                                                    Frame_B  : in Matrix_4x4) return physics.Joint.slider.view
   is
      pragma unreferenced (Self);
      the_Joint : constant physics.Joint.slider.view := bullet_physics.Joint.new_slider_Joint (Object_A, Object_B,
                                                                                               Frame_A,  Frame_B);
   begin
      return the_Joint;
   end new_slider_Joint;


   overriding
   function new_cone_twist_Joint (Self : access Item;   Object_A,
                                                        Object_B : in physics.Object.view;
                                                        Frame_A,
                                                        Frame_B  : in Matrix_4x4) return physics.Joint.cone_twist.view
   is
      pragma unreferenced (Self);
      the_Joint : constant physics.Joint.cone_twist.view := bullet_physics.Joint.new_cone_twist_Joint (Object_A, Object_B,
                                                                                                       Frame_A,  Frame_B);
   begin
      return the_Joint;
   end new_cone_twist_Joint;


   ---------------
   --- Operations
   --

   overriding
   procedure update_Bounds (Self : in out Item;   of_Obect : in physics.Object.view)
   is
      the_c_Object : constant access bullet_c.Object := bullet_physics.Object.view (of_Obect).C;
      pragma Unreferenced (the_c_Object);
   begin
      raise Error with "TODO";
   end update_Bounds;


   overriding
   procedure add (Self : in out Item;   Object : in physics.Object.view)
   is
      the_c_Object : constant Object_Pointer := bullet_physics.Object.view (Object).C;
   begin
      b3d_Space_add_Object (Self.C, the_c_Object);
   end add;


   overriding
   procedure rid (Self : in out Item;   Object : in physics.Object.view)
   is
      the_c_Object : constant Object_Pointer := bullet_physics.Object.view (Object).C;
   begin
      b3d_Space_rid_Object (Self.C, the_c_Object);
   end rid;


   overriding
   function cast_Ray (Self : access Item;    From, To : in Vector_3) return physics.Space.ray_Collision
   is
      c_From : aliased c_math_c.Vector_3.item := +From;
      c_To   : aliased c_math_c.Vector_3.item := +To;

      the_Collision   :          physics.Space.ray_Collision;
      the_c_Collision : constant bullet_c.ray_Collision.item := b3d_Space_cast_Ray (Self.C, c_From'unchecked_Access,
                                                                                            c_To  'unchecked_Access);
   begin
      if the_c_Collision.near_Object /= null
      then
         the_Collision.near_Object := to_Object_view (b3d_Object_user_Data (the_c_Collision.near_Object));
      end if;

      the_Collision.hit_Fraction :=  Real (the_c_Collision.hit_Fraction);
      the_Collision.Normal_world := +the_c_Collision.Normal_world;
      the_Collision.Site_world   := +the_c_Collision.Site_world;

      return the_Collision;
   end cast_Ray;


   overriding
   procedure evolve (Self : in out Item;   By : in Duration)
   is
   begin
      bullet_c.Binding.b3d_Space_evolve (Self.C, C.C_float (By));

      -- Update each objects dynamics.
      --
      declare
         use c_Object_Maps_of_Object;
         Cursor     : c_Object_Maps_of_Object.Cursor := Self.object_Map.First;
         the_Object : bullet_Physics.Object.view;
      begin
         while has_Element (Cursor)
         loop
            the_Object := Element (Cursor);
            the_Object.update_Dynamics;

            next (Cursor);
         end loop;
      end;
   end evolve;


   overriding
   function Gravity (Self : in Item) return Vector_3
   is
   begin
      raise Error with "TODO";
      return (0.0, 0.0, 0.0);
   end Gravity;


   overriding
   procedure Gravity_is (Self : in out Item;   Now : in Vector_3)
   is
      c_Now : aliased c_math_c.Vector_3.item := +Now;
   begin
      bullet_c.Binding.b3d_Space_Gravity_is (Self.C, c_Now'unchecked_Access);
   end Gravity_is;


   overriding
   procedure add (Self : in out Item;   Joint : in physics.Joint.view)
   is
      the_c_Joint : constant Joint_Pointer := bullet_physics.Joint.view (Joint).C;
   begin
      b3d_Space_add_Joint (Self.C, the_c_Joint);
   end add;


   overriding
   procedure rid (Self : in out Item;   Joint : in physics.Joint.view)
   is
   begin
      raise Error with "TODO";
   end rid;


   overriding
   function manifold_Count (Self : in Item) return Natural
   is
   begin
      raise Error with "TODO";
      return 0;
   end manifold_Count;


   overriding
   function Manifold (Self : access Item;   Index : in Positive) return physics.space.a_Manifold
   is
      type Any_limited_view is access all lace.Any.limited_item'Class;
      pragma Unreferenced (Any_limited_view);
      the_Manifold : physics.space.a_Manifold;
   begin
      raise Error with "TODO";
      return the_Manifold;
   end Manifold;


   overriding
   procedure set_Joint_local_Anchor (Self : in out Item;   the_Joint    : in physics.Joint.view;
                                                           is_Anchor_A  : in Boolean;
                                                           local_Anchor : in Vector_3)
   is
   begin
      raise Error with "TODO";
   end set_Joint_local_Anchor;


   -----------------
   --- Joint Cursors
   --

   overriding
   procedure next (Cursor : in out joint_Cursor)
   is
   begin
      raise Error with "TODO";
   end next;


   overriding
   function has_Element (Cursor : in joint_Cursor) return Boolean
   is
   begin
      raise Error with "TODO";
      return False;
   end has_Element;


   overriding
   function Element (Cursor : in joint_Cursor) return physics.Joint.view
   is
   begin
      raise Error with "TODO";
      return null;
   end Element;


   overriding
   function first_Joint (Self : in Item) return physics.Space.joint_Cursor'Class
   is
   begin
      raise Error with "TODO";
      return joint_Cursor' (others => <>);
   end first_Joint;


end bullet_Physics.Space;
