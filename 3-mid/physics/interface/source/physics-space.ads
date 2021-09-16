with
     physics.Model,
     physics.Shape,
     physics.Object,
     physics.Joint.DoF6,
     physics.Joint.hinge,
     physics.Joint.cone_twist,
     physics.Joint.slider,
     physics.Joint.ball;

package physics.Space
--
-- Models a static/dynamic physics space.
--
is
   type Item is limited interface;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   procedure free     (Self : in out View);
   procedure destruct (Self : in out Item)  is abstract;


   --------------
   --- Attributes
   --

   procedure add    (Self : in out Item;   the_Object : in Object.view)   is abstract;
   procedure rid    (Self : in out Item;   the_Object : in Object.view)   is abstract;
   procedure evolve (Self : in out Item;   By         : in Duration)      is abstract;


   type Real_view is access all Real;


   --- Contacts
   --

   type a_Contact is
      record
         Site : Vector_3;
      end record;

   type Contacts is array (Positive range 1 .. 4) of a_Contact;


   --- Manifolds
   --

   type a_Manifold is
      record
         Objects       : Object.views (1 .. 2);
         Contact       : a_Contact;
      end record;

   function manifold_Count (Self : in     Item) return Natural                             is abstract;
   function Manifold       (Self : access Item;   Index : in Positive) return a_Manifold   is abstract;


   --- Ray Casting
   --

   type ray_Collision is
      record
         near_Object  : Object.view;
         hit_Fraction : Real;
         Normal_world : Vector_3;
         Site_world   : Vector_3;
      end record;

   function  cast_Ray (Self : access Item;    From, To : in Vector_3) return ray_Collision   is abstract;


   --- Bounds
   --

   procedure update_Bounds (Self : in out Item;   of_Obect : in Object.view)   is abstract;


   -----------
   --- Factory
   --

   unsupported_Shape : exception;


   --- Physical Objects
   --

   function new_Object   (Self : access Item;   of_Shape      : in Shape.view;
                                                 of_Mass      : in Real;
                                                 Friction     : in Real;
                                                 Restitution  : in Real;
                                                 at_Site      : in Vector_3;
                                                 is_Kinematic : in Boolean) return Object.view   is abstract;

   function object_Count (Self : in     Item) return Natural   is abstract;


   --- 3D
   --

   --  Shapes
   --

   function              new_Shape (Self : access Item;   from_Model : in Model.view)                        return Shape.view   is abstract;

   function          new_box_Shape (Self : access Item;   half_Extents : in     Vector_3 := (0.5, 0.5, 0.5)) return Shape.view   is abstract;
   function       new_sphere_Shape (Self : access Item;   Radius       : in     Real     :=  0.5)            return Shape.view   is abstract;
   function      new_capsule_Shape (Self : access Item;   Radius       : in     Real     :=  0.5;
                                                          Height       : in     Real)                        return Shape.view   is abstract;
   function         new_cone_Shape (Self : access Item;   Radius       : in     Real     :=  0.5;
                                                          Height       : in     Real     :=  1.0)            return Shape.view   is abstract;
   function     new_cylinder_Shape (Self : access Item;   half_Extents : in     Vector_3 := (0.5, 0.5, 0.5)) return Shape.view   is abstract;
   function  new_heightfield_Shape (Self : access Item;   Heightfield  : in out physics.Heightfield;
                                                          Scale        : in     Vector_3)                    return Shape.view   is abstract;
   function  new_multisphere_Shape (Self : access Item;   Sites        : in     Vector_3_array;
                                                          Radii        : in     Vector)                      return Shape.view   is abstract;
   function        new_plane_Shape (Self : access Item;   Normal       : in     Vector_3 := (0.0, 1.0, 0.0);
                                                          Offset       : in     Real     :=  0.0)            return Shape.view   is abstract;
   function  new_convex_hull_Shape (Self : access Item;   Points       : in     Vector_3_array)              return Shape.view   is abstract;
   function         new_mesh_Shape (Self : access Item;   Points       : access Geometry_3D.a_Model)         return Shape.view   is abstract;


   --  Joints
   --

   function new_hinge_Joint      (Self : access Item;   Object_A,
                                                        Object_B     : in Object.view;
                                                        Anchor_in_A,
                                                        Anchor_in_B  : in Vector_3;
                                                        pivot_Axis   : in Vector_3;
                                                        low_Limit,
                                                        high_Limit        : in Real;
                                                        collide_Connected : in Boolean) return Joint.hinge.view is abstract;

   function new_hinge_Joint      (Self : access Item;   Object_A,
                                                        Object_B     : in Object.view;
                                                        Frame_A,
                                                        Frame_B      : in Matrix_4x4;
                                                        low_Limit,
                                                        high_Limit   : in Real;
                                                        collide_Connected : in Boolean) return Joint.hinge.view is abstract;

   function new_hinge_Joint      (Self : access Item;   Object_A     : in Object.view;
                                                        Frame_A      : in Matrix_4x4)   return Joint.hinge.view is abstract;

   function new_DoF6_Joint       (Self : access Item;   Object_A,
                                                        Object_B     : in Object.view;
                                                        Frame_A,
                                                        Frame_B      : in Matrix_4x4)   return Joint.DoF6.view  is abstract;

   function new_ball_Joint       (Self : access Item;   Object_A,
                                                        Object_B     : in Object.view;
                                                        Pivot_in_A,
                                                        Pivot_in_B   : in Vector_3)     return Joint.ball.view  is abstract;

   function new_slider_Joint     (Self : access Item;   Object_A,
                                                        Object_B     : in Object.view;
                                                        Frame_A,
                                                        Frame_B      : in Matrix_4x4)  return Joint.slider.view is abstract;

   function new_cone_twist_Joint (Self : access Item;   Object_A,
                                                        Object_B     : in Object.view;
                                                        Frame_A,
                                                        Frame_B      : in Matrix_4x4)  return Joint.cone_twist.view is abstract;
   type joint_Cursor is interface;

   procedure next        (Cursor : in out joint_Cursor)                             is abstract;
   function  has_Element (Cursor : in     joint_Cursor) return Boolean              is abstract;
   function  Element     (Cursor : in     joint_Cursor) return Joint.view           is abstract;

   function  first_Joint (Self   : in     Item)         return joint_Cursor'Class   is abstract;


   --- 2D
   --

   --- Shapes
   --

   type polygon_Vertices is array (Positive range <>) of aliased Vector_2;

   function new_circle_Shape  (Self : access Item;   Radius   : in Real := 0.5)      return Shape.view is abstract;
   function new_polygon_Shape (Self : access Item;   Vertices : in polygon_Vertices) return Shape.view is abstract;


   ------------
   --  Dynamics
   --

   function  Gravity    (Self : in     Item)     return Vector_3             is abstract;
   procedure Gravity_is (Self : in out Item;   Now : in Vector_3)            is abstract;

   procedure add        (Self : in out Item;   the_Joint    : in Joint.view) is abstract;
   procedure rid        (Self : in out Item;   the_Joint    : in Joint.view) is abstract;

   procedure set_Joint_local_Anchor
                        (Self : in out Item;   the_Joint    : in Joint.view;
                                               is_Anchor_A  : in Boolean;
                                               local_Anchor : in Vector_3)   is abstract;

end physics.Space;
