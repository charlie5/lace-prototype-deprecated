with
     physics.Space;

private
with
     box2d_Physics.Object,
     box2d_c.joint_Cursor,
     box2d_c.Pointers,

     physics.Model,
     physics.Shape,
     physics.Object,
     physics.Joint.ball,
     physics.Joint.slider,
     physics.Joint.hinge,
     physics.Joint.cone_twist,
     physics.Joint.DoF6,

     ada.Containers.hashed_Maps;

package box2d_Physics.Space
--
-- Provide a Box2D implementation of a physical space.
--
is
   type Item is new physics.Space.item with private;
   type View is access all Item'Class;


   function to_Space return Item;

   overriding
   function manifold_Count (Self : in     Item) return Natural;

   overriding
   function Manifold       (Self : access Item;   Index : in Positive) return physics.space.a_Manifold;

   overriding
   function object_Count   (Self : in     Item) return Natural;



private

   function Hash (the_C_Object : in box2d_c.Pointers.Object_Pointer) return ada.Containers.Hash_type;
   use type box2d_c.Pointers.Object_pointer;
   use type box2d_Physics.Object.view;
   package c_Object_Maps_of_Object is new ada.Containers.hashed_Maps (Key_type        => box2d_c.Pointers.Object_Pointer,
                                                                      Element_type    => box2d_Physics.Object.view,
                                                                      Hash            => Hash,
                                                                      equivalent_Keys => "=",
                                                                      "="             => "=");
   type Item is new physics.Space.item with
      record
         C          : box2d_c.Pointers.Space_Pointer;
         object_Map : c_Object_Maps_of_Object.Map;
      end record;

   use Math;

   type joint_Cursor is new physics.Space.joint_Cursor with
      record
         C : aliased box2d_c.joint_Cursor.item;
      end record;

   overriding
   procedure next        (Cursor : in out joint_Cursor);
   overriding
   function  has_Element (Cursor : in     joint_Cursor) return Boolean;
   overriding
   function  Element     (Cursor : in     joint_Cursor) return physics.Joint.view;

   overriding
   function  first_Joint (Self   : in     Item)         return physics.Space.joint_Cursor'Class;


   ----------
   --- Forge
   --

   overriding
   procedure destruct (Self : in out Item);


   ---------
   --- Forge
   --

   -- Shapes

   overriding
   function new_Shape (Self : access Item;   Model : in physics.Model.view) return physics.Shape.view;


   -- 3D

   overriding
   function       new_sphere_Shape (Self : access Item;   Radius       : in Real := 0.5)                 return physics.Shape.view;
   overriding
   function          new_box_Shape (Self : access Item;   half_Extents : in Vector_3 := (0.5, 0.5, 0.5)) return physics.Shape.view;
   overriding
   function      new_capsule_Shape (Self : access Item;   Radius       : in Real :=  0.5;
                                                          Height       : in Real)                        return physics.Shape.view;
   overriding
   function         new_cone_Shape (Self : access Item;   Radius       : in Real :=  0.5;
                                                          Height       : in Real := 1.0)                 return physics.Shape.view;
   overriding
   function     new_cylinder_Shape (Self : access Item;   half_Extents : in Vector_3 := (0.5, 0.5, 0.5)) return physics.Shape.view;
   overriding
   function  new_heightfield_Shape (Self : access Item;   Heightfield  : in out physics.Heightfield;
                                                          Scale        : in     Vector_3)                return physics.Shape.view;
   overriding
   function  new_multisphere_Shape (Self : access Item;   Sites        : in physics.Vector_3_array;
                                                          Radii        : in math.Vector)                 return physics.Shape.view;
   overriding
   function        new_plane_Shape (Self : access Item;   Normal       : in Vector_3 := (0.0, 1.0, 0.0);
                                                          Offset       : in Real     :=  0.0)            return physics.Shape.view;
   overriding
   function  new_convex_hull_Shape (Self : access Item;   Points       : in physics.Vector_3_array)      return physics.Shape.view;
   overriding
   function         new_mesh_Shape (Self : access Item;   Points       : access Physics.Geometry_3D.a_Model) return physics.Shape .view;


   --  2D

   overriding
   function  new_circle_Shape  (Self : access Item;   Radius   : in Real := 0.5)                    return physics.Shape .view;
   overriding
   function  new_polygon_Shape (Self : access Item;   Vertices : in physics.Space.polygon_Vertices) return physics.Shape .view;


   --  Objects

   overriding
   function  new_Object (Self : access Item;   of_Shape     : in physics.Shape .view;
                                               of_Mass      : in Real;
                                               Friction     : in Real;
                                               Restitution  : in Real;
                                               at_Site      : in Vector_3;
                                               is_Kinematic : in Boolean) return physics.Object.view;
   --  Joints
   --
   overriding
   function new_hinge_Joint      (Self : access Item;   Object_A,
                                                        Object_B         : in physics.Object.view;
                                                        Anchor_in_A,
                                                        Anchor_in_B      : in Vector_3;
                                                        pivot_Axis       : in Vector_3;
                                                        low_Limit,
                                                        high_Limit       : in Real;
                                                        collide_Conected : in Boolean)    return physics.Joint.hinge.view;
   overriding
   function new_hinge_Joint      (Self : access Item;   Object_A,
                                                        Object_B         : in physics.Object.view;
                                                        Frame_A,
                                                        Frame_B          : in Matrix_4x4;
                                                        low_Limit,
                                                        high_Limit       : in Real;
                                                        collide_Conected : in Boolean)    return physics.Joint.hinge.view;
   overriding
   function new_hinge_Joint      (Self : access Item;   Object_A         : in physics.Object.view;
                                                        Frame_A          : in Matrix_4x4) return physics.Joint.hinge.view;
   overriding
   function new_DoF6_Joint       (Self : access Item;   Object_A,
                                                        Object_B         : in physics.Object.view;
                                                        Frame_A,
                                                        Frame_B          : in Matrix_4x4) return physics.Joint.DoF6.view;
   overriding
   function new_ball_Joint       (Self : access Item;   Object_A,
                                                        Object_B         : in physics.Object.view;
                                                        Pivot_in_A,
                                                        Pivot_in_B       : in Vector_3)   return physics.Joint.ball.view;
   overriding
   function new_slider_Joint     (Self : access Item;   Object_A,
                                                        Object_B         : in physics.Object.view;
                                                        Frame_A,
                                                        Frame_B          : in Matrix_4x4) return physics.Joint.slider.view;
   overriding
   function new_cone_twist_Joint (Self : access Item;   Object_A,
                                                        Object_B         : in physics.Object.view;
                                                        Frame_A,
                                                        Frame_B          : in Matrix_4x4) return physics.Joint.cone_twist.view;
   -------------
   -- Attributes
   --

   overriding
   function  Gravity    (Self : in     Item)     return Vector_3;
   overriding
   procedure Gravity_is (Self : in out Item;   Now : in Vector_3);


   ---------------
   --- Operations
   --

   overriding
   procedure evolve        (Self : in out Item;   By           : in Duration);
   overriding
   procedure add           (Self : in out Item;   the_Object   : in physics.Object.view);
   overriding
   procedure rid           (Self : in out Item;   the_Object   : in physics.Object.view);
   overriding
   function  cast_Ray      (Self : access Item;   From, To     : in Vector_3) return physics.Space.ray_Collision;
   overriding
   procedure add           (Self : in out Item;   the_Joint    : in physics.Joint.view);
   overriding
   procedure rid           (Self : in out Item;   the_Joint    : in physics.Joint.view);
   overriding
   procedure update_Bounds (Self : in out Item;   of_Obect     : in physics.Object.view);
   overriding
   procedure set_Joint_local_Anchor
                           (Self : in out Item;   the_Joint    : in physics.Joint.view;
                                                  is_Anchor_A  : in Boolean;
                                                  local_Anchor : in Vector_3);
end box2d_Physics.Space;
