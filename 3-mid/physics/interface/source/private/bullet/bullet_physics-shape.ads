with
     physics.Shape,

     c_math_c.Pointers,

     bullet_c.Pointers,
     bullet_c;

package bullet_Physics.Shape
--
--  Provides glue between a physics shape and a Bullet3D shape.
--
is
   type Item is abstract new physics.Shape.item with
      record
         C : bullet_c.Pointers.Shape_Pointer;
      end record;

   type View is access all Item'Class;


   use Math;


   ---------
   --  Forge
   --

   overriding
   procedure define   (Self : in out Item);
   overriding
   procedure destruct (Self : in out Item);


   function new_box_Shape         (half_Extents : in     Vector_3) return physics.Shape.view;
   function new_capsule_Shape     (Radii        : in     Vector_2;
                                   Height       : in     Real)     return physics.Shape.view;
   function new_cone_Shape        (Radius,
                                   Height       : in     Real)     return physics.Shape.view;
   function new_convex_hull_Shape (Points       : in     physics.Vector_3_array)
                                                                   return physics.Shape.view;
   function new_mesh_Shape        (Model        : access Geometry.d3.a_Model)
                                                                   return physics.Shape.view;
   function new_cylinder_Shape    (half_Extents : in     Vector_3) return physics.Shape.view;
   function new_heightfield_Shape (Width,
                                   Depth        : in     Positive;
                                   Heights      : in     c_math_c.Pointers.Real_pointer;
                                   min_Height,
                                   max_Height   : in     Real;
                                   Scale        : in     Vector_3) return physics.Shape.view;
   function new_multiSphere_Shape (Positions    : in     physics.Vector_3_array;
                                   Radii        : in     Vector)   return physics.Shape.view;
   function new_plane_Shape       (Normal       : in     Vector_3;
                                   Offset       : in     Real)     return physics.Shape.view;
   function new_sphere_Shape      (Radius       : in     Real)     return physics.Shape.view;

   procedure free (the_Shape : in out physics.Shape.view);


   ---------------
   ---  Attributes
   --

   overriding
   procedure Scale_is (Self : in out Item;   Now : Vector_3);



private

   type Box         is new Item with null record;
   type Capsule     is new Item with null record;
   type Cone        is new Item with null record;
   type Cylinder    is new Item with null record;
   type Heightfield is new Item with null record;
   type multiSphere is new Item with null record;
   type Plane       is new Item with null record;
   type Sphere      is new Item with null record;
   type convex_Hull is new Item with null record;
   type Mesh        is new Item with null record;

end bullet_Physics.Shape;
