with
     physics.Shape,
     box2d_c.Pointers,
     physics.Space;

package box2d_Physics.Shape
--
--  Provides glue between a physics shape and a Box2D shape.
--
is
   type Item is abstract new physics.Shape.item with     -- TODO: Make private.
      record
         C : box2d_c.Pointers.Shape_pointer;
      end record;

   type View is access all Item'Class;


   use Math;

   overriding
   procedure define   (Self : in out Item);

   overriding
   procedure destruct (Self : in out Item);

   overriding
   procedure Scale_is (Self : in out Item;   Now : Vector_3);


   ---------
   --  Forge
   --

   --  Shapes

   procedure free (the_Shape : in out physics.Shape.view);

   --  3D

   function new_box_Shape         (half_Extents : in Vector_3)    return physics.Shape.view;
   function new_capsule_Shape     (Radii        : in Vector_2;
                                   Height       : in Real)        return physics.Shape.view;
   function new_cone_Shape        (Radius,
                                   Height       : in Real)        return physics.Shape.view;
   function new_convex_hull_Shape (Points       : in physics.Vector_3_array)
                                                                  return physics.Shape.view;
   function new_cylinder_Shape    (half_Extents : in Vector_3)    return physics.Shape.view;
   function new_heightfield_Shape (Width,
                                   Depth        : in Positive;
                                   Heights      : access constant Real;
                                   min_Height,
                                   max_Height   : in Real;
                                   Scale        : in Vector_3)    return physics.Shape.view;
   function new_multiSphere_Shape (Positions    : in physics.Vector_3_array;
                                   Radii        : in Vector)      return physics.Shape.view;
   function new_plane_Shape       (Normal       : in Vector_3;
                                   Offset       : in Real)        return physics.Shape.view;
   function new_sphere_Shape      (Radius       : in Real)        return physics.Shape.view;


   --  2D

   function  new_circle_Shape (Radius   : in Real)                           return physics.Shape.view;
   function new_polygon_Shape (Vertices : in physics.Space.polygon_Vertices) return physics.Shape.view;



private

   type Circle is new Item with
      record
         Radius : Real;
      end record;

   overriding
   procedure define (Self : in out Circle);


   type Polygon (vertex_Count : Positive) is new Item with
      record
         Vertices : physics.Space.polygon_Vertices (1 .. vertex_Count);
      end record;

   overriding
   procedure define (Self : in out Polygon);


   type Box         is new Item with null record;
   type Capsule     is new Item with null record;
   type Cone        is new Item with null record;
   type Cylinder    is new Item with null record;
   type Heightfield is new Item with null record;
   type multiSphere is new Item with null record;
   type Plane       is new Item with null record;
   type Sphere      is new Item with null record;
   type convex_Hull is new Item with null record;

end box2d_Physics.Shape;
