with
     mmi.remote.physics_Model,
     physics.Shape;


package mmi.physics_Model
--
--  Provides a model describing physical properties.
--
is
   use Math;

   type shape_Kind is (Cylinder,  Cone,    Cube,  a_Sphere,  a_Capsule,  Heightfield,  Hull,  Mesh,  multi_Sphere, Plane,     -- 3D
                       Circle,    Polygon);                                                                                   -- 2D


   type a_Shape (Kind : shape_Kind := Cube) is
      record
         case Kind
         is
            when Cube | Cylinder =>
               half_Extents  :        Vector_3;

            when a_Capsule =>
               lower_Radius,
               upper_Radius  :        math.Real;
               Height        :        math.Real;

            when Heightfield =>
               Heights       : access physics.Heightfield;
               height_Range  :        Vector_2;

            when a_Sphere =>
               sphere_Radius :        math.Real;

            when Circle =>
               circle_Radius :        math.Real;

            when Hull =>
               Points        : access physics.Vector_3_array;

            when Mesh =>
               Model         : access math.Geometry.d3.a_Model;

            when multi_Sphere =>
               Sites         : access physics.Vector_3_array;
               Radii         : access math   .Vector;

            when Plane =>
               plane_Normal  :        Vector_3;
               plane_Offset  :        Real;

            when Polygon =>
               Vertices      :        Geometry_2d.Sites (1 .. 8);
               vertex_Count  :        Natural := 0;

            when others =>
               null;
         end case;
      end record;



   type Item is new mmi.remote.physics_Model.item with
      record
         shape_Info  : a_Shape;
         Shape       : physics.Shape.view;

         Mass        : Real;
         Friction    : Real;
         Restitution : Real;     -- Bounce

         is_Tangible : Boolean := True;
      end record;

   type View is access all Item'Class;




   ----------
   --- Forge
   --

   package Forge
   is
      function new_physics_Model (Id          : in mmi.physics_model_Id := mmi.null_physics_model_Id;
                                  shape_Info  : in a_Shape;
                                  Scale       : in math.Vector_3        := (1.0, 1.0, 1.0);
                                  Mass        : in math.Real            := 0.0;
                                  Friction    : in math.Real            := 0.1;
                                  Restitution : in math.Real            := 0.1;
                                  is_Tangible : in Boolean              := True) return mmi.physics_Model.view;
   end Forge;


   procedure define  (Self : in out Item;   Scale : in math.Vector_3);
   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);




   ---------------
   --- Attributes
   --

   function  Id       (Self : in     Item'Class)     return mmi.physics_model_Id;
   procedure Id_is    (Self : in out Item'Class;   Now : in mmi.physics_model_Id);


   procedure Scale_is (Self : in out Item'Class;   Now : in math.Vector_3);


end mmi.physics_Model;
