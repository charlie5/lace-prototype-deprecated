with
     physics.remote.Model,
     physics.Shape;

package physics.Model
--
--  Provides a model describing physical properties.
--
is
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
               upper_Radius  :        Real;
               Height        :        Real;

            when Heightfield =>
               Heights       : access physics.Heightfield;
               height_Range  :        Vector_2;

            when a_Sphere =>
               sphere_Radius :        Real;

            when Circle =>
               circle_Radius :        Real;

            when Hull =>
               Points        : access physics.Vector_3_array;

            when Mesh =>
               Model         : access Geometry_3D.a_Model;

            when multi_Sphere =>
               Sites         : access physics.Vector_3_array;
               Radii         : access Vector;

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


   type Item is new physics.remote.Model.item with
      record
         shape_Info  : a_Shape;
         Shape       : physics.Shape.view;

         Mass        : Real;
         Friction    : Real;
         Restitution : Real;     -- Bounce
         --  Site        : Vector_3;

         is_Tangible : Boolean := True;
      end record;

   type View is access all Item'Class;


   ----------
   --- Forge
   --

   package Forge
   is
      function new_physics_Model (Id          : in model_Id := null_model_Id;
                                  shape_Info  : in a_Shape;
                                  Scale       : in Vector_3 := (1.0, 1.0, 1.0);
                                  Mass        : in Real     := 0.0;
                                  Friction    : in Real     := 0.1;
                                  Restitution : in Real     := 0.1;
                                  --  Site        : in Vector_3 := Origin_3d;
                                  is_Tangible : in Boolean  := True) return View;
   end Forge;

   procedure define  (Self : in out Item;   Scale : in Vector_3);
   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);


   ---------------
   --- Attributes
   --

   function  Id       (Self : in     Item'Class)     return model_Id;
   procedure Id_is    (Self : in out Item'Class;   Now : in model_Id);


   procedure Scale_is (Self : in out Item'Class;   Now : in Vector_3);


end physics.Model;
