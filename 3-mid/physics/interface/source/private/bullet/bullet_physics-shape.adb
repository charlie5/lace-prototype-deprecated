with
     bullet_c.Binding,

     c_math_c.Vector_2,
     c_math_c.Vector_3,
     c_math_c.Conversion,
     c_math_c.Pointers,
     c_math_c.Triangle,

     physics.Forge,

     ada.unchecked_Deallocation,
     ada.Unchecked_Conversion,

     interfaces.C;


package body bullet_Physics.Shape
is

   use c_math_c.Conversion,
       bullet_c.Binding;



   --  Base Shape
   --


   overriding
   procedure define (Self : in out Item)
   is
   begin
      raise Program_Error with "Bullet Shape not supported";
   end define;



   overriding
   procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;


   overriding
   procedure Scale_is (Self : in out Item;   Now : math.Vector_3)
   is
   begin
      null;
   end Scale_is;



   --- Box
   --
   function new_box_Shape (half_Extents : in Vector_3) return physics.Shape.view
   is
      Self           : constant access Box                    := new Box;
      c_half_Extents : aliased         c_math_c.Vector_3.item := +half_Extents;
   begin
      Self.C := b3d_new_Box (c_half_Extents'Unchecked_Access);
      return Self;
   end new_box_Shape;



   --- Capsule
   --
   function new_capsule_Shape (Radii  : in Vector_2;
                               Height : in Real) return physics.Shape.view
   is
      Self    : constant access Capsule                := new Capsule;
      c_Radii : aliased         c_math_c.Vector_2.item := +Radii;
   begin
      Self.C := b3d_new_Capsule (c_Radii'Unchecked_Access, +Height);
      return Self;
   end new_capsule_Shape;



   --- Cone
   --
   function new_cone_Shape (Radius,
                            Height : in Real) return physics.Shape.view
   is
      Self : constant access Cone := new Cone;
   begin
      Self.C := b3d_new_Cone (+Radius, +Height);
      return Self;
   end new_cone_Shape;



   --- convex_Hull
   --
   function new_convex_hull_Shape (Points : in physics.Vector_3_array) return physics.Shape.view
   is
      Self     : constant access convex_Hull := new convex_Hull;
      c_Points : array (1 .. Points'Length) of aliased c_math_c.Vector_3.item;
   begin
      for i in c_Points'Range
      loop
         c_Points (i) := +Points (i);
      end loop;

      Self.C := b3d_new_convex_Hull (c_Points (1)'Unchecked_Access,
                                     c_Points'Length);
      return Self;
   end new_convex_hull_Shape;



   --- Mesh
   --
   function new_mesh_Shape (Model : access math.Geometry.d3.a_Model) return physics.Shape.view
   is
      use Interfaces;

      Self        : constant access Mesh := new Mesh;
      c_Points    : array (1 .. Model.site_Count) of aliased c_math_c.Vector_3.item;

      type Triangles is array (1 .. Model. tri_Count) of aliased c_math_c.Triangle.item;
      pragma Pack (Triangles);

      c_Triangles : Triangles;

   begin
      for i in c_Points'Range
      loop
         c_Points (i) := +Model.Sites (i);
      end loop;

      for i in c_Triangles'Range
      loop
         c_Triangles (i) := (a => C.int (Model.Triangles (i)(1)),
                             b => C.int (Model.Triangles (i)(2)),
                             c => C.int (Model.Triangles (i)(3)));
      end loop;

      Self.C := b3d_new_Mesh (Points         => c_Points (c_Points'First)'unchecked_Access,
                              point_Count    => 0,
                              Triangles      => c_Triangles (c_Triangles'First)'unchecked_Access,
                              triangle_Count => Interfaces.C.int (Model.tri_Count));
      return Self;
   end new_mesh_Shape;




   --- Cylinder
   --
   function new_cylinder_Shape (half_Extents : in Vector_3) return physics.Shape.view
   is
      Self           : constant access Cylinder               := new Cylinder;
      c_half_Extents : aliased         c_math_c.Vector_3.item := +half_Extents;
   begin
      Self.C := b3d_new_Cylinder (c_half_Extents'Unchecked_Access);
      return Self;
   end new_cylinder_Shape;



   --- Heightfield
   --
   function new_heightfield_Shape (Width,
                                   Depth        : in              Positive;
                                   Heights      : access constant Real;
                                   min_Height,
                                   max_Height   : in              Real;
                                   Scale        : in              Vector_3)    return physics.Shape.view
   is
      function to_real_Pointer is new ada.unchecked_Conversion (physics.Forge.Real_view,
                                                                c_math_c.pointers.real_Pointer);
      Self    : constant access Heightfield            := new Heightfield;
      c_Scale : aliased         c_math_c.Vector_3.item := +Scale;
   begin
      Self.C := b3d_new_Heightfield (+Width,
                                     +Depth,
                                     to_real_Pointer (Heights),
                                     c_math_c.Real (min_Height),
                                     c_math_c.Real (max_Height),
                                     c_Scale'Unchecked_Access);
      return Self;
   end new_heightfield_Shape;



   --- multiSphere
   --
   function new_multiSphere_Shape (Positions    : in physics.Vector_3_array;
                                   Radii        : in math.Vector) return physics.Shape.view
   is
      pragma Assert (Positions'Length = Radii'Length);

      Self        : constant access multiSphere := new multiSphere;

      c_Positions : array (1 .. Positions'Length) of aliased c_math_c.Vector_3.item;
      c_Radii     : array (1 .. Radii    'Length) of aliased c_math_c.Real;
   begin
      for i in c_Radii'Range
      loop
         c_Positions (i) := +Positions (i);
         c_Radii     (i) := +Radii     (i);
      end loop;

      Self.C := b3d_new_multiSphere (c_Positions (1)'Unchecked_Access,
                                     c_Radii     (1)'Unchecked_Access,
                                     Radii'Length);
      return Self;
   end new_multiSphere_Shape;



   --- Plane
   --
   function new_plane_Shape (Normal : in Vector_3;
                             Offset : in Real) return physics.Shape.view
   is
      Self : constant access Plane := new Plane;
      Pad : aliased c_math_c.Vector_3.item := +Normal;
   begin
      Self.C := b3d_new_Plane (Pad'Unchecked_Access, +Offset);
      return Self;
   end new_plane_Shape;



   --- Sphere
   --
   function new_sphere_Shape (Radius : in math.Real) return physics.Shape.view
   is
      Self : constant access Sphere := new Sphere;
   begin
      Self.C := b3d_new_Sphere (+Radius);
      return Self;
   end new_sphere_Shape;




   --- Free
   --

   procedure free (the_Shape : in out physics.Shape.view)
   is
      procedure deallocate is new ada.unchecked_Deallocation (physics.Shape.item'Class,
                                                              physics.Shape.view);
   begin
      the_Shape.destruct;
      deallocate (the_Shape);
   end free;


end bullet_Physics.Shape;
