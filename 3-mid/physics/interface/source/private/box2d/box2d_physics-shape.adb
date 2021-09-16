with
     box2d_c.Binding,

     c_math_c.Vector_2,
     c_math_c.Conversion,

     ada.unchecked_Deallocation,
     ada.unchecked_Conversion;

package body box2d_Physics.Shape
is
   use c_math_c.Conversion,
       box2d_c .Binding;


   --  Base Shape
   --

   overriding
   procedure define (Self : in out Item)
   is
   begin
      raise Error with "Shape not supported.";
   end define;


   overriding
   procedure destruct (Self : in out Item)
   is
   begin
      b2d_free_Shape (Self.C);
   end destruct;


   overriding
   procedure Scale_is (Self : in out Item;   Now : Vector_3)
   is
   begin
      b2d_shape_Scale_is (Self.C, (c_math_c.Real (Now (1)),
                                   c_math_c.Real (Now (2))));
   end Scale_is;


   -----------
   --  Forge
   --

   --  2D
   --

   type Circle_view is access Circle;

   function new_circle_Shape (Radius : in Real) return physics.Shape.view
   is
      Self : constant Circle_view := new Circle;
      --  Self : constant access Circle := new Circle;
--        c_Radius : aliased constant c_math_c.Real := +Radius;
   begin
      --        Self.C := b2d_new_Circle (c_Radius);
      Self.Radius := Radius;
      Self.define;
      return physics.Shape.view (Self);
   end new_circle_Shape;


   overriding
   procedure define (Self : in out Circle)
   is
      c_Radius : aliased constant c_math_c.Real := +Self.Radius;
   begin
      Self.C := b2d_new_Circle (c_Radius);
   end define;



   type Polygon_view is access Polygon;

   function new_polygon_Shape (Vertices : in physics.Space.polygon_Vertices) return physics.Shape.view
   is
      --  P : Polygon (vertex_Count => Vertices'Length);
      --  Self : constant Polygon_view := new Polygon' (P);
      Self : constant Polygon_view := new Polygon (vertex_Count => Vertices'Length);
--        c_Verts : array (1 .. Vertices'Length) of aliased c_math_c.Vector_2.item;
   begin
         Self.Vertices := Vertices;
--        for i in c_Verts'Range
--        loop
--           c_Verts (i) := +Vertices (i);
--        end loop;
--
--        Self.C := b2d_new_Polygon (c_Verts (1)'Unchecked_Access,
--                                   c_Verts'Length);
      Self.define;
      return physics.Shape.view (Self);
   end new_polygon_Shape;


   overriding
   procedure define (Self : in out Polygon)
   is
      c_Verts : array (1 .. Self.vertex_Count) of aliased c_math_c.Vector_2.item;
   begin
      for i in c_Verts'Range
      loop
         c_Verts (i) := +Self.Vertices (i);
      end loop;

      Self.C := b2d_new_Polygon (c_Verts (1)'unchecked_Access,
                                 c_Verts'Length);
   end define;


   -- 3D
   --

   function new_box_Shape (half_Extents : in Vector_3) return physics.Shape.view
   is
      pragma unreferenced (half_Extents);
   begin
      raise physics.unsupported_Error;
      return null;
   end new_box_Shape;


   function new_capsule_Shape (Radii  : in Vector_2;
                               Height : in Real) return physics.Shape.view
   is
   begin
      raise physics.unsupported_Error;
      return null;
   end new_capsule_Shape;


   function new_cone_Shape (Radius,
                            Height : in Real) return physics.Shape.view
   is
   begin
      raise physics.unsupported_Error;
      return null;
   end new_cone_Shape;


   function new_convex_hull_Shape (Points : in physics.Vector_3_array) return physics.Shape.view
   is
   begin
      raise physics.unsupported_Error;
      return null;
   end new_convex_hull_Shape;


   function new_cylinder_Shape (half_Extents : in Vector_3) return physics.Shape.view
   is
   begin
      raise physics.unsupported_Error;
      return null;
   end new_cylinder_Shape;


   function new_heightfield_Shape (Width,
                                   Depth       : in Positive;
                                   Heights     : access constant Real;
                                   min_Height,
                                   max_Height  : in Real;
                                   Scale       : in Vector_3) return physics.Shape.view
   is
   begin
      raise physics.unsupported_Error;
      return null;
   end new_heightfield_Shape;


   function new_multiSphere_Shape (Positions : in physics.Vector_3_array;
                                   Radii     : in Vector) return physics.Shape.view
   is
   begin
      raise physics.unsupported_Error;
      return null;
   end new_multiSphere_Shape;


   function new_plane_Shape (Normal : in Vector_3;
                             Offset : in Real) return physics.Shape.view
   is
   begin
      raise physics.unsupported_Error;
      return null;
   end new_plane_Shape;


   function new_sphere_Shape (Radius : in math.Real) return physics.Shape.view
   is
   begin
      raise physics.unsupported_Error;
      return null;
   end new_sphere_Shape;


   procedure free (the_Shape : in out physics.Shape.view)
   is
      procedure deallocate is new ada.unchecked_Deallocation (physics.Shape.item'Class,
                                                              physics.Shape.view);
   begin
      the_Shape.destruct;
      deallocate (the_Shape);
   end free;


end box2d_Physics.Shape;
