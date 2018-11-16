with impact.d3.Vector;
with impact.d3.collision.Proxy;
with Interfaces.C;
with ada.unchecked_Deallocation;
with impact.d3.Scalar;
with impact.d3.Containers;
--  #include "impact.d3.convex_HullShape.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.collision.Margin.h"
--
--  #include "LinearMath/impact.d3.Quaternion.h"
--  #include "LinearMath/btSerializer.h"


package body impact.d3.Shape.convex.internal.polyhedral.hull
--
--
--
is


   ----------
   --- Forge
   --

   function to_hull_Shape (points    : access math.Real := null;
                           numPoints : in     Natural   := 0;
                           stride    : in     Integer   := math.Vector_3'Size / 8) return Item
   is
      Self : Item;

      pointsAddress : impact.d3.Containers.real_Pointer;

   begin
      Self.setShapeType (impact.d3.collision.Proxy.CONVEX_HULL_SHAPE_PROXYTYPE);

      Self.m_unscaledPoints := new c_Vector_3_array (1 .. numPoints);
      Self.m_point_Count    := numPoints;


      pointsAddress := points;

      for i in 1 .. numPoints
      loop
         declare
            use impact.d3.Containers.real_Pointers;
            point : impact.d3.Containers.real_Pointer  := pointsAddress;
         begin
            Self.m_unscaledPoints (i)(0) := point.all;   increment (Point);
            Self.m_unscaledPoints (i)(1) := point.all;   increment (Point);
            Self.m_unscaledPoints (i)(2) := point.all;

            pointsAddress                := pointsAddress + Interfaces.C.ptrdiff_t (stride);
         end;
      end loop;

      Self.recalcLocalAabb;


      return Self;
   end to_hull_Shape;






   ---------------
   --- Attributes
   --

   overriding function  getName        (Self : in     Item) return String
   is
      pragma Unreferenced (Self);
   begin
      return "Convex";
   end getName;






   --  Currently just for debugging (drawing), perhaps future support for algebraic continuous collision detection
   --
   --  Please note that you can debug-draw impact.d3.convex_HullShape with the Raytracer Demo
   --

   overriding function  getNumVertices (Self : in Item) return Integer
   is
   begin
      return Self.m_point_Count;
   end getNumVertices;




   overriding function  getNumEdges    (Self : in Item) return Integer
   is
   begin
      return Self.m_point_Count;
   end getNumEdges;




   overriding function  getNumPlanes   (Self : in Item) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 0;
   end getNumPlanes;




   overriding procedure getVertex (Self : in Item;   i   : in     Integer;
                                          vtx :    out math.Vector_3)
   is
   begin
      vtx := Self.getScaledPoint (i);
   end getVertex;





   overriding procedure getEdge   (Self : in Item;   i      : in     Integer;
                                          pa, pb :    out math.Vector_3)
   is
      index0 : constant Integer :=  i    mod Self.m_point_Count;
      index1 : constant Integer := (i + 1) mod Self.m_point_Count;
   begin
      pa := Self.getScaledPoint (index0);
      pb := Self.getScaledPoint (index1);
   end getEdge;





   overriding procedure getPlane (Self : in Item;   planeNormal  :    out math.Vector_3;
                                         planeSupport :    out math.Vector_3;
                                         i            : in     Integer)
   is
   begin
      raise Program_Error;
   end getPlane;






   --  not yet
   --
   overriding function  isInside   (Self : in Item;   pt        : in math.Vector_3;
                                           tolerance : in math.Real) return Boolean
   is
   begin
      raise Program_Error;
      return False;
   end isInside;








   procedure addPoint (Self : in out Item;   point : in math.Vector_3)
   is
   begin
      Self.m_point_Count := Self.m_point_Count + 1;

      if Self.m_point_Count > Self.m_unscaledPoints'Length then
         declare
            procedure free is new ada.unchecked_Deallocation (c_Vector_3_array,  access_c_Vector_3_array);
            new_Points : constant access_c_Vector_3_array := new c_Vector_3_array (1 .. 2 * Self.m_unscaledPoints'Length);
         begin
            new_Points (Self.m_unscaledPoints'Range) := Self.m_unscaledPoints.all;
            free (Self.m_unscaledPoints);
            Self.m_unscaledPoints := new_Points;
         end;
      end if;


      Self.m_unscaledPoints (Self.m_point_Count) := to_C (point);
      Self.recalcLocalAabb;
   end addPoint;






   function  getUnscaledPoints (Self : in     Item                    ) return access c_Vector_3
   is
   begin
      return Self.m_unscaledPoints (1)'Access;
   end getUnscaledPoints;





   function  getScaledPoint    (Self : in Item;   i : in Integer) return math.Vector_3
   is
      use impact.d3.Vector;
   begin
      return scaled (to_Math (Self.m_unscaledPoints (i)),
                     by => Self.getLocalScaling);
   end getScaledPoint;





   function  getNumPoints (Self : in Item) return Natural
   is
   begin
      return Self.m_point_Count;
   end getNumPoints;






   overriding function localGetSupportingVertex (Self : in Item;   vec : in Math.Vector_3) return Math.Vector_3
   is
      use impact.d3.Vector, math.Vectors;

      supVertex : math.Vector_3 := Self.localGetSupportingVertexWithoutMargin (vec);
      vecnorm   : math.Vector_3;

   begin
      if Self.getMargin /= 0.0 then
         vecnorm := vec;

         if length2 (vecnorm) < impact.d3.Scalar.SIMD_EPSILON * impact.d3.Scalar.SIMD_EPSILON then
            vecnorm := (-1.0,  -1.0,  -1.0);
         end if;

         normalize (vecnorm);
         supVertex := supVertex  +  Self.getMargin * vecnorm;
      end if;


      return supVertex;
   end localGetSupportingVertex;








   overriding function localGetSupportingVertexWithoutMargin        (Self : in Item;   vec : in Math.Vector_3) return Math.Vector_3
   is
      use impact.d3.Vector, Math.Vectors;

      supVec : math.Vector_3 := math.Origin_3d;
      newDot,
      maxDot : math.Real     := -BT_LARGE_FLOAT;
      vtx    : math.Vector_3;

   begin
      for  i in 1 .. Integer (Self.m_point_Count)
      loop
         vtx    := Scaled (to_Math (Self.m_unscaledPoints (i)),
                           by => Self.getLocalScaling);

         newDot := dot (vec, vtx);


         if newDot > maxDot then
            maxDot := newDot;
            supVec := vtx;
         end if;

      end loop;


      return supVec;
   end localGetSupportingVertexWithoutMargin;







   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                numVectors         : in     Integer)
   is
      use impact.d3.Vector;

      newDot : math.Real;

      vec,
      vtx    : math.Vector_3;

   begin
      --  Use 'w' component of supportVerticesOut ?

      for i in 1 .. numVectors
      loop
         supportVerticesOut (i) (4) := -BT_LARGE_FLOAT;
      end loop;


      for i in 1 .. Self.m_point_Count
      loop
         vtx := Self.getScaledPoint (i);

         for j in 1 .. numVectors
         loop
            vec    := vectors (j);
            newDot := dot (vec, vtx);

            if newDot > supportVerticesOut (j)(4) then
               --  WARNING: don't swap next lines, the w component would get overwritten!
               --
               supportVerticesOut (j)    := vtx;
               supportVerticesOut (j)(4) := newDot;
            end if;
         end loop;

      end loop;

   end batchedUnitVectorGetSupportingVertexWithoutMargin;







   procedure project (Self : in Item'Class;   t        : in     Transform_3d;
                                              dir      : in     Math.Vector_3;
                      min, max :    out Real)
   is
      use linear_Algebra_3d,
          impact.d3.Vector;

      numVerts    : constant Integer := Self.m_point_Count;

      witnesPtMin,
      witnesPtMax : math.Vector_3;
      pragma Unreferenced (witnesPtMin, witnesPtMax);

      vtx, pt     : math.Vector_3;
      dp, tmp     : math.Real;

   begin
      min :=  math.Real'Last;
      max :=  math.Real'First;


      for i in 1 .. numVerts
      loop
         vtx := Scaled (to_Math (Self.m_unscaledPoints (i)),
                        by => Self.getLocalScaling);

         pt  := t * vtx;
         dp  := dot (pt, dir);

         if dp < min then
            min         := dp;
            witnesPtMin := pt;
         end if;

         if dp > max then
            max         := dp;
            witnesPtMax := pt;
         end if;
      end loop;


      if min > max then
         tmp := min;
         min := max;
         max := tmp;
      end if;

   end project;







   overriding procedure setLocalScaling (Self : in out Item;   scaling : in Math.Vector_3)
   is
   begin
      Self.set_m_localScaling (scaling);
      Self.recalcLocalAabb;
   end setLocalScaling;



end impact.d3.Shape.convex.internal.polyhedral.hull;
