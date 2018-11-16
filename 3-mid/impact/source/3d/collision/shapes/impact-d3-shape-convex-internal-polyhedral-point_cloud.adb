with impact.d3.collision.Proxy;
with impact.d3.Vector;
with impact.d3.Scalar;



package body impact.d3.Shape.convex.internal.polyhedral.point_cloud
is


   --- Forge
   --

   function to_point_cloud_Shape return Item
   is
      Self : Item;
   begin
      Self.set_m_localScaling ((1.0, 1.0, 1.0));
      Self.setShapeType  (impact.d3.collision.Proxy.CONVEX_POINT_CLOUD_SHAPE_PROXYTYPE);

      Self.m_numPoints      := 0;


      return Self;
   end to_point_cloud_Shape;





   function to_point_cloud_Shape (points       : access Vector_3_Array;
                                  numPoints    : in     Natural;
                                  localScaling : in     math.Vector_3;
                                  computeAabb  : in     Boolean      := True) return Item
   is
      Self : Item;
   begin
      Self.set_m_localScaling (localScaling);
      Self.setShapeType       (impact.d3.collision.Proxy.CONVEX_POINT_CLOUD_SHAPE_PROXYTYPE);

      Self.m_unscaledPoints := points;
      Self.m_numPoints      := numPoints;

      if computeAabb then
         Self.recalcLocalAabb;
      end if;


      return Self;
   end to_point_cloud_Shape;







   overriding function  getName   (Self : in     Item)        return String
   is
      pragma Unreferenced (Self);
   begin
      return "ConvexPointCloud";
   end getName;








   --  Currently just for debugging (drawing), perhaps future support for algebraic continuous collision detection
   --  Please note that you can debug-draw impact.d3.convex_HullShape with the Raytracer Demo
   --
   overriding function getNumVertices (Self : in Item) return Integer
   is
   begin
      return Self.m_numPoints;
   end getNumVertices;






   overriding function getNumEdges    (Self : in Item) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 0;
   end getNumEdges;






   overriding function getNumPlanes   (Self : in Item) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 0;
   end getNumPlanes;







   overriding procedure getVertex (Self : in Item;   i   : in     Integer;
                                          vtx :    out math.Vector_3)
   is
      use impact.d3.Vector;
   begin
      vtx := Scaled (Self.m_unscaledPoints (i),
                     by => Self.getLocalScaling);
   end getVertex;






   overriding procedure getEdge   (Self : in Item;   i      : in     Integer;
                                          pa, pb :    out math.Vector_3)
   is
      pragma Unreferenced (pa, pb, Self, i);
   begin
      pragma Assert (False);
      null;
   end getEdge;







   overriding procedure getPlane (Self : in Item;   planeNormal  :    out math.Vector_3;
                                         planeSupport :    out math.Vector_3;
                                         i            : in     Integer    )
   is
      pragma Unreferenced (planeNormal, planeSupport, Self, i);
   begin
      pragma Assert (False);
      null;
   end getPlane;






   --  not yet
   --
   overriding function isInside   (Self : in Item;   pt        : in math.Vector_3;
                                          tolerance : in math.Real) return Boolean
   is
      pragma Unreferenced (Self, pt, tolerance);
   begin
      pragma Assert (False);
      return False;
   end isInside;









   procedure setPoints (Self : in out Item;   points       : access Vector_3_Array;
                                              numPoints    : in     Natural;
                                              computeAabb  : in     Boolean       := True;
                        localScaling : in     math.Vector_3 := (1.0, 1.0, 1.0))
   is
   begin
      Self.m_unscaledPoints   := points;
      Self.m_numPoints        := numPoints;
      Self.set_m_localScaling (localScaling);

      if computeAabb then
         Self.recalcLocalAabb;
      end if;
   end setPoints;






   function getUnscaledPoints (Self : in Item) return access Vector_3_Array
   is
   begin
      return Self.m_unscaledPoints;
   end getUnscaledPoints;






   function getNumPoints   (Self : in Item) return Natural
   is
   begin
      return Self.m_numPoints;
   end getNumPoints;








   function getScaledPoint   (Self : in Item;   index : Positive) return math.Vector_3
   is
      use impact.d3.Vector;
   begin
      return Scaled (Self.m_unscaledPoints (index),
                     by => Self.getLocalScaling);
   end getScaledPoint;





   overriding function localGetSupportingVertex (Self : in Item;   vec : in math.Vector_3) return math.Vector_3
   is
      use impact.d3.Vector, math.Vectors;

      supVertex : math.Vector_3 := Self.localGetSupportingVertexWithoutMargin (vec);
      vecnorm   : math.Vector_3;

   begin
      if Self.getMargin /= 0.0 then
         vecnorm := vec;

         if length2 (vecnorm) < impact.d3.Scalar.SIMD_EPSILON * impact.d3.Scalar.SIMD_EPSILON then
            vecnorm := (-1.0, -1.0, -1.0);
         end if;

         normalize (vecnorm);

         supVertex := supVertex  +  Self.getMargin * vecnorm;
      end if;


      return supVertex;
   end localGetSupportingVertex;







   overriding function localGetSupportingVertexWithoutMargin (Self : in Item;   vec : in math.Vector_3) return math.Vector_3
   is
      use impact.d3.Vector, math.Functions, math.Vectors;

      supVec  : math.Vector_3 := math.Origin_3d;

      newDot,
      maxDot  : math.Real     := -BT_LARGE_FLOAT;

      the_vec : math.Vector_3 := vec;
      lenSqr  : constant math.Real     := length2 (the_vec);

      rlen    : math.Real;
      vtx     : math.Vector_3;

   begin

      if lenSqr < 0.0001 then
         the_vec  := (1.0, 0.0, 0.0);
      else
         rlen := 1.0 / sqRt (lenSqr);
         the_vec  := the_vec * rlen;
      end if;


      for i in 1 .. Self.m_numPoints
      loop
         vtx    := Self.getScaledPoint (i);
         newDot := dot (the_vec, vtx);

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
      newDot : math.Real;
      vtx    : math.Vector_3;

   begin
        -- use 'w' component of supportVerticesOut?
        --
      for i in 1 .. numVectors
      loop
         supportVerticesOut (i)(4) := -BT_LARGE_FLOAT;
      end loop;


      for i in 1 .. Self.m_numPoints
      loop
         vtx := Self.getScaledPoint (i);

         for j in 1 .. numVectors
         loop
            declare
               use impact.d3.Vector;
               vec : math.Vector_3 renames vectors (j);
            begin
               newDot := dot (vec, vtx);

               if newDot > supportVerticesOut (j)(4) then
                  supportVerticesOut (j)    := vtx;          -- WARNING: don't swap these lines, the w component would get overwritten!
                  supportVerticesOut (j)(4) := newDot;
               end if;
            end;
         end loop;

      end loop;

   end batchedUnitVectorGetSupportingVertexWithoutMargin;








   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3)
   is
   begin
      Self.set_m_localScaling (scaling);
      Self.recalcLocalAabb;
   end setLocalScaling;







end impact.d3.Shape.convex.internal.polyhedral.point_cloud;


















