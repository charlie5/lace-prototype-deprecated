with impact.d3.collision.Proxy;
with impact.d3.Matrix;
with impact.d3.Vector;
with impact.d3.Transform;
with impact.d3.aabb_Util;


--  #include "impact.d3.Shape.concave.triangle_mesh.h"
--  #include "LinearMath/impact.d3.Vector.h"
--  #include "LinearMath/impact.d3.Quaternion.h"
--  #include "impact.d3.striding_Mesh.h"
--  #include "LinearMath/btAabbUtil2.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.collision.Margin.h"



package body impact.d3.Shape.concave.triangle_mesh
is



   function to_triangle_mesh_Shape (meshInterface : access impact.d3.striding_Mesh.Item'Class) return Item'Class
   is
      Self : Item := (impact.d3.Shape.concave.item with
                      m_meshInterface => meshInterface.all'Access,
                      others          => <>);
   begin
      Self.setShapeType (impact.d3.collision.Proxy.TRIANGLE_MESH_SHAPE_PROXYTYPE);

      if meshInterface.hasPremadeAabb then
         meshInterface.getPremadeAabb (Self.m_localAabbMin,  Self.m_localAabbMax);
      else
         Self.recalcLocalAabb;
      end if;


      return Self;
   end to_triangle_mesh_Shape;






   overriding procedure destruct (Self : in out Item)
   is
      pragma Unreferenced (Self);
   begin
      return;
   end destruct;










   overriding procedure getAabb (Self : in     Item;   t                : in     Transform_3d;
                      aabbMin, aabbMax :    out math.Vector_3)
   is
      use linear_Algebra_3d, impact.d3.Vector,  impact.d3.Matrix, impact.d3.Transform,
          math.Vectors;

      localHalfExtents : constant math.Vector_3   :=   0.5 * (Self.m_localAabbMax - Self.m_localAabbMin)
                                            + (Self.getMargin, Self.getMargin, Self.getMargin);

      localCenter      : constant math.Vector_3   := 0.5 * (Self.m_localAabbMax + Self.m_localAabbMin);

      abs_b            : constant math.Matrix_3x3 := absolute (getBasis (t));
      center           : constant math.Vector_3   := t * localCenter;

      extent           : constant math.Vector_3   := (dot (getRow (abs_b, 1),  localHalfExtents),
                                             dot (getRow (abs_b, 2),  localHalfExtents),
                                             dot (getRow (abs_b, 3),  localHalfExtents));
   begin
      aabbMin := center - extent;
      aabbMax := center + extent;
   end getAabb;







   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3)
   is
   begin
      Self.m_meshInterface.setScaling (scaling);
      Self.recalcLocalAabb;
   end setLocalScaling;





   overriding function  getLocalScaling (Self : in     Item)         return math.Vector_3
   is
   begin
      return Self.m_meshInterface.getScaling;
   end getLocalScaling;





   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3)
   is
      pragma Unreferenced (Self, mass);
   begin
      --  moving concave objects not supported
      pragma Assert (False);
      inertia := (0.0, 0.0, 0.0);
   end calculateLocalInertia;





   overriding function  getName   (Self : in     Item)        return String
   is
      pragma Unreferenced (Self);
   begin
      return "TRIANGLEMESH";
   end getName;







   overriding procedure processAllTriangles (Self : in     Item;   callback         : access impact.d3.triangle_Callback.Item'Class;
                                                        aabbMin, aabbMax : in     math.Vector_3)
   is

      type FilteredCallback is new impact.d3.triangle_Callback.btInternalTriangleIndexCallback with
         record
            m_callback : access impact.d3.triangle_Callback.Item'Class;
            m_aabbMin,
            m_aabbMax  :        math.Vector_3;
         end record;


      overriding procedure internalProcessTriangleIndex (Self : in out FilteredCallback;   triangle      : access math.Matrix_3x3;
                                                                                partId        : in     Integer;
                                                                                triangleIndex : in     Integer)
      is
      begin
         if impact.d3.aabb_Util.TestTriangleAgainstAabb2 (triangle.all,  Self.m_aabbMin, Self.m_aabbMax) then
            --  check aabb in triangle-space, before doing this
            Self.m_callback.processTriangle (triangle, partId, triangleIndex);
         end if;
      end internalProcessTriangleIndex;



      function to_FilteredCallback (callback         : access impact.d3.triangle_Callback.Item'Class;
                                    aabbMin, aabbMax : in     math.Vector_3              ) return FilteredCallback
      is
         Self : FilteredCallback;
      begin
         Self.m_callback := callback;
         Self.m_aabbMin  := aabbMin;
         Self.m_aabbMax  := aabbMax;

         return Self;
      end to_FilteredCallback;


      filterCallback : aliased FilteredCallback := to_FilteredCallback (callback,  aabbMin, aabbMax);

   begin
      Self.m_meshInterface.InternalProcessAllTriangles (filterCallback'Access,  aabbMin, aabbMax);
   end processAllTriangles;








   --- SupportVertexCallback
   --


   function to_SupportVertexCallback (supportVecWorld : in math.Vector_3;
                                      trans           : in Transform_3d) return SupportVertexCallback
   is
      use impact.d3.Transform, math.Vectors;
      Self : SupportVertexCallback;
   begin
      Self.m_supportVertexLocal := (0.0, 0.0, 0.0);
      Self.m_worldTrans         := trans;
      Self.m_maxDot             := -BT_LARGE_FLOAT;
      Self.m_supportVecLocal    := supportVecWorld * getBasis (Self.m_worldTrans);

      return Self;
   end to_SupportVertexCallback;






   overriding procedure processTriangle (Self : in out SupportVertexCallback;   triangle      : access math.Matrix_3x3;
                                                                     partId        : in     Integer;
                                                                     triangleIndex : in     Integer    )
   is
      pragma Unreferenced (partId, triangleIndex);
      use impact.d3.Vector, impact.d3.Matrix;
      the_dot : math.Real;

   begin
      for i in 1 .. 3
      loop
         the_dot := dot (Self.m_supportVecLocal,  getRow (triangle.all, i));

         if the_dot > Self.m_maxDot then
            Self.m_maxDot             := the_dot;
            Self.m_supportVertexLocal := getRow (triangle.all, i);
         end if;
      end loop;
   end processTriangle;




   function GetSupportVertexWorldSpace (Self : in SupportVertexCallback) return math.Vector_3
   is
      use linear_Algebra_3d, impact.d3.Transform;
   begin
      return Self.m_worldTrans * Self.m_supportVertexLocal;
   end GetSupportVertexWorldSpace;




   function GetSupportVertexLocal (Self : in SupportVertexCallback) return math.Vector_3
   is
      use impact.d3.Transform;
   begin
      return Self.m_supportVertexLocal;
   end GetSupportVertexLocal;








   function localGetSupportingVertex (Self : in     Item;   vec : in math.Vector_3) return math.Vector_3
   is
      use math.Vectors;

      supportVertex   : math.Vector_3;
      ident           : constant Transform_3d      := impact.d3.Transform.getIdentity;
      supportCallback : aliased SupportVertexCallback := to_SupportVertexCallback (vec, ident);
      aabbMax         : constant math.Vector_3         := (BT_LARGE_FLOAT, BT_LARGE_FLOAT, BT_LARGE_FLOAT);

   begin
      Self.processAllTriangles (supportCallback'Access,  -aabbMax, aabbMax);

      supportVertex := supportCallback.GetSupportVertexLocal;
      return supportVertex;
   end localGetSupportingVertex;






   procedure recalcLocalAabb (Self : in out Item)
   is
      vec,
      tmp : math.Vector_3;
   begin
      for i in 1 .. 3
      loop
         vec                     := (0.0, 0.0, 0.0);

         vec (i)                 := 1.0;
         tmp                     := Self.localGetSupportingVertex (vec);
         Self.m_localAabbMax (i) := tmp (i) + Self.getMargin;

         vec (i)                 := -1.0;
         tmp                     := Self.localGetSupportingVertex (vec);
         Self.m_localAabbMin (i) := tmp (i) - Self.getMargin;
      end loop;
   end recalcLocalAabb;







   function getMeshInterface (Self : in     Item) return access impact.d3.striding_Mesh.item'Class
   is
   begin
      return Self.m_meshInterface;
   end getMeshInterface;






   function getLocalAabbMin (Self : in     Item) return math.Vector_3
   is
   begin
      return Self.m_localAabbMin;
   end getLocalAabbMin;





   function getLocalAabbMax (Self : in     Item) return math.Vector_3
   is
   begin
      return Self.m_localAabbMax;
   end getLocalAabbMax;



end impact.d3.Shape.concave.triangle_mesh;
