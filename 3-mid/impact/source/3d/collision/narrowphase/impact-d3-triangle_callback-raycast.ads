with impact.d3.triangle_Callback;
with impact.d3.Shape.convex;



package impact.d3.triangle_Callback.raycast
--
--
--
is
   --  @BP Mod - allow backface filtering and unflipped normals

   use Math;



   --     enum EFlags
   --
   use type Flags;

   kF_None                : constant Flags := 0;
   kF_FilterBackfaces     : constant Flags := 2**0;
   kF_KeepUnflippedNormal : constant Flags := 2**1;           -- Prevents returned face normal getting flipped when a ray hits a back-facing triangle
   kF_Terminator          : constant Flags := 16#FFFFFFFF#;






   --- btTriangleRaycastCallback
   --

   type btTriangleRaycastCallback is abstract new impact.d3.triangle_Callback.item with
      record
         m_from,                          -- input
         m_to          : math.Vector_3;   --

         m_flags       : Flags;
         m_hitFraction : math.Real;
      end record;



   procedure define (Self : in out btTriangleRaycastCallback;   from, to : in math.Vector_3;
                                                                flags    : in d3.Flags := 0);



   overriding procedure processTriangle (Self : in out btTriangleRaycastCallback;   triangle      : access math.Matrix_3x3;
                                                                         partId        : in     Integer;
                                                                         triangleIndex : in     Integer    );

   function  reportHit       (Self : access btTriangleRaycastCallback;   hitNormalLocal : in math.Vector_3;
                                                                         hitFraction    : in math.Real;
                                                                         partId         : in Integer;
                                                                         triangleIndex  : in Integer) return math.Real
   is abstract;







   --- btTriangleConvexcastCallback
   --

   type btTriangleConvexcastCallback is abstract new impact.d3.triangle_Callback.item with
      record
         m_convexShape            : impact.d3.Shape.convex.view;

         m_convexShapeFrom,
         m_convexShapeTo,
         m_triangleToWorld        : Transform_3d;

         m_hitFraction,
         m_triangleCollisionMargin,
         m_allowedPenetration     : math.Real;
      end record;



   procedure define (Self : in out btTriangleConvexcastCallback;   convexShape             : in impact.d3.Shape.convex.view;
                                                                   convexShapeFrom,
                                                                   convexShapeTo           : in Transform_3d;
                                                                   triangleToWorld         : in Transform_3d;
                                                                   triangleCollisionMargin : in math.Real);



   procedure processTriangle (Self : in out btTriangleConvexcastCallback'Class;   triangle      : access math.Matrix_3x3;
                                                                                  partId        : in     Integer;
                              triangleIndex : in     Integer    );

   function  reportHit       (Self : access btTriangleConvexcastCallback;   hitNormalLocal : in math.Vector_3;
                                                                            hitPointLocal  : in math.Vector_3;
                                                                            hitFraction    : in math.Real;
                                                                            partId         : in Integer;
                                                                            triangleIndex  : in Integer) return math.Real
   is abstract;



end impact.d3.triangle_Callback.raycast;
