with impact.d3.Matrix,
     impact.d3.Vector,
     impact.d3.Shape.convex.internal.polyhedral.triangle,
     impact.d3.collision.simplex_Solver.voronoi,
     impact.d3.collision.convex_penetration_depth_Solver.gjk_epa,
     impact.d3.collision.convex_Raycast,
     impact.d3.collision.convex_Raycast.continuous_convex;



package body impact.d3.triangle_Callback.raycast
is


   --- btTriangleRaycastCallback
   --

   procedure define (Self : in out btTriangleRaycastCallback;   from, to : in math.Vector_3;
                                                                flags    : in d3.Flags := 0)
   is
   begin
      Self.m_from        := from;
      Self.m_to        := to;
      --  @BP Mod
      Self.m_flags       := flags;
      Self.m_hitFraction := 1.0;
   end define;






   overriding procedure processTriangle (Self : in out btTriangleRaycastCallback;   triangle      : access math.Matrix_3x3;
                                                                         partId        : in     Integer;
                                                                         triangleIndex : in     Integer    )
   is
      use impact.d3.Vector, impact.d3.Matrix, math.Vectors;

      vert0 : math.Vector_3 renames getRow (triangle.all, 1);
      vert1 : math.Vector_3 renames getRow (triangle.all, 2);
      vert2 : math.Vector_3 renames getRow (triangle.all, 3);

      v10 : constant math.Vector_3 := vert1 - vert0;
      v20 : constant math.Vector_3 := vert2 - vert0;

      triangleNormal : math.Vector_3 := cross (v10, v20);

      dist   : constant math.Real := dot (vert0,          triangleNormal);
      dist_a :          math.Real := dot (triangleNormal, Self.m_from);
      dist_b :          math.Real;

   begin
      dist_a := dist_a - dist;
      dist_b := dot (triangleNormal, Self.m_to);
      dist_b := dist_b - dist;


      if dist_a * dist_b  >=  0.0 then
         return; -- same sign
      end if;


      --  @BP Mod - Backface filtering
      if         (Self.m_flags and kF_FilterBackfaces) /= 0
        and then  dist_a > 0.0
      then
         return;   -- Backface, skip check
      end if;


      declare
         proj_length : constant math.Real    := dist_a - dist_b;
         distance    : constant math.Real    := dist_a / proj_length;
         point       :          math.Vector_3;
      begin
         --  Now we have the intersection point on the plane, we'll see if it's inside the triangle
         --  Add an epsilon as a tolerance for the raycast,
         --  in case the ray hits exacly on the edge of the triangle.
         --  It must be scaled for the triangle size.
         --
         if distance < Self.m_hitFraction then
            setInterpolate3 (point,  Self.m_from,  Self.m_to,  distance);

            declare
               edge_tolerance : constant math.Real := -0.0001 * length2 (triangleNormal);

               v0p            : constant math.Vector_3 := vert0 - point;
               v1p            : constant math.Vector_3 := vert1 - point;
               cp0            : constant math.Vector_3 := cross (v0p, v1p);

            begin

               if dot (cp0, triangleNormal) >= edge_tolerance then
                  declare
                     v2p : constant math.Vector_3 := vert2 - point;
                     cp1 : constant math.Vector_3 := cross (v1p, v2p);
                     cp2 : math.Vector_3;
                  begin
                     if dot (cp1, triangleNormal) >= edge_tolerance then
                        cp2 := cross (v2p, v0p);

                        if dot (cp2, triangleNormal) >= edge_tolerance then
                           --  @BP Mod
                           --  Triangle normal isn't normalized
                           normalize (triangleNormal);

                           --  @BP Mod - Allow for unflipped normal when raycasting against backfaces
                           --
                           if        (Self.m_flags and kF_KeepUnflippedNormal) /= 0
                             or else dist_a <= 0.0
                           then
                              Self.m_hitFraction := btTriangleRaycastCallback'Class (Self).reportHit (-triangleNormal, distance, partId, triangleIndex);
                           else
                              Self.m_hitFraction := btTriangleRaycastCallback'Class (Self).reportHit (triangleNormal, distance, partId, triangleIndex);
                           end if;

                        end if;

                     end if;
                  end;
               end if;

            end;
         end if;
      end;

   end processTriangle;







   --- btTriangleConvexcastCallback
   --


   procedure define (Self : in out btTriangleConvexcastCallback;   convexShape             : in impact.d3.Shape.convex.view;
                                                                   convexShapeFrom,
                                                                   convexShapeTo           : in Transform_3d;
                                                                   triangleToWorld         : in Transform_3d;
                     triangleCollisionMargin : in math.Real)
   is
   begin
      Self.m_convexShape             := convexShape;
      Self.m_convexShapeFrom         := convexShapeFrom;
      Self.m_convexShapeTo           := convexShapeTo;
      Self.m_triangleToWorld         := triangleToWorld;
      Self.m_hitFraction             := 1.0;
      Self.m_triangleCollisionMargin := triangleCollisionMargin;
      Self.m_allowedPenetration      := 0.0;
   end define;





   procedure processTriangle (Self : in out btTriangleConvexcastCallback'Class;   triangle      : access math.Matrix_3x3;
                                                                                  partId        : in     Integer;
                                                                                  triangleIndex : in     Integer    )
   is
      use impact.d3.Matrix;

      triangleShape           : aliased impact.d3.Shape.convex.internal.polyhedral.triangle.Item
        := impact.d3.Shape.convex.internal.polyhedral.triangle.to_triangle_Shape (getRow (triangle.all, 1),
                                                                                  getRow (triangle.all, 2),
                                                                                  getRow (triangle.all, 3));
      simplexSolver           : aliased impact.d3.collision.simplex_Solver.voronoi.Item;
      gjkEpaPenetrationSolver : aliased impact.d3.collision.convex_penetration_depth_Solver.gjk_epa.Item;

   begin
      triangleShape.setMargin (Self.m_triangleCollisionMargin);


      declare
         use impact.d3.collision.convex_Raycast.continuous_convex, impact.d3.Vector;

         convexCaster : aliased impact.d3.collision.convex_Raycast.continuous_convex.item
           := to_convex_Raycast (Self.m_convexShape,
                                 triangleShape'Access,
                                 simplexSolver'Access,
                                 gjkEpaPenetrationSolver'Access);

         castResult   : aliased impact.d3.collision.convex_Raycast.CastResult;
         unused       :         math.Real;
         pragma Unreferenced (unused);

      begin
         castResult.m_fraction           := 1.0;
         castResult.m_allowedPenetration := Self.m_allowedPenetration;

         if convexCaster.calcTimeOfImpact (Self.m_convexShapeFrom,
                                           Self.m_convexShapeTo,
                                           Self.m_triangleToWorld,
                                           Self.m_triangleToWorld,
                                           castResult'Access)
         then   -- add hit
            if length2 (castResult.m_normal)  >  0.0001 then

               if castResult.m_fraction < Self.m_hitFraction then
                  normalize (castResult.m_normal);

                  unused := Self.reportHit (castResult.m_normal,
                                            castResult.m_hitPoint,
                                            castResult.m_fraction,
                                            partId,
                                            triangleIndex);
               end if;

            end if;
         end if;
      end;

   end processTriangle;



end impact.d3.triangle_Callback.raycast;
