
package impact.d3.triangle_Callback
--
--  Provides a callback for each overlapping triangle when calling processAllTriangles.
--
--  This callback is called by processAllTriangles for all 'impact.d3.Shape.concave' derived class,
-- such as 'impact.d3.Shape.concave.triangle_mesh.bvh',
--         'impact.d3.Shape.concave.static_plane' and
--         'impact.d3.Shape.concave.height_field_terrain'.
--
is

   type Item is abstract tagged null record;

   procedure destruct        (Self : in out Item)
   is null;

   procedure processTriangle (Self : in out Item;   triangle      : access math.Matrix_3x3; -- each row is a vertex ... tbd: improve ?
                                                    partId        : in     Integer;
                                                    triangleIndex : in     Integer)
   is abstract;



   type btInternalTriangleIndexCallback is abstract tagged null record;

   procedure destruct                     (Self : in out btInternalTriangleIndexCallback)
   is null;

   procedure internalProcessTriangleIndex (Self : in out btInternalTriangleIndexCallback;   triangle      : access math.Matrix_3x3;
                                                                                            partId        : in     Integer;
                                                                                            triangleIndex : in     Integer)
   is null;

end impact.d3.triangle_Callback;
