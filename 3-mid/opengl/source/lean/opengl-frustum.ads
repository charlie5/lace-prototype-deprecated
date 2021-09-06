package openGL.Frustum
--
-- Provide frustum operations.
--
is

   type Plane_Id    is (Left, Right, High, Low, Near, Far);
   type Plane_array is array (Plane_Id) of openGL.Geometry_3d.Plane;

   procedure normalise (Planes : in out Plane_array);

end openGL.Frustum;
