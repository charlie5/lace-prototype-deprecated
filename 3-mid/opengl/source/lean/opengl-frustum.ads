package openGL.Frustum
--
-- Provide frustum operations.
--
is

   type plane_Id    is (Left, Right, High, Low, Near, Far);


   type plane_Array is array (plane_Id) of openGL.Geometry_3d.Plane;

   procedure normalise (the_Planes : in out plane_Array);

end openGL.Frustum;
