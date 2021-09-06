package body openGL.Frustum
is

   procedure normalise (Planes : in out Plane_array)
   is
      use Geometry_3D;
   begin
      for Each in Planes'Range
      loop
         normalise (Planes (Each));
      end loop;
   end normalise;

end openGL.Frustum;
