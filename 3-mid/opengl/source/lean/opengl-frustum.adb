package body openGL.Frustum
is


   procedure normalise (the_Planes : in out plane_Array)
   is
      use Geometry_3d;
   begin
      for Each in the_Planes'Range
      loop
         normalise (the_Planes (Each));
      end loop;
   end normalise;


end openGL.Frustum;
