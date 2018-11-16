package body impact.d3.collision.Detector.discrete
is


   overriding procedure addContactPoint (Self : in out btStorageResult;   normalOnBInWorld : in math.Vector_3;
                                                               pointInWorld     : in math.Vector_3;
                                                               depth            : in math.Real  )
   is
   begin
      if depth < Self.m_distance then
         Self.m_normalOnSurfaceB := normalOnBInWorld;
         Self.m_closestPointInB  := pointInWorld;
         Self.m_distance         := depth;
      end if;
   end addContactPoint;




end impact.d3.collision.Detector.discrete;

