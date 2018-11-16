
package body impact.d3.collision.point_Collector
is


   overriding procedure addContactPoint (Self : in out Item;   normalOnBInWorld : in math.Vector_3;
                                                    pointInWorld     : in math.Vector_3;
                                                    depth            : in math.Real  )
   is
   begin
      if depth < Self.m_distance then
         Self.m_hasResult        := True;
         Self.m_normalOnBInWorld := normalOnBInWorld;
         Self.m_pointInWorld     := pointInWorld;
         Self.m_distance         := depth;                -- negative means penetration
      end if;
   end addContactPoint;


end impact.d3.collision.point_Collector;
