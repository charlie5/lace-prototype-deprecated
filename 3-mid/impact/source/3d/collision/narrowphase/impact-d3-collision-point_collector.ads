with impact.d3.collision.Detector.discrete;



package impact.d3.collision.point_Collector
--
--
--
is

   type Item is new impact.d3.collision.Detector.discrete.Result with
      record
         m_normalOnBInWorld,
         m_pointInWorld    : math.Vector_3;
         m_distance        : math.Real    := BT_LARGE_FLOAT;         -- negative means penetration
         m_hasResult       : Boolean      := False;
      end record;


   overriding procedure addContactPoint (Self : in out Item;   normalOnBInWorld : in math.Vector_3;
                                                    pointInWorld     : in math.Vector_3;
                                                    depth            : in math.Real  );


   overriding procedure setShapeIdentifiersA (Self : in out Item;   partId0 : in Integer;
                                                         index0  : in Integer)   is null;

   overriding procedure setShapeIdentifiersB (Self : in out Item;   partId1 : in Integer;
                                                         index1  : in Integer)   is null;



end impact.d3.collision.point_Collector;


--  struct impact.d3.collision.point_Collector : public impact.d3.collision.Detector.discrete::Result
--  {
--
--          virtual void addContactPoint(const impact.d3.Vector& normalOnBInWorld,const impact.d3.Vector& pointInWorld,impact.d3.Scalar depth)
--          {
--                  if (depth< m_distance)
--                  {
--                          m_hasResult = true;
--                          m_normalOnBInWorld = normalOnBInWorld;
--                          m_pointInWorld = pointInWorld;
--                          //negative means penetration
--                          m_distance = depth;
--                  }
--          }
--  };

