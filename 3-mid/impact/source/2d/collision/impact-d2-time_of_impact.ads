with impact.d2.Math,
     impact.d2.Distance;



package impact.d2.Time_of_impact
--
--
--
is
   use impact.d2.Math;



   --  Input parameters for b2TimeOfImpact
   --
   type b2TOIInput is
      record
         proxyA : aliased distance.b2DistanceProxy;
         proxyB : aliased distance.b2DistanceProxy;
         sweepA : b2Sweep;
         sweepB : b2Sweep;
         tMax   : float32;                -- defines sweep interval [0, tMax]
      end record;


   type State is (e_unknown, e_failed, e_overlapped, e_touching, e_separated);


   --  Output parameters for b2TimeOfImpact.
   --
   type b2TOIOutput is
      record
           state : time_of_impact.State;
           t     : float32;
      end record;




   --  Compute the upper bound on time before two shapes penetrate. Time is represented as
   --  a fraction between [0,tMax]. This uses a swept separating axis and may miss some intermediate,
   --  non-tunneling collision. If you change the time interval, you should call this function again.
   --  Note: use b2Distance to compute the contact point and normal at the time of impact.
   --
   procedure b2TimeOfImpact (output : access b2TOIOutput;   input : in b2TOIInput);



private


   type Kind is (e_points, e_faceA, e_faceB);



   type b2SeparationFunction is tagged
      record
         m_proxyA     : access constant distance.b2DistanceProxy;
         m_proxyB     : access constant distance.b2DistanceProxy;
         m_sweepA,
         m_sweepB     : b2Sweep;

         m_kind       : Kind;
         m_localPoint : b2Vec2;
         m_axis       : aliased b2Vec2;
      end record;


   function Initialize        (Self : access b2SeparationFunction;   cache  : access constant distance.b2SimplexCache;
                                                                     proxyA : access constant distance.b2DistanceProxy;   sweepA : in b2Sweep;
                                                                     proxyB : access constant distance.b2DistanceProxy;   sweepB : in b2Sweep) return float32;

   function FindMinSeparation (Self : in     b2SeparationFunction;   indexA, indexB : access int32;
                                                                     t              :        float32) return float32;

   function Evaluate          (Self : in     b2SeparationFunction;   indexA, indexB : access int32;
                                                                     t              :        float32) return float32;

end impact.d2.Time_of_impact;
