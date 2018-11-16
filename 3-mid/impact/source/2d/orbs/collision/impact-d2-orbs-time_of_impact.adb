


package body impact.d2.orbs.Time_of_impact
is
   use type int32;


   b2_toiCalls,
   b2_toiIters,
   b2_toiMaxIters     : int32;

   b2_toiRootIters,
   b2_toiMaxRootIters : int32;

   b2_toiMaxOptIters  : int32;




   --  TODO_ERIN might not need to return the separation
   --
   function Initialize (Self : access b2SeparationFunction;   cache  : access constant distance.b2SimplexCache;
                                                              proxyA : access constant distance.b2DistanceProxy;   sweepA : in b2Sweep;
                                                              proxyB : access constant distance.b2DistanceProxy;   sweepB : in b2Sweep) return float32
   is
      use impact.d2.orbs.Distance;
      use type uint8;

      count    :         int32;
      xfA, xfB : aliased b2Transform;
   begin
      Self.m_proxyA := proxyA.all'Unchecked_Access;
      Self.m_proxyB := proxyB.all'Unchecked_Access;
      count         := int32 (cache.count);
      pragma Assert (0 < count and then count < 3);

      Self.m_sweepA := sweepA;
      Self.m_sweepB := sweepB;

      GetTransform (Self.m_sweepA,  xfA'Access, 0.0);
      GetTransform (Self.m_sweepB,  xfB'Access, 0.0);


      if count = 1 then
         Self.m_Kind := e_points;
         declare
            localPointA : constant b2Vec2 := GetVertex (Self.m_proxyA.all,  int32 (cache.indexA (1)));
            localPointB : constant b2Vec2 := GetVertex (Self.m_proxyB.all,  int32 (cache.indexB (1)));
            pointA      : constant b2Vec2 := b2Mul (xfA, localPointA);
            pointB      : constant b2Vec2 := b2Mul (xfB, localPointB);
            s           : float32;
         begin
            Self.m_axis := pointB - pointA;
            s           := Normalize (Self.m_axis'Access);
            return s;
         end;

      elsif cache.indexA (1) = cache.indexA (2) then
         --  Two points on B and one on A.
         Self.m_Kind := e_faceB;
         declare
            localPointB1 : constant b2Vec2 := GetVertex (proxyB.all,  int32 (cache.indexB (1)));
            localPointB2 : constant b2Vec2 := GetVertex (proxyB.all,  int32 (cache.indexB (2)));

            normal,
            pointA,
            localPointA,
            pointB       : b2Vec2;

            s            : float32;
         begin
            Self.m_axis       := b2Cross (localPointB2 - localPointB1,  1.0);
            Normalize (Self.m_axis);
            normal            := b2Mul (xfB.R, Self.m_axis);

            Self.m_localPoint := 0.5 * (localPointB1 + localPointB2);
            pointB            := b2Mul (xfB, Self.m_localPoint);

            localPointA       := GetVertex (proxyA.all,  int32 (cache.indexA (1)));
            pointA            := b2Mul (xfA, localPointA);

            s                 := b2Dot (pointA - pointB,  normal);

            if s < 0.0 then
               Self.m_axis := -Self.m_axis;
               s           := -s;
            end if;

            return s;
         end;

      else
         --  Two points on A and one or two points on B.
         Self.m_Kind := e_faceA;
         declare
            localPointA1 : constant b2Vec2 := GetVertex (Self.m_proxyA.all,  int32 (cache.indexA (1)));
            localPointA2 : constant b2Vec2 := GetVertex (Self.m_proxyA.all,  int32 (cache.indexA (2)));

            normal,
            pointA,
            localPointB,
            pointB       : b2Vec2;

            s            : float32;
         begin
            Self.m_axis := b2Cross (localPointA2 - localPointA1, 1.0);
            Normalize (Self.m_axis);
            normal := b2Mul (xfA.R, Self.m_axis);

            Self.m_localPoint := 0.5 * (localPointA1 + localPointA2);
            pointA := b2Mul (xfA, Self.m_localPoint);

            localPointB := GetVertex (Self.m_proxyB.all,  int32 (cache.indexB (1)));
            pointB      := b2Mul (xfB, localPointB);

            s := b2Dot (pointB - pointA,  normal);

            if s < 0.0 then
               Self.m_axis := -Self.m_axis;
               s := -s;
            end if;

            return s;
         end;

      end if;

   end initialize;







   function FindMinSeparation (Self : in b2SeparationFunction;   indexA, indexB : access int32;
                                                                 t              :        float32) return float32
   is
      use impact.d2.orbs.Distance;
      xfA, xfB                : aliased b2Transform;

      normal,
      axisA,       axisB,
      localPointA, localPointB,
      pointA,      pointB     : b2Vec2;

      separation              : float32;
   begin
      getTransform (Self.m_sweepA,  xfA'Access, t);
      getTransform (Self.m_sweepB,  xfB'Access, t);

      case Self.m_kind is
      when e_points =>
         axisA := b2MulT (xfA.R,  Self.m_axis);
         axisB := b2MulT (xfB.R, -Self.m_axis);

         indexA.all := GetSupport (Self.m_proxyA.all, axisA);
         indexB.all := GetSupport (Self.m_proxyB.all, axisB);

         localPointA := GetVertex (Self.m_proxyA.all, indexA.all);
         localPointB := GetVertex (Self.m_proxyB.all, indexB.all);

         pointA := b2Mul (xfA, localPointA);
         pointB := b2Mul (xfB, localPointB);

         separation := b2Dot (pointB - pointA,  Self.m_axis);
         return separation;

      when e_faceA =>
         normal := b2Mul (xfA.R, Self.m_axis);
         pointA := b2Mul (xfA, Self.m_localPoint);

         axisB  := b2MulT (xfB.R, -normal);

         indexA.all := -1;
         indexB.all := GetSupport (Self.m_proxyB.all, axisB);

         localPointB := GetVertex (Self.m_proxyB.all, indexB.all);
         pointB      := b2Mul (xfB, localPointB);

         separation := b2Dot (pointB - pointA,  normal);
         return separation;

      when e_faceB =>
         normal := b2Mul (xfB.R, Self.m_axis);
         pointB := b2Mul (xfB, Self.m_localPoint);

         axisA := b2MulT (xfA.R, -normal);

         indexB.all := -1;
         indexA.all := GetSupport (Self.m_proxyA.all, axisA);

         localPointA := GetVertex (Self.m_proxyA.all, indexA.all);
         pointA      := b2Mul (xfA, localPointA);

         separation := b2Dot (pointA - pointB,  normal);
         return separation;

      when others =>
         pragma Assert (False);
         indexA.all := -1;
         indexB.all := -1;
         return 0.0;
      end case;

   end FindMinSeparation;





   function Evaluate (Self : in b2SeparationFunction;   indexA, indexB : access int32;
                                                        t              :        float32) return float32
   is
      use impact.d2.orbs.Distance;
      xfA, xfB                : aliased b2Transform;

      normal,
      axisA,       axisB,
      localPointA, localPointB,
      pointA,      pointB     : b2Vec2;
      pragma Unreferenced (axisA, axisB);

      separation              : float32;
   begin
      GetTransform (Self.m_sweepA,  xfA'Access, t);
      GetTransform (Self.m_sweepB,  xfB'Access, t);

      case Self.m_Kind is
      when e_points =>
         axisA := b2MulT (xfA.R,  Self.m_axis);
         axisB := b2MulT (xfB.R, -Self.m_axis);

         localPointA := GetVertex (Self.m_proxyA.all, indexA.all);
         localPointB := GetVertex (Self.m_proxyB.all, indexB.all);

         pointA := b2Mul (xfA, localPointA);
         pointB := b2Mul (xfB, localPointB);

         separation := b2Dot (pointB - pointA,  Self.m_axis);
         return separation;

      when e_faceA =>
         normal := b2Mul (xfA.R, Self.m_axis);
         pointA := b2Mul (xfA,   Self.m_localPoint);

         axisB := b2MulT (xfB.R, -normal);

         localPointB := GetVertex (Self.m_proxyB.all, indexB.all);
         pointB      := b2Mul (xfB, localPointB);

         separation := b2Dot (pointB - pointA,  normal);
         return separation;

      when e_faceB =>
         normal := b2Mul (xfB.R, Self.m_axis);
         pointB := b2Mul (xfB,   Self.m_localPoint);

         axisA := b2MulT (xfA.R, -normal);

         localPointA := getVertex (Self.m_proxyA.all, indexA.all);
         pointA      := b2Mul (xfA, localPointA);

         separation := b2Dot (pointA - pointB, normal);
         return separation;

      when others =>
         pragma Assert (False);
         return 0.0;
      end case;

   end Evaluate;







   --  CCD via the local separating axis method. This seeks progression
   --  by computing the largest time at which separation is maintained.
   --
   procedure b2TimeOfImpact (output : access b2TOIOutput;   input : in b2TOIInput)
   is
      proxyA          : distance.b2DistanceProxy renames input.proxyA;
      proxyB          : distance.b2DistanceProxy renames input.proxyB;

      xfA, xfB        : aliased b2Transform;

      sweepA          : b2Sweep := input.sweepA;
      sweepB          : b2Sweep := input.sweepB;

      tMax            : float32 := input.tMax;

      totalRadius     : constant float32 := proxyA.m_radius + proxyB.m_radius;
      target          : float32 := float32'Max (b2_linearSlop, totalRadius - 3.0 * b2_linearSlop);
      tolerance       : float32 := 0.25 * b2_linearSlop;
      pragma Assert (target > tolerance);

      t1              : float32  := 0.0;
      k_maxIterations : constant := 20;        -- TODO_ERIN b2Settings
      iter            : int32    := 0;

      cache           : aliased distance.b2SimplexCache;
      distanceInput   : aliased distance.b2DistanceInput;
      distanceOutput  : aliased distance.b2DistanceOutput;

      fcn             : aliased b2SeparationFunction;

      unused          : float32;
      pragma Unreferenced (unused);
   begin
      b2_toiCalls := b2_toiCalls + 1;

      output.state := e_unknown;
      output.t     := input.tMax;

      --  Large rotations can make the root finder fail, so we normalize the sweep angles.
      Normalize (sweepA);
      Normalize (sweepB);

      --  Prepare input for distance query.
      cache.count := 0;

      distanceInput.proxyA   := input.proxyA;
      distanceInput.proxyB   := input.proxyB;
      distanceInput.useRadii := False;

      --  The outer loop progressively attempts to compute new separating axes.
      --  This loop terminates when an axis is repeated (no progress is made).
      loop
         GetTransform (sweepA,  xfA'Access, t1);
         GetTransform (sweepB,  xfB'Access, t1);

         --  Get the distance between shapes. We can also use the results
         --  to get a separating axis.
         distanceInput.transformA := xfA;
         distanceInput.transformB := xfB;

         distance.b2Distance (distanceOutput'Access,  cache'Access,  distanceInput);

         --  If the shapes are overlapped, we give up on continuous collision.
         if distanceOutput.distance <= 0.0 then
            --  Failure!
            output.state := e_overlapped;
            output.t     := 0.0;
            exit;
         end if;

         if distanceOutput.distance < target + tolerance then
            --  Victory!
            output.state := e_touching;
            output.t     := t1;
            exit;
         end if;

         --  Initialize the separating axis.
         unused := fcn.Initialize (cache'Access,  proxyA'Access, sweepA,  proxyB'Access, sweepB);


         --  #if 0
         --                  -- Dump the curve seen by the root finder
         --                  {
         --                          const int32 N := 100;
         --                          float32 dx := 1.0f / N;
         --                          float32 xs[N+1];
         --                          float32 fs[N+1];
         --
         --                          float32 x := 0.0f;
         --
         --                          for (int32 i = 0; i <= N; ++i)
         --                          {
         --                                  sweepA.GetTransform(&xfA, x);
         --                                  sweepB.GetTransform(&xfB, x);
         --                                  float32 f := fcn.Evaluate(xfA, xfB) - target;
         --
         --                                  printf("%g %g\n", x, f);
         --
         --                                  xs[i] := x;
         --                                  fs[i] := f;
         --
         --                                  x += dx;
         --                          }
         --                  }
         --  #endif


         --  Compute the TOI on the separating axis. We do this by successively
         --  resolving the deepest point. This loop is bounded by the number of vertices.
         declare

            done : Boolean := False;
            t2 : float32 := tMax;
            pushBackIter : int32 := 0;
         begin
            loop
               declare
                  use type uint32;

                  --  Find the deepest point at t2. Store the witness point indices.
                  indexA,
                  indexB        : aliased int32;
                  s2            : float32 := fcn.FindMinSeparation (indexA'Access, indexB'Access, t2);
                  s1            : float32;

                  rootIterCount : int32;
                  a1, a2        : float32;
                  t, s          : float32;

               begin
                  --  Is the final configuration separated?
                  if s2 > target + tolerance then
                     --  Victory!
                     output.state := e_separated;
                     output.t     := tMax;
                     done         := True;
                     exit;
                  end if;

                  --  Has the separation reached tolerance?
                  if s2 > target - tolerance then
                     t1 := t2;     -- Advance the sweeps
                     exit;
                  end if;

                  --  Compute the initial separation of the witness points.
                  s1 := fcn.Evaluate (indexA'Access, indexB'Access, t1);

                  --  Check for initial overlap. This might happen if the root finder
                  --  runs out of iterations.
                  if s1 < target - tolerance then
                     output.state := e_failed;
                     output.t     := t1;
                     done         := True;
                     exit;
                  end if;

                  --  Check for touching
                  if s1 <= target + tolerance then
                     --  Victory! t1 should hold the TOI (could be 0.0).
                     output.state := e_touching;
                     output.t     := t1;
                     done         := True;
                     exit;
                  end if;

                  --  Compute 1D root of: f(x) - target = 0
                  rootIterCount := 0;
                  a1            := t1;
                  a2            := t2;

                  loop
                     --  Use a mix of the secant rule and bisection.
                     if (uint32 (rootIterCount) and 1) /= 0 then
                        t := a1 + (target - s1) * (a2 - a1) / (s2 - s1);   -- Secant rule to improve convergence.
                     else
                        t := 0.5 * (a1 + a2);                              -- Bisection to guarantee progress.
                     end if;

                     s := fcn.Evaluate (indexA'Access, indexB'Access, t);

                     if abs (s - target) < tolerance then
                        t2 := t;                                           -- t2 holds a tentative value for t1
                        exit;
                     end if;

                     --  Ensure we continue to bracket the root.
                     if s > target then
                        a1 := t;
                        s1 := s;
                     else
                        a2 := t;
                        s2 := s;
                     end if;

                     rootIterCount   := rootIterCount + 1;
                     b2_toiRootIters := b2_toiRootIters + 1;

                     exit when rootIterCount = 50;
                  end loop;

                  b2_toiMaxRootIters := int32'Max (b2_toiMaxRootIters, rootIterCount);
                  pushBackIter       := pushBackIter + 1;

                  exit when pushBackIter = b2_maxPolygonVertices;
               end;
            end loop;

            iter        := iter + 1;
            b2_toiIters := b2_toiIters + 1;

            exit when done;

            if iter = k_maxIterations then
               --  Root finder got stuck. Semi-victory.
               output.state := e_failed;
               output.t     := t1;
               exit;
            end if;
         end;
      end loop;

      b2_toiMaxIters := int32'Max (b2_toiMaxIters, iter);
   end b2TimeOfImpact;



end impact.d2.orbs.Time_of_impact;
