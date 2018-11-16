package body impact.d3.contact_solver_Info
is


   function to_solver_Info return Item
   is
      Self : Item;
   begin
      Self.m_tau                                :=   0.6;
      Self.m_damping                            :=   1.0;
      Self.m_friction                           :=   0.3;
      Self.m_restitution                        :=   0.0;
      Self.m_maxErrorReduction                  :=  20.0;
      Self.m_numIterations                      :=  10;
      Self.m_erp                                :=   0.2;
      Self.m_erp2                               :=   0.1;
      Self.m_globalCfm                          :=   0.0;
      Self.m_sor                                :=   1.0;
      Self.m_splitImpulse                       := False;
      Self.m_splitImpulsePenetrationThreshold   :=  -0.02;
      Self.m_linearSlop                         :=   0.0;
      Self.m_warmstartingFactor                 :=   0.85;
      Self.m_solverMode                         := SOLVER_USE_WARMSTARTING or SOLVER_SIMD; -- | SOLVER_RANDMIZE_ORDER;
      Self.m_restingContactRestitutionThreshold :=   2;        -- resting contact lifetime threshold to disable restitution
      Self.m_minimumSolverBatchSize             := 128;        -- try to combine islands until the amount of constraints reaches this limit

      return Self;
   end to_solver_Info;


end impact.d3.contact_solver_Info;
