

package impact.d3.contact_solver_Info
--
--
--
is

   --- btSolverMode
   --
   SOLVER_RANDMIZE_ORDER            : constant Flags;
   SOLVER_FRICTION_SEPARATE         : constant Flags;
   SOLVER_USE_WARMSTARTING          : constant Flags;
   SOLVER_USE_FRICTION_WARMSTARTING : constant Flags;
   SOLVER_USE_2_FRICTION_DIRECTIONS : constant Flags;
   SOLVER_ENABLE_FRICTION_DIRECTION_CACHING             : constant Flags;
   SOLVER_DISABLE_VELOCITY_DEPENDENT_FRICTION_DIRECTION : constant Flags;
   SOLVER_CACHE_FRIENDLY            : constant Flags;
   SOLVER_SIMD                      : constant Flags;        -- Enabled for Windows, the solver innerloop is branchless SIMD, 40% faster than FPU/scalar version.
   SOLVER_CUDA                      : constant Flags;        -- will be open sourced during Game Developers Conference 2009. Much faster.




   type Data is tagged
      record
         m_tau,
         m_damping,                   -- Global non-contact constraint damping, can be locally overridden by constraints during 'getInfo2'.
         m_friction,
         m_timeStep,
         m_restitution            : math.Real;

         m_numIterations          : Integer;

         m_maxErrorReduction,
         m_sor,
         m_erp,                                    -- Used as Baumgarte factor.
         m_erp2,                                   -- Used in Split Impulse.
         m_globalCfm              : math.Real;     -- Constraint force mixing.

         m_splitImpulse           : Boolean;

         m_splitImpulsePenetrationThreshold,
         m_linearSlop,
         m_warmstartingFactor     : math.Real;

         m_solverMode             : Flags;

         m_restingContactRestitutionThreshold,
         m_minimumSolverBatchSize : Integer;
      end record;





   type Item is new Data with private;


   function to_solver_Info return Item;







private


   type Item is new Data with
      record
         null;
      end record;


   use type Flags;

   SOLVER_RANDMIZE_ORDER            : constant Flags := 2**0;
   SOLVER_FRICTION_SEPARATE         : constant Flags := 2**1;
   SOLVER_USE_WARMSTARTING          : constant Flags := 2**2;
   SOLVER_USE_FRICTION_WARMSTARTING : constant Flags := 2**3;
   SOLVER_USE_2_FRICTION_DIRECTIONS : constant Flags := 2**4;
   SOLVER_ENABLE_FRICTION_DIRECTION_CACHING             : constant Flags := 2**5;
   SOLVER_DISABLE_VELOCITY_DEPENDENT_FRICTION_DIRECTION : constant Flags := 2**6;
   SOLVER_CACHE_FRIENDLY            : constant Flags := 2**7;
   SOLVER_SIMD                      : constant Flags := 2**8;
   SOLVER_CUDA                      : constant Flags := 2**9;


end impact.d3.contact_solver_Info;
