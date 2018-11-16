with impact.d3.Containers;

with impact.d3.constraint_Solver;
with impact.d3.Object;
with impact.d3.Manifold;
with impact.d3.Joint;
with impact.d3.contact_solver_Info;
with impact.d3.Dispatcher;
with impact.d3.solver_Constraint;
with impact.d3.Object.rigid;
with impact.d3.manifold_Point;
with Interfaces;


--  class btIDebugDraw;
--  #include "impact.d3.Joint.contact.h"
--  #include "btSolverBody.h"
--  #include "impact.d3.solver_Constraint.h"
--  #include "impact.d3.Joint.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.manifold_Point.h"



package impact.d3.constraint_Solver.sequential_impulse
--
--  The impact.d3.constraint_Solver.sequential_impulse is a fast SIMD implementation of the Projected Gauss Seidel (iterative LCP) method.
--
is

   type Item is new impact.d3.constraint_Solver.item with private;



   subtype Prefered is Item;

--  #ifndef BT_PREFER_SIMD
--  typedef impact.d3.constraint_Solver.sequential_impulse impact.d3.constraint_Solver.sequential_impulsePrefered;
--  #endif




   --- Forge
   --

   function  to_constraint_Solver return Item;

   overriding procedure destruct (Self : in out Item);





   --- Operations
   --

--     procedure prepareSolve (Self : in out Item;   numBodies    : in Integer;
--                                                   numManifolds : in Integer);


   overriding function  solveGroup (Self : access     Item;   bodies      : access impact.d3.Object.Vector;
                                                   manifold    : access impact.d3.Manifold.Vector;
                                                   constraints : access impact.d3.Joint.Vector;
                                                   info        : in     impact.d3.contact_solver_Info.Item'Class;
                                                   dispatcher  : in     impact.d3.Dispatcher.item'Class) return math.Real;


--     procedure allSolved    (Self : in out Item;   info : in impact.d3.contact_solver_Info.Item'Class);

   overriding procedure reset        (Self : in out Item);
   --
   --  Clear internal cached data and reset random seed.




   function btRand2 (Self : access Item                ) return interfaces.Unsigned_64;
   function btRandInt2 (Self : access Item;   n : in Integer) return Integer;


   procedure setRandSeed (Self :    out Item;   seed : in interfaces.Unsigned_64);
   function  getRandSeed (Self : in     Item)      return interfaces.Unsigned_64;


--          void        setRandSeed(unsigned long seed)
--          {
--                  m_btSeed2 = seed;
--          }


--          unsigned long        getRandSeed() const
--          {
--                  return m_btSeed2;
--          }






private


   type Item is new impact.d3.constraint_Solver.item with
      record
         m_tmpSolverContactConstraintPool         : impact.d3.solver_Constraint.btConstraintArray;
         m_tmpSolverNonContactConstraintPool      : impact.d3.solver_Constraint.btConstraintArray;
         m_tmpSolverContactFrictionConstraintPool : impact.d3.solver_Constraint.btConstraintArray;

         m_orderTmpConstraintPool                 : Containers.Integer_Vector;
         m_orderFrictionConstraintPool            : Containers.Integer_Vector;

         m_tmpConstraintSizesPool                 : impact.d3.Joint.btConstraintInfo1_Vector;

         m_btSeed2                                : interfaces.Unsigned_64;   -- Used for re-arranging the constraint rows. Improves convergence/quality of friction.
      end record;


   procedure setupFrictionConstraint (Self : in out Item;   solverConstraint         : in out impact.d3.solver_Constraint.item;
                                                            normalAxis               : in     math.Vector_3;
                                                            solverBodyA, solverBodyB : in     impact.d3.Object.rigid.View;
                                                            cp                       : in out impact.d3.manifold_Point.item;
                                                            rel_pos1, rel_pos2       : in     math.Vector_3;
                                                            colObj0,  colObj1        : in     impact.d3.Object.view;
                                                            relaxation               : in     math.Real;
                                                            desiredVelocity          : in     math.Real := 0.0;
                                                            cfmSlip                  : in     math.Real := 0.0);


   function  addFrictionConstraint   (Self : access Item;   normalAxis               : in     math.Vector_3;
                                                            solverBodyA, solverBodyB : in     impact.d3.Object.rigid.View;
                                                            frictionIndex            : in     Integer;
                                                            cp                       : in out impact.d3.manifold_Point.item;
                                                            rel_pos1, rel_pos2       : in     math.Vector_3;
                                                            colObj0,  colObj1        : in     impact.d3.Object.view;
                                                            relaxation               : in     math.Real;
                                                            desiredVelocity          : in     math.Real := 0.0;
                                                            cfmSlip                  : in     math.Real := 0.0) return access impact.d3.solver_Constraint.item;


   procedure setupContactConstraint (Self : in out Item;   solverConstraint         : in out impact.d3.solver_Constraint.item;
                                                            colObj0,  colObj1        : in     impact.d3.Object.view;
                                                            cp                       : in out impact.d3.manifold_Point.item;
                                                            infoGlobal               : in     impact.d3.contact_solver_Info.item'Class;
                                                            vel                      : in out math.Vector_3;
                                                            rel_vel                  : in out math.Real;
                                                            relaxation               : in out math.Real;
                                     rel_pos1, rel_pos2       : in out math.Vector_3);


   procedure setFrictionConstraintImpulse (Self : in out Item;   solverConstraint : in out impact.d3.solver_Constraint.item;
                                                                 rb0, rb1         : in     impact.d3.Object.rigid.View;
                                                                 cp               : in out impact.d3.manifold_Point.item;
                                                                 infoGlobal       : in     impact.d3.contact_solver_Info.item'Class);




   function  restitutionCurve             (Self : access Item;   rel_vel     : in     math.Real;
                                                                 restitution : in     math.Real) return math.Real;



   procedure convertContact (Self : access Item;   manifold   : in     impact.d3.Manifold.view;
                             infoGlobal : in     impact.d3.contact_solver_Info.item'Class);


   procedure resolveSplitPenetrationImpulseCacheFriendly (Self : in out Item;   body1, body2      : in     impact.d3.Object.rigid.View;
                                                          contactConstraint : in out impact.d3.solver_Constraint.item);


   function  getOrInitSolverBody (Self : in Item;   the_body : access impact.d3.Object.item'Class) return Integer;
   --
   --  internal method




   procedure resolveSingleConstraintRowGeneric (Self : in out Item;   body1, body2      : in     impact.d3.Object.rigid.View;
                                                contactConstraint : in out impact.d3.solver_Constraint.item);

   procedure resolveSingleConstraintRowLowerLimit (Self : in out Item;   body1, body2      : in     impact.d3.Object.rigid.View;
                                                   contactConstraint : in out impact.d3.solver_Constraint.item);


   function  getFixedBody return impact.d3.Object.rigid.view;



   procedure solveGroupCacheFriendlySplitImpulseIterations (Self : in out Item;   bodies      : access impact.d3.Object   .Vector;
                                                                                  manifoldPtr : access impact.d3.Manifold.Vector;
                                                                                  constraints : access impact.d3.Joint.Vector;
                                                                                  infoGlobal  : in     impact.d3.contact_solver_Info.item'Class);


   function  solveGroupCacheFriendlyFinish (Self : in out Item;   bodies      : access impact.d3.Object   .Vector;
                                                                  manifoldPtr : access impact.d3.Manifold.Vector;
                                                                  constraints : access impact.d3.Joint   .Vector;
                                                                  infoGlobal  : in     impact.d3.contact_solver_Info .Item'Class) return math.Real;


   function  solveSingleIteration          (Self : in out Item;   iteration   : in     Integer;
                                                                  bodies      : access impact.d3.Object   .Vector;
                                                                  manifoldPtr : access impact.d3.Manifold.Vector;
                                                                  constraints : access impact.d3.Joint   .Vector;
                                                                  infoGlobal  : in     impact.d3.contact_solver_Info .Item'Class) return math.Real;


   function  solveGroupCacheFriendlySetup  (Self : in out Item;   bodies      : access impact.d3.Object   .Vector;
                                                                  manifoldPtr : access impact.d3.Manifold.Vector;
                                                                  constraints : access impact.d3.Joint   .Vector;
                                                                  infoGlobal  : in     impact.d3.contact_solver_Info .Item'Class) return math.Real;

   function  solveGroupCacheFriendlyIterations (Self : in out Item;   bodies      : access impact.d3.Object   .Vector;
                                                                      manifoldPtr : access impact.d3.Manifold.Vector;
                                                                      constraints : access impact.d3.Joint   .Vector;
                                                                      infoGlobal  : in     impact.d3.contact_solver_Info .Item'Class) return math.Real;


end impact.d3.constraint_Solver.sequential_impulse;




--  class impact.d3.constraint_Solver.sequential_impulse : public impact.d3.constraint_Solver
--  {
--  protected:


--  tbd: are these needed ?
--
--          void        resolveSplitPenetrationSIMD(
--          impact.d3.Object.rigid& body1,
--          impact.d3.Object.rigid& body2,
--          const impact.d3.solver_Constraint& contactConstraint);
--
--          void        resolveSingleConstraintRowGenericSIMD(impact.d3.Object.rigid& body1,impact.d3.Object.rigid& body2,const impact.d3.solver_Constraint& contactConstraint);
--
--          void        resolveSingleConstraintRowLowerLimitSIMD(impact.d3.Object.rigid& body1,impact.d3.Object.rigid& body2,const impact.d3.solver_Constraint& contactConstraint);


--  };




