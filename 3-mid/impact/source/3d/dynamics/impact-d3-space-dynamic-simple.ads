with impact.d3.Space.dynamic,
     impact.d3.Object.rigid,
     impact.d3.constraint_Solver;

use  impact.d3.Space.dynamic,
     impact.d3.Object.rigid,
     impact.d3.constraint_Solver;


package impact.d3.Space.dynamic.simple
is



   type Item is abstract new impact.d3.Space.dynamic.Item with private;

   type constraint_Solver_view is access all impact.d3.constraint_Solver.Item'Class;

   overriding function stepSimulation (Self          : access Item;
                            timeStep      : in     Real;
                            maxSubSteps   : in     Integer := 1;
                            fixedTimeStep : in     Real := 1.0 / 60.0) return Integer;

   overriding procedure setGravity (Self    : in out Item;
                         gravity : in     Vector_3);

   overriding function getGravity (Self : in Item) return Vector_3;

   overriding procedure addRigidBody (Self     : in out Item;
                           the_body : access impact.d3.Object.rigid.Item'Class);

   procedure addRigidBody (the_body    : access impact.d3.Object.rigid.Item'Class;
                           group, mask : in     Flags          );

   procedure removeRigidBody (Self     : in out Item;
                              the_body : access impact.d3.Object.rigid.Item'Class);

   procedure debugDrawWorld (Self : in out Item);

   --- tbd: leave these for the time being (rak) ...
   --
   --     procedure addAction (Self   : in out Item;
   --                          action : access impact.d3.Action.Item'Class);
   --
   --     procedure removeAction (Self   : in out Item;
   --                             action : access impact.d3.Action.Item'Class);

   procedure removeCollisionObject (Self            : in out Item;
                                    collisionObject : access impact.d3.Object.view);

   overriding procedure updateAabbs (Self : in out Item);

   overriding procedure synchronizeMotionStates (Self : in out Item);

   procedure setConstraintSolver (solver : access impact.d3.constraint_Solver.Item'Class);

   overriding function getConstraintSolver (Self : in Item) return access impact.d3.constraint_Solver.Item'Class;

   overriding function getWorldType (Self : in Item) return impact.d3.Space.dynamic.Kind;

   overriding procedure clearForces (Self : in out Item);

private

   type Item is abstract new impact.d3.Space.dynamic.Item with
      record
         m_constraintSolver  : access impact.d3.constraint_Solver.Item;
         m_ownsIslandManager : Boolean;
         m_gravity           : Vector_3;
      end record;

   procedure predictUnconstraintMotion (Self     : in out Item;
                                        timeStep : in     Real);

   procedure integrateTransforms (Self     : in out Item;
                                  timeStep : in     Real);


end impact.d3.Space.dynamic.simple;

--  #ifndef BT_SIMPLE_DYNAMICS_WORLD_H
--  #define BT_SIMPLE_DYNAMICS_WORLD_H

--  #include "impact.d3.Space.dynamic.h"

--  class impact.d3.Dispatcher;
--  class impact.d3.collision.overlapped_pair_Callback.cached;
--  class impact.d3.constraint_Solver;

--  ///The impact.d3.Space.dynamic.simple serves as unit-test and to verify more complicated and optimized dynamics worlds.
--  ///Please use impact.d3.Space.dynamic.discrete instead
--  class impact.d3.Space.dynamic.simple : public impact.d3.Space.dynamic
--  {
--  protected:

--          impact.d3.constraint_Solver*        m_constraintSolver;

--          bool        m_ownsConstraintSolver;

--          void        predictUnconstraintMotion(impact.d3.Scalar timeStep);

--          void        integrateTransforms(impact.d3.Scalar timeStep);

--          impact.d3.Vector        m_gravity;

--  public:



--          ///this impact.d3.Space.dynamic.simple constructor creates dispatcher, broadphase pairCache and constraintSolver
--          impact.d3.Space.dynamic.simple(impact.d3.Dispatcher* dispatcher,impact.d3.collision.Broadphase* pairCache,impact.d3.constraint_Solver* constraintSolver,impact.d3.collision.Configuration* collisionConfiguration);

--          virtual ~impact.d3.Space.dynamic.simple();

--          ///maxSubSteps/fixedTimeStep for interpolation is currently ignored for impact.d3.Space.dynamic.simple, use impact.d3.Space.dynamic.discrete instead
--          virtual int        stepSimulation( impact.d3.Scalar timeStep,int maxSubSteps=1, impact.d3.Scalar fixedTimeStep=impact.d3.Scalar(1.)/impact.d3.Scalar(60.));

--          virtual void        setGravity(const impact.d3.Vector& gravity);

--          virtual impact.d3.Vector getGravity () const;

--          virtual void        addRigidBody(impact.d3.Object.rigid* body);

--          virtual void        addRigidBody(impact.d3.Object.rigid* body, short group, short mask);

--          virtual void        removeRigidBody(impact.d3.Object.rigid* body);

--          virtual void        debugDrawWorld();

--          virtual void        addAction(impact.d3.Action* action);

--          virtual void        removeAction(impact.d3.Action* action);

--          ///removeCollisionObject will first check if it is a rigid body, if so call removeRigidBody otherwise call impact.d3.Space::removeCollisionObject
--          virtual void        removeCollisionObject(impact.d3.Object* collisionObject);

--          virtual void        updateAabbs();

--          virtual void        synchronizeMotionStates();

--          virtual void        setConstraintSolver(impact.d3.constraint_Solver* solver);

--          virtual impact.d3.constraint_Solver* getConstraintSolver();

--          virtual impact.d3.Space.dynamicType        getWorldType() const
--          {
--                  return BT_SIMPLE_DYNAMICS_WORLD;
--          }

--          virtual void        clearForces();

--  };

--  #endif //BT_SIMPLE_DYNAMICS_WORLD_H
