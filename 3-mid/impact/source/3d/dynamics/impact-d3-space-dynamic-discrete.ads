with ada.Containers.Vectors;
with impact.d3.Space.dynamic;
with impact.d3.constraint_Solver;
with impact.d3.Joint;
with impact.d3.Action;
with impact.d3.simulation_island_Manager;
with impact.d3.Space;
with impact.d3.Object;
with impact.d3.collision.Proxy;
with impact.d3.Object.rigid;
--  with btSerializer;
with impact.d3.contact_solver_Info;

use  impact.d3.collision.Proxy;
with impact.d3.Dispatcher;
with impact.d3.Manifold;
with impact.d3.collision.Broadphase;
with impact.d3.collision.Configuration;



package impact.d3.Space.dynamic.discrete
--
--
--
is


   type Item is new impact.d3.Space.dynamic.item with private;

   type rigid_Object_view                is access all impact.d3.Object.rigid.item'Class;
   type       Object_view                is access all impact.d3.Object.item'Class;
   type ClosestConvexResultCallback_view is access all impact.d3.Space.ClosestConvexResultCallback'Class;




   --- Forge
   --


   package Forge
   is
      function to_Space (dispatcher             : access impact.d3.Dispatcher.item'Class;
                         pairCache              : access impact.d3.collision.Broadphase.Item'Class;
                         constraintSolver       : in     impact.d3.constraint_Solver.view;
                         CollisionConfiguration : access impact.d3.collision.Configuration.Item'Class) return Item;
   end Forge;



   overriding procedure destruct (Self : in out Item);







   --- Attributes
   --

   overriding function stepSimulation (Self          : access Item;
                            timeStep      : in Real;
                            maxSubSteps   : in Integer := 1;
                            fixedTimeStep : in Real := 1.0/60.0) return Integer;

   procedure synchronizeSingleMotionState (Self     : in out Item;
                                           the_body : access impact.d3.Object.rigid.Item'Class);

   overriding procedure synchronizeMotionStates (Self : in out Item);


   overriding procedure addConstraint (Self       : in out Item;
                            constraint : access impact.d3.Joint.Item'Class;
                            disableCollisionsBetweenLinkedBodies : Boolean := False);

   overriding procedure removeConstraint (Self       : in out Item;
                               constraint : access impact.d3.Joint.Item'Class);

   procedure addAction (Self             : in out Item;
                        Action_Interface : access impact.d3.Action.Item'Class);

   procedure removeAction (Self             : in out Item;
                           Action_Interface : access impact.d3.Action.Item'Class);

   function getSimulationIslandManager (Self : in Item) return access impact.d3.simulation_island_Manager.Item'Class;

--     function getCollisionWorld (Self : access Item) return impact.d3.Space.view;

   overriding procedure setGravity (Self    : in out Item;
                         gravity : in Vector_3);

   overriding function getGravity (Self : in Item) return Vector_3;

--     procedure addCollisionObject (Self                 : in out Item;
--                                   collisionObject      : access impact.d3.Object.impact.d3.Object'Class;
--                                   collisionFilterGroup : in CollisionFilterGroups := StaticFilter;
--                                   collisionFilterMask  : in CollisionFilterGroups := AllFilter xor StaticFilter);

   overriding procedure addRigidBody (Self     : in out Item;
                           The_Body : access impact.d3.Object.rigid.Item'Class);

   procedure addRigidBody (Self     : in out Item;
                           the_body : access impact.d3.Object.rigid.Item'Class;
                           group    : in CollisionFilterGroups;
                           mask     : in CollisionFilterGroups                  );

   procedure removeRigidBody (Self     : in out Item;
                              the_body : access impact.d3.Object.rigid.Item'Class);

   overriding procedure removeCollisionObject (Self            : in out Item;
                                    collisionObject : access impact.d3.Object.item'Class); -- Doubtfull

   --  procedure debugDrawConstraint (Self       : in Item;
   --                                 constraint : access impact.d3.Joint.Item'Class);

   --  procedure debugDrawWorld (Self : in Item);

   procedure setConstraintSolver (Self   : in out Item;
                                  solver : access impact.d3.constraint_Solver.Item'Class);

   overriding function getConstraintSolver (Self : in Item) return access impact.d3.constraint_Solver.Item'Class;

   overriding function getNumConstraints (Self : in Item) return Integer;

   overriding function getConstraint (Self  : in Item;
                           index : in Integer) return access impact.d3.Joint.Item'Class;

   overriding function getWorldType (Self : in Item) return impact.d3.Space.dynamic.Kind;

   overriding procedure clearForces (Self : in out Item);

   procedure applyGravity (Self : in out Item);

   procedure setNumTasks (Self     : in Item;
                          numTasks : in Integer) is null;

   procedure setSynchronizeAllMotionStates (Self           : in out Item;
                                            synchronizeAll : in     Boolean);

   function getSynchronizeAllMotionStates (Self : in Item) return Boolean;

--     procedure serialize (Self       : in Item;
--                          serializer : access btSerializer.Item'Class);

--     procedure dummy;





private

   use type impact.d3.Object.rigid.view;
   package rigid_object_Vectors is new ada.Containers.Vectors (Positive, impact.d3.Object.rigid.view);
   subtype rigid_object_Vector  is     rigid_object_Vectors.Vector;


   type Item is new impact.d3.Space.dynamic.item with
      record
         m_constraintSolver     : impact.d3.constraint_Solver.view;
         m_islandManager        : impact.d3.simulation_island_Manager.view;
         m_constraints          : impact.d3.Joint.Vector;
         m_nonStaticRigidBodies : rigid_object_Vector; -- Doubtfull
         m_gravity              : Vector_3;
         m_localTime            : Real;
         m_ownsIslandManager    : Boolean;
         m_ownsConstraintSolver : Boolean;
         m_synchronizeAllMotionStates : Boolean;
         --  m_actions : impact.d3.Action_Vector; -- Doubtfull
         m_profileTimings       : Integer;
      end record;

   procedure predictUnconstraintMotion (Self     : in out Item;
                                        timeStep : in Real);

   procedure integrateTransforms (Self     : in out Item;
                                  timeStep : in Real);

   procedure calculateSimulationIslands (Self : access Item);

   procedure solveConstraints (Self       : access Item;
                               solverInfo : access impact.d3.contact_solver_Info.Item'Class);

   procedure updateActivationState (Self     : in Item;
                                    timeStep : in Real);

   procedure updateActions (Self     : in out Item;
                            timeStep : in Real);

   procedure startProfiling (Self     : in Item;
                             timeStep : in Real);

   procedure internalSingleStepSimulation (Self     : access item;
                                           timeStep : in Real);

   procedure saveKinematicState (Self     : in out Item;
                                 timeStep : in Real);

   --  procedure serializeRigidBodies (Self       : in Item;
   --                                  serializer : access btSerializer.Item'Class);







   type InplaceSolverIslandCallback is new impact.d3.simulation_island_Manager.IslandCallback with
      record
         m_solverInfo        : access impact.d3.contact_solver_Info.Item'Class;
         m_solver            : access impact.d3.constraint_Solver.Item'Class;

         m_sortedConstraints : access impact.d3.Joint.Vector;
--           m_numConstraints    :        Integer;

         m_dispatcher        : access impact.d3.Dispatcher.item'Class;

         m_bodies            : aliased impact.d3.Object.Vector;
         m_manifolds         : aliased impact.d3.Manifold.Vector;
         m_constraints       : aliased impact.d3.Joint.Vector;
      end record;


   overriding
   procedure ProcessIsland (Self      : access InplaceSolverIslandCallback;
                            bodies    : access impact.d3.Object.Vector;
                            manifolds : access impact.d3.Manifold.Vector;
                            islandId  : in Integer                  );

   procedure processConstraints (Self : in out InplaceSolverIslandCallback);


end impact.d3.Space.dynamic.discrete;



--  #ifndef BT_DISCRETE_DYNAMICS_WORLD_H
--  #define BT_DISCRETE_DYNAMICS_WORLD_H
--
--  #include "impact.d3.Space.dynamic.h"
--
--  class impact.d3.Dispatcher;
--  class impact.d3.collision.overlapped_pair_Callback.cached;
--  class impact.d3.constraint_Solver;
--  class impact.d3.simulation_island_Manager;
--  class impact.d3.Joint;
--  class impact.d3.Action;
--
--  class btIDebugDraw;
--  #include "LinearMath/btAlignedObjectArray.h"
--
--
--  ///impact.d3.Space.dynamic.discrete provides discrete rigid body simulation
--  ///those classes replace the obsolete CcdPhysicsEnvironment/CcdPhysicsController
--  class impact.d3.Space.dynamic.discrete : public impact.d3.Space.dynamic
--  {
--  protected:
--
--          impact.d3.constraint_Solver*        m_constraintSolver;
--
--          impact.d3.simulation_island_Manager*        m_islandManager;
--
--          btAlignedObjectArray<impact.d3.Joint*> m_constraints;
--
--          btAlignedObjectArray<impact.d3.Object.rigid*> m_nonStaticRigidBodies;
--
--          impact.d3.Vector        m_gravity;
--
--          //for variable timesteps
--          impact.d3.Scalar        m_localTime;
--          //for variable timesteps
--
--          bool        m_ownsIslandManager;
--          bool        m_ownsConstraintSolver;
--          bool        m_synchronizeAllMotionStates;
--
--          btAlignedObjectArray<impact.d3.Action*>        m_actions;
--
--          int        m_profileTimings;
--
--          virtual void        predictUnconstraintMotion(impact.d3.Scalar timeStep);
--
--          virtual void        integrateTransforms(impact.d3.Scalar timeStep);
--
--          virtual void        calculateSimulationIslands();
--
--          virtual void        solveConstraints(impact.d3.contact_solver_Info& solverInfo);
--
--          void        updateActivationState(impact.d3.Scalar timeStep);
--
--          void        updateActions(impact.d3.Scalar timeStep);
--
--          void        startProfiling(impact.d3.Scalar timeStep);
--
--          virtual void        internalSingleStepSimulation( impact.d3.Scalar timeStep);
--
--
--          virtual void        saveKinematicState(impact.d3.Scalar timeStep);
--
--          void        serializeRigidBodies(btSerializer* serializer);
--
--  public:
--
--
--          ///this impact.d3.Space.dynamic.discrete constructor gets created objects from the user, and will not delete those
--          impact.d3.Space.dynamic.discrete(impact.d3.Dispatcher* dispatcher,impact.d3.collision.Broadphase* pairCache,impact.d3.constraint_Solver* constraintSolver,impact.d3.collision.Configuration* collisionConfiguration);
--
--          virtual ~impact.d3.Space.dynamic.discrete();
--
--          ///if maxSubSteps > 0, it will interpolate motion between fixedTimeStep's
--          virtual int        stepSimulation( impact.d3.Scalar timeStep,int maxSubSteps=1, impact.d3.Scalar fixedTimeStep=impact.d3.Scalar(1.)/impact.d3.Scalar(60.));
--
--
--          virtual void        synchronizeMotionStates();
--
--          ///this can be useful to synchronize a single rigid body -> graphics object
--          void        synchronizeSingleMotionState(impact.d3.Object.rigid* body);
--
--          virtual void        addConstraint(impact.d3.Joint* constraint, bool disableCollisionsBetweenLinkedBodies=false);
--
--          virtual void        removeConstraint(impact.d3.Joint* constraint);
--
--          virtual void        addAction(impact.d3.Action*);
--
--          virtual void        removeAction(impact.d3.Action*);
--
--          impact.d3.simulation_island_Manager*        getSimulationIslandManager()
--          {
--                  return m_islandManager;
--          }
--
--          const impact.d3.simulation_island_Manager*        getSimulationIslandManager() const
--          {
--                  return m_islandManager;
--          }
--
--          impact.d3.Space*        getCollisionWorld()
--          {
--                  return this;
--          }
--
--          virtual void        setGravity(const impact.d3.Vector& gravity);
--
--          virtual impact.d3.Vector getGravity () const;
--
--          virtual void        addCollisionObject(impact.d3.Object* collisionObject,short int collisionFilterGroup=impact.d3.collision.Proxy::StaticFilter,short int collisionFilterMask=impact.d3.collision.Proxy::AllFilter ^ impact.d3.collision.Proxy::StaticFilter);
--
--          virtual void        addRigidBody(impact.d3.Object.rigid* body);
--
--          virtual void        addRigidBody(impact.d3.Object.rigid* body, short group, short mask);
--
--          virtual void        removeRigidBody(impact.d3.Object.rigid* body);
--
--          ///removeCollisionObject will first check if it is a rigid body, if so call removeRigidBody otherwise call impact.d3.Space::removeCollisionObject
--          virtual void        removeCollisionObject(impact.d3.Object* collisionObject);
--
--
--          void        debugDrawConstraint(impact.d3.Joint* constraint);
--
--          virtual void        debugDrawWorld();
--
--          virtual void        setConstraintSolver(impact.d3.constraint_Solver* solver);
--
--          virtual impact.d3.constraint_Solver* getConstraintSolver();
--
--          virtual        int                getNumConstraints() const;
--
--          virtual impact.d3.Joint* getConstraint(int index)        ;
--
--          virtual const impact.d3.Joint* getConstraint(int index) const;
--
--
--          virtual impact.d3.Space.dynamicType        getWorldType() const
--          {
--                  return BT_DISCRETE_DYNAMICS_WORLD;
--          }
--
--          ///the forces on each rigidbody is accumulating together with gravity. clear this after each timestep.
--          virtual void        clearForces();
--
--          ///apply gravity, call this once per timestep
--          virtual void        applyGravity();
--
--          virtual void        setNumTasks(int numTasks)
--          {
--          (void) numTasks;
--          }
--
--          ///obsolete, use updateActions instead
--          virtual void updateVehicles(impact.d3.Scalar timeStep)
--          {
--                  updateActions(timeStep);
--          }
--
--          ///obsolete, use addAction instead
--          virtual void        addVehicle(impact.d3.Action* vehicle);
--          ///obsolete, use removeAction instead
--          virtual void        removeVehicle(impact.d3.Action* vehicle);
--          ///obsolete, use addAction instead
--          virtual void        addCharacter(impact.d3.Action* character);
--          ///obsolete, use removeAction instead
--          virtual void        removeCharacter(impact.d3.Action* character);
--
--          void        setSynchronizeAllMotionStates(bool synchronizeAll)
--          {
--                  m_synchronizeAllMotionStates = synchronizeAll;
--          }
--          bool getSynchronizeAllMotionStates() const
--          {
--                  return m_synchronizeAllMotionStates;
--          }
--
--          ///Preliminary serialization test for Bullet 2.76. Loading those files requires a separate parser (see Bullet/Demos/SerializeDemo)
--          virtual        void        serialize(btSerializer* serializer);
--
--  };
--
--  #endif //BT_DISCRETE_DYNAMICS_WORLD_H
