package body impact.d3.Space.dynamic.simple is

   overriding function stepSimulation (Self          : access Item;
                            timeStep      : in     Real;
                            maxSubSteps   : in     Integer := 1;
                            fixedTimeStep : in     Real := 1.0 / 60.0) return Integer
   is
      pragma Unreferenced (Self, timeStep, maxSubSteps, fixedTimeStep);
      Stub : Integer;
   begin
      return Stub;
   end stepSimulation;

   overriding procedure setGravity (Self    : in out Item;
                         gravity : in     Vector_3)
   is
   begin
      --  Self.m_gravity := gravity;
      --  for I in 1 .. Integer (Self.get_m_collisionObjects.Length) loop
      --     declare
      --        colObj   : impact.d3.Object_view := Self.m_get_collisionObject.Element (I).all'access;
      --        the_body : impact.d3.Object.rigid_view := impact.d3.Object.rigid_view (colObj);
      --     begin
      --        if the_body /= null then
      --           the_body.setGravity (gravity);
      --        end if;
      --     end;
      --  end loop;
      null;
   end setGravity;

   --  void        impact.d3.Space.dynamic.simple::setGravity(const impact.d3.Vector& gravity)
   --  {
   --          m_gravity = gravity;
   --          for ( int i=0;i<m_collisionObjects.size();i++)
   --          {
   --                  impact.d3.Object* colObj = m_collisionObjects[i];
   --                  impact.d3.Object.rigid* body = impact.d3.Object.rigid::upcast(colObj);
   --                  if (body)
   --                  {
   --                          body->setGravity(gravity);
   --                  }
   --          }
   --  }

   overriding function getGravity (Self : in Item) return Vector_3
   is
   begin
      return Self.m_gravity;
   end getGravity;

   --  impact.d3.Vector impact.d3.Space.dynamic.simple::getGravity () const
   --  {
   --          return m_gravity;
   --  }

   overriding procedure addRigidBody (Self     : in out Item;
                           the_body : access impact.d3.Object.rigid.Item'Class)
   is
   begin
      null;
   end addRigidBody;

   procedure addRigidBody (the_body    : access impact.d3.Object.rigid.Item'Class;
                           group, mask : in     Flags          )
   is
   begin
      null;
   end addRigidBody;

   procedure removeRigidBody (Self     : in out Item;
                              the_body : access impact.d3.Object.rigid.Item'Class)
   is
   begin
      Self.removeCollisionObject (the_body);
   end removeRigidBody;

   --  void        impact.d3.Space.dynamic.simple::removeRigidBody(impact.d3.Object.rigid* body)
   --  {
   --          impact.d3.Space::removeCollisionObject(body);
   --  }

   procedure debugDrawWorld (Self : in out Item)
   is
   begin
      null;
   end debugDrawWorld;

   --  void        impact.d3.Space.dynamic.simple::debugDrawWorld()
   --  {

   --  }

   --- tbd: leave these for the time being (rak) ...
   --
   --     procedure addAction (Self   : in out Item;
   --                          action : access impact.d3.Action.Item'Class) is begin null; end addAction;
   --  void        impact.d3.Space.dynamic.simple::addAction(impact.d3.Action* action)
   --  {

   --  }
   --
   --     procedure removeAction (Self   : in out Item;
   --                             action : access impact.d3.Action.Item'Class) is begin null; end removeAction;
   --  void        impact.d3.Space.dynamic.simple::removeAction(impact.d3.Action* action)
   --  {

   --  }

   procedure removeCollisionObject (Self            : in out Item;
                                    collisionObject : access impact.d3.Object.view)
   is
      --  the_body : impact.d3.Object.rigid_view := impact.d3.Object.rigid_view (collisionObject);
   begin
      --  if the_body /= null then
      --     Self.removeRigidBody (the_body);
      --  else
      --     Self.removeCollisionObject (collisionObject);
      --  end if;
      null;
   end removeCollisionObject;

   --  void        impact.d3.Space.dynamic.simple::removeCollisionObject(impact.d3.Object* collisionObject)
   --  {
   --          impact.d3.Object.rigid* body = impact.d3.Object.rigid::upcast(collisionObject);
   --          if (body)
   --                  removeRigidBody(body);
   --          else
   --                  impact.d3.Space::removeCollisionObject(collisionObject);
   --  }

   overriding procedure updateAabbs (Self : in out Item)
   is
   begin
      null;
   end updateAabbs;

   overriding procedure synchronizeMotionStates (Self : in out Item)
   is
   begin
      null;
   end synchronizeMotionStates;

   procedure setConstraintSolver (solver : access impact.d3.constraint_Solver.Item'Class)
   is
   begin
      null;
   end setConstraintSolver;

   overriding function getConstraintSolver (Self : in Item) return access impact.d3.constraint_Solver.Item'Class
   is
      pragma Unreferenced (Self);
      Stub : impact.d3.constraint_Solver.view;
   begin
      return Stub;
   end getConstraintSolver;

   overriding function getWorldType (Self : in Item) return impact.d3.Space.dynamic.Kind
   is
      pragma Unreferenced (Self);
      Stub : impact.d3.Space.dynamic.Kind;
   begin
      return Stub;
   end getWorldType;

   overriding procedure clearForces (Self : in out Item)
   is
   begin
      null;
   end clearForces;

   procedure predictUnconstraintMotion (Self     : in out Item;
                                        timeStep : in     Real)
   is
   begin
      null;
   end predictUnconstraintMotion;

   procedure integrateTransforms (Self     : in out Item;
                                  timeStep : in     Real)
   is
   begin
      null;
   end integrateTransforms;

end impact.d3.Space.dynamic.simple;

--  #include "impact.d3.Space.dynamic.simple.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.Dispatcher.collision.h"
--  #include "BulletCollision/BroadphaseCollision/btSimpleBroadphase.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.h"
--  #include "BulletDynamics/Dynamics/impact.d3.Object.rigid.h"
--  #include "BulletDynamics/ConstraintSolver/impact.d3.constraint_Solver.sequential_impulse.h"
--  #include "BulletDynamics/ConstraintSolver/impact.d3.contact_solver_Info.h"


--  /*
--    Make sure this dummy function never changes so that it
--    can be used by probes that are checking whether the
--    library is actually installed.
--  */
--  extern "C"
--  {
--          void btBulletDynamicsProbe ();
--          void btBulletDynamicsProbe () {}
--  }




--  impact.d3.Space.dynamic.simple::impact.d3.Space.dynamic.simple(impact.d3.Dispatcher* dispatcher,impact.d3.collision.Broadphase* pairCache,impact.d3.constraint_Solver* constraintSolver,impact.d3.collision.Configuration* collisionConfiguration)
--  :impact.d3.Space.dynamic(dispatcher,pairCache,collisionConfiguration),
--  m_constraintSolver(constraintSolver),
--  m_ownsConstraintSolver(false),
--  m_gravity(0,0,-10)
--  {

--  }


--  impact.d3.Space.dynamic.simple::~impact.d3.Space.dynamic.simple()
--  {
--          if (m_ownsConstraintSolver)
--                  btAlignedFree( m_constraintSolver);
--  }

--  int                impact.d3.Space.dynamic.simple::stepSimulation( impact.d3.Scalar timeStep,int maxSubSteps, impact.d3.Scalar fixedTimeStep)
--  {
--          (void)fixedTimeStep;
--          (void)maxSubSteps;


--          ///apply gravity, predict motion
--          predictUnconstraintMotion(timeStep);

--          impact.d3.DispatcherInfo&        dispatchInfo = getDispatchInfo();
--          dispatchInfo.m_timeStep = timeStep;
--          dispatchInfo.m_stepCount = 0;
--          dispatchInfo.m_debugDraw = getDebugDrawer();

--          ///perform collision detection
--          performDiscreteCollisionDetection();

--          ///solve contact constraints
--          int numManifolds = m_dispatcher1->getNumManifolds();
--          if (numManifolds)
--          {
--                  impact.d3.Manifold** manifoldPtr = ((impact.d3.Dispatcher.collision*)m_dispatcher1)->getInternalManifoldPointer();

--                  impact.d3.contact_solver_Info infoGlobal;
--                  infoGlobal.m_timeStep = timeStep;
--                  m_constraintSolver->prepareSolve(0,numManifolds);
--                  m_constraintSolver->solveGroup(&getCollisionObjectArray()[0],getNumCollisionObjects(),manifoldPtr, numManifolds,0,0,infoGlobal,m_debugDrawer, m_stackAlloc,m_dispatcher1);
--                  m_constraintSolver->allSolved(infoGlobal,m_debugDrawer, m_stackAlloc);
--          }

--          ///integrate transforms
--          integrateTransforms(timeStep);

--          updateAabbs();

--          synchronizeMotionStates();

--          clearForces();

--          return 1;

--  }

--  void        impact.d3.Space.dynamic.simple::clearForces()
--  {
--          ///@todo: iterate over awake simulation islands!
--          for ( int i=0;i<m_collisionObjects.size();i++)
--          {
--                  impact.d3.Object* colObj = m_collisionObjects[i];

--                  impact.d3.Object.rigid* body = impact.d3.Object.rigid::upcast(colObj);
--                  if (body)
--                  {
--                          body->clearForces();
--                  }
--          }
--  }











--  void        impact.d3.Space.dynamic.simple::addRigidBody(impact.d3.Object.rigid* body)
--  {
--          body->setGravity(m_gravity);

--          if (body->getCollisionShape())
--          {
--                  addCollisionObject(body);
--          }
--  }

--  void        impact.d3.Space.dynamic.simple::addRigidBody(impact.d3.Object.rigid* body, short group, short mask)
--  {
--          body->setGravity(m_gravity);

--          if (body->getCollisionShape())
--          {
--                  addCollisionObject(body,group,mask);
--          }
--  }









--  void        impact.d3.Space.dynamic.simple::updateAabbs()
--  {
--          impact.d3.Transform predictedTrans;
--          for ( int i=0;i<m_collisionObjects.size();i++)
--          {
--                  impact.d3.Object* colObj = m_collisionObjects[i];
--                  impact.d3.Object.rigid* body = impact.d3.Object.rigid::upcast(colObj);
--                  if (body)
--                  {
--                          if (body->isActive() && (!body->isStaticObject()))
--                          {
--                                  impact.d3.Vector minAabb,maxAabb;
--                                  colObj->getCollisionShape()->getAabb(colObj->getWorldTransform(), minAabb,maxAabb);
--                                  impact.d3.collision.Broadphase* bp = getBroadphase();
--                                  bp->setAabb(body->getBroadphaseHandle(),minAabb,maxAabb, m_dispatcher1);
--                          }
--                  }
--          }
--  }

--  void        impact.d3.Space.dynamic.simple::integrateTransforms(impact.d3.Scalar timeStep)
--  {
--          impact.d3.Transform predictedTrans;
--          for ( int i=0;i<m_collisionObjects.size();i++)
--          {
--                  impact.d3.Object* colObj = m_collisionObjects[i];
--                  impact.d3.Object.rigid* body = impact.d3.Object.rigid::upcast(colObj);
--                  if (body)
--                  {
--                          if (body->isActive() && (!body->isStaticObject()))
--                          {
--                                  body->predictIntegratedTransform(timeStep, predictedTrans);
--                                  body->proceedToTransform( predictedTrans);
--                          }
--                  }
--          }
--  }



--  void        impact.d3.Space.dynamic.simple::predictUnconstraintMotion(impact.d3.Scalar timeStep)
--  {
--          for ( int i=0;i<m_collisionObjects.size();i++)
--          {
--                  impact.d3.Object* colObj = m_collisionObjects[i];
--                  impact.d3.Object.rigid* body = impact.d3.Object.rigid::upcast(colObj);
--                  if (body)
--                  {
--                          if (!body->isStaticObject())
--                          {
--                                  if (body->isActive())
--                                  {
--                                          body->applyGravity();
--                                          body->integrateVelocities( timeStep);
--                                          body->applyDamping(timeStep);
--                                          body->predictIntegratedTransform(timeStep,body->getInterpolationWorldTransform());
--                                  }
--                          }
--                  }
--          }
--  }


--  void        impact.d3.Space.dynamic.simple::synchronizeMotionStates()
--  {
--          ///@todo: iterate over awake simulation islands!
--          for ( int i=0;i<m_collisionObjects.size();i++)
--          {
--                  impact.d3.Object* colObj = m_collisionObjects[i];
--                  impact.d3.Object.rigid* body = impact.d3.Object.rigid::upcast(colObj);
--                  if (body && body->getMotionState())
--                  {
--                          if (body->getActivationState() != ISLAND_SLEEPING)
--                          {
--                                  body->getMotionState()->setWorldTransform(body->getWorldTransform());
--                          }
--                  }
--          }

--  }


--  void        impact.d3.Space.dynamic.simple::setConstraintSolver(impact.d3.constraint_Solver* solver)
--  {
--          if (m_ownsConstraintSolver)
--          {
--                  btAlignedFree(m_constraintSolver);
--          }
--          m_ownsConstraintSolver = false;
--          m_constraintSolver = solver;
--  }

--  impact.d3.constraint_Solver* impact.d3.Space.dynamic.simple::getConstraintSolver()
--  {
--          return m_constraintSolver;
--  }
