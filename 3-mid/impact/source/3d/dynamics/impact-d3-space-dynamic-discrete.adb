with impact.d3.Transform_Util,
     impact.d3.Scalar;
with ada.Unchecked_Deallocation;
with impact.d3.collision.overlapped_pair_Callback.cached;
with impact.d3.Vector;
with impact.d3.Transform;
with impact.d3.Shape.convex.internal.sphere;
with impact.d3.Joint.contact;
with impact.d3.constraint_Solver.sequential_impulse;
with impact.d3.Object;




--  #include "impact.d3.Space.dynamic.discrete.h"
--
--  //collision detection
--  #include "BulletCollision/CollisionDispatch/impact.d3.Dispatcher.collision.h"
--  #include "BulletCollision/BroadphaseCollision/btSimpleBroadphase.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.Algorithm.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.simulation_island_Manager.h"
--  #include "LinearMath/impact.d3.TransformUtil.h"
--  #include "LinearMath/btQuickprof.h"
--
--  //rigidbody & constraints
--  #include "BulletDynamics/Dynamics/impact.d3.Object.rigid.h"
--  #include "BulletDynamics/ConstraintSolver/impact.d3.constraint_Solver.sequential_impulse.h"
--  #include "BulletDynamics/ConstraintSolver/impact.d3.contact_solver_Info.h"
--  #include "BulletDynamics/ConstraintSolver/impact.d3.Joint.h"
--  #include "BulletDynamics/ConstraintSolver/btPoint2PointConstraint.h"
--  #include "BulletDynamics/ConstraintSolver/impact.d3.Joint.hinge.h"
--  #include "BulletDynamics/ConstraintSolver/btConeTwistConstraint.h"
--  #include "BulletDynamics/ConstraintSolver/impact.d3.Joint.any.h"
--  #include "BulletDynamics/ConstraintSolver/btSliderConstraint.h"
--  #include "BulletDynamics/ConstraintSolver/impact.d3.Joint.contact.h"
--
--
--  #include "LinearMath/btIDebugDraw.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.internal.sphere.h"
--
--
--  #include "BulletDynamics/Dynamics/impact.d3.Action.h"
--  #include "LinearMath/btQuickprof.h"
--  #include "LinearMath/impact.d3.motion_State.h"
--
--  #include "LinearMath/btSerializer.h"



package body impact.d3.Space.dynamic.discrete
is




   --- Forge
   --


   package body Forge
   is

      function to_Space (dispatcher             : access impact.d3.Dispatcher.item'Class;
                         pairCache              : access impact.d3.collision.Broadphase.Item'Class;
                         constraintSolver       : in     impact.d3.constraint_Solver.view;
                         CollisionConfiguration : access impact.d3.collision.Configuration.Item'Class) return Item
      is
         use type impact.d3.constraint_Solver.view;
      begin
         return Self : Item
         do
            impact.d3.Space.dynamic.Forge.define (impact.d3.Space.dynamic.item (Self),  dispatcher, pairCache, collisionConfiguration);

            Self.m_constraintSolver           := constraintSolver;
            Self.m_gravity                    := (0.0, -10.0, 0.0);
            Self.m_localTime                  := 0.0;
            Self.m_synchronizeAllMotionStates := False;
            Self.m_profileTimings             := 0;


            if Self.m_constraintSolver = null then
               Self.m_constraintSolver     := new impact.d3.constraint_Solver.sequential_impulse.item;
               Self.m_ownsConstraintSolver := True;
            else
               Self.m_ownsConstraintSolver := False;
            end if;

            Self.m_islandManager     := new impact.d3.simulation_island_Manager.item;
            Self.m_ownsIslandManager := True;
         end return;
      end to_Space;

   end Forge;





   overriding procedure destruct (Self : in out Item)
   is
      procedure free is new ada.Unchecked_Deallocation (impact.d3.simulation_island_Manager.item'Class, impact.d3.simulation_island_Manager.view);
      procedure free is new ada.Unchecked_Deallocation (impact.d3.constraint_Solver.item'Class, impact.d3.constraint_Solver.view);

   begin
      if Self.m_ownsIslandManager then   -- only delete it when we created it
         Self.m_islandManager.destruct;
         free (Self.m_islandManager);
      end if;

      if Self.m_ownsConstraintSolver then
         Self.m_constraintSolver.destruct;
         free (Self.m_constraintSolver);
      end if;
   end destruct;








   --- Attributes
   --

   overriding function stepSimulation (Self          : access Item;
                            timeStep      : in Real;
                            maxSubSteps   : in Integer := 1;
                            fixedTimeStep : in Real := 1.0/60.0) return Integer
   is
      use impact.d3.Scalar;

      numSimulationSubSteps : Integer := 0;
      the_fixedTimeStep     : Real := fixedTimeStep;
      the_maxSubSteps       : Integer := maxSubSteps;
      pragma Unreferenced (the_maxSubSteps);

   begin
      if maxSubSteps /= 0 then
         Self.m_localTime := Self.m_localTime + the_fixedTimeStep;

         if Self.m_localTime >= the_fixedTimeStep then
            numSimulationSubSteps := Integer (Self.m_localTime / the_fixedTimeStep);
            Self.m_localTime      := Self.m_localTime  -  Real (numSimulationSubSteps) * the_fixedTimeStep;
         end if;

      else
         the_fixedTimeStep := timeStep;
         Self.m_localTime := timeStep;

         if btFuzzyZero (timeStep) then
            numSimulationSubSteps := 0;
            the_maxSubSteps       := 0;
         else
            numSimulationSubSteps := 1;
            the_maxSubSteps       := 1;
         end if;

      end if;

      --  Skipping debug

      if numSimulationSubSteps /= 0 then
         declare
            clampedSimulationSteps : constant Integer := (if numSimulationSubSteps > maxSubSteps then maxSubSteps else numSimulationSubSteps);
         begin
            Self.saveKinematicState (the_fixedTimeStep * Real (clampedSimulationSteps));
            Self.applyGravity;

            for I in 1 .. clampedSimulationSteps loop
               Self.internalSingleStepSimulation (the_fixedTimeStep);
               Self.synchronizeMotionStates;
            end loop;
         end;
      else
         Self.synchronizeMotionStates;
      end if;

      Self.clearForces;
      return numSimulationSubSteps;
   end stepSimulation;

   --  int        impact.d3.Space.dynamic.discrete::stepSimulation( impact.d3.Scalar timeStep,int maxSubSteps, impact.d3.Scalar fixedTimeStep)
   --  {
   --          startProfiling(timeStep);
   --
   --          BT_PROFILE("stepSimulation");
   --
   --          int numSimulationSubSteps = 0;
   --
   --          if (maxSubSteps)
   --          {
   --                  //fixed timestep with interpolation
   --                  m_localTime += timeStep;
   --                  if (m_localTime >= fixedTimeStep)
   --                  {
   --                          numSimulationSubSteps = int( m_localTime / fixedTimeStep);
   --                          m_localTime -= numSimulationSubSteps * fixedTimeStep;
   --                  }
   --          } else
   --          {
   --                  //variable timestep
   --                  fixedTimeStep = timeStep;
   --                  m_localTime = timeStep;
   --                  if (btFuzzyZero(timeStep))
   --                  {
   --                          numSimulationSubSteps = 0;
   --                          maxSubSteps = 0;
   --                  } else
   --                  {
   --                          numSimulationSubSteps = 1;
   --                          maxSubSteps = 1;
   --                  }
   --          }
   --
   --          //process some debugging flags
   --          if (getDebugDrawer())
   --          {
   --                  btIDebugDraw* debugDrawer = getDebugDrawer ();
   --                  gDisableDeactivation = (debugDrawer->getDebugMode() & btIDebugDraw::DBG_NoDeactivation) != 0;
   --          }
   --          if (numSimulationSubSteps)
   --          {
   --
   --                  //clamp the number of substeps, to prevent simulation grinding spiralling down to a halt
   --                  int clampedSimulationSteps = (numSimulationSubSteps > maxSubSteps)? maxSubSteps : numSimulationSubSteps;
   --
   --                  saveKinematicState(fixedTimeStep*clampedSimulationSteps);
   --
   --                  applyGravity();
   --
   --
   --
   --                  for (int i=0;i<clampedSimulationSteps;i++)
   --                  {
   --                          internalSingleStepSimulation(fixedTimeStep);
   --                          synchronizeMotionStates();
   --                  }
   --
   --          } else
   --          {
   --                  synchronizeMotionStates();
   --          }
   --
   --          clearForces();
   --
   --  #ifndef BT_NO_PROFILE
   --          CProfileManager::Increment_Frame_Counter();
   --  #endif //BT_NO_PROFILE
   --
   --          return numSimulationSubSteps;
   --  }

   procedure synchronizeSingleMotionState (Self     : in out Item;
                                           the_body : access impact.d3.Object.rigid.Item'Class)
   is
   begin
      if the_body.getMotionState /= null and not the_body.isStaticOrKinematicObject then
         declare
            interpolatedTransform : Transform_3d;
         begin
            impact.d3.transform_Util.integrateTransform (the_body.getInterpolationWorldTransform,
                                                the_body.getInterpolationLinearVelocity,
                                                the_body.getInterpolationAngularVelocity,
                                                Self.m_localTime * the_body.getHitFraction,
                                                interpolatedTransform                    );
            the_body.getMotionState.setWorldTransform (interpolatedTransform);
         end;
      end if;
   end synchronizeSingleMotionState;

   --  void        impact.d3.Space.dynamic.discrete::synchronizeSingleMotionState(impact.d3.Object.rigid* body)
   --  {
   --          btAssert(body);
   --
   --          if (body->getMotionState() && !body->isStaticOrKinematicObject())
   --          {
   --                  //we need to call the update at least once, even for sleeping objects
   --                  //otherwise the 'graphics' transform never updates properly
   --                  ///@todo: add 'dirty' flag
   --                  //if (body->getActivationState() != ISLAND_SLEEPING)
   --                  {
   --                          impact.d3.Transform interpolatedTransform;
   --                          impact.d3.TransformUtil::integrateTransform(body->getInterpolationWorldTransform(),
   --                                  body->getInterpolationLinearVelocity(),body->getInterpolationAngularVelocity(),m_localTime*body->getHitFraction(),interpolatedTransform);
   --                          body->getMotionState()->setWorldTransform(interpolatedTransform);
   --                  }
   --          }
   --  }

   overriding procedure synchronizeMotionStates (Self : in out Item)
   is
   begin
      if Self.m_synchronizeAllMotionStates then

         for I in 1 .. Integer (Self.get_m_collisionObjects.Length) loop
            declare
               colObj   : constant impact.d3.Object.view := Self.get_m_collisionObjects.Element (I).all'Access;
               the_body : constant impact.d3.Object.rigid.view       := impact.d3.Object.rigid.view (colObj);
            begin
               if the_body /= null then
                  Self.synchronizeSingleMotionState (the_body);
               end if;
            end;
         end loop;

      else

         for I in 1 .. Integer (Self.m_nonStaticRigidBodies.Length) loop
            declare
               the_body : constant access impact.d3.Object.rigid.Item := Self.m_nonStaticRigidBodies.Element (I);
            begin
               if the_body.isActive then
                  Self.synchronizeSingleMotionState (the_body);
               end if;
            end;
         end loop;

      end if;
   end synchronizeMotionStates;

   --  void        impact.d3.Space.dynamic.discrete::synchronizeMotionStates()
   --  {
   --          BT_PROFILE("synchronizeMotionStates");
   --          if (m_synchronizeAllMotionStates)
   --          {
   --                  //iterate  over all collision objects
   --                  for ( int i=0;i<m_collisionObjects.size();i++)
   --                  {
   --                          impact.d3.Object* colObj = m_collisionObjects[i];
   --                          impact.d3.Object.rigid* body = impact.d3.Object.rigid::upcast(colObj);
   --                          if (body)
   --                                  synchronizeSingleMotionState(body);
   --                  }
   --          } else
   --          {
   --                  //iterate over all active rigid bodies
   --                  for ( int i=0;i<m_nonStaticRigidBodies.size();i++)
   --                  {
   --                          impact.d3.Object.rigid* body = m_nonStaticRigidBodies[i];
   --                          if (body->isActive())
   --                                  synchronizeSingleMotionState(body);
   --                  }
   --          }
   --  }

   overriding procedure addConstraint (Self       : in out Item;
                            constraint : access impact.d3.Joint.Item'Class;
                            disableCollisionsBetweenLinkedBodies : Boolean := False)
   is
   begin
      raise Program_Error; -- tbd

--        Self.m_constraints.push_back (constraint);
--
--        if disableCollisionsBetweenLinkedBodies then
--           constraint.getRigidBodyA.addConstraintRef (Self, constraint);
--           constraint.getRigidBodyB.addConstraintRef (Self, constraint);
--        end if;
   end addConstraint;

   --  void        impact.d3.Space.dynamic.discrete::addConstraint(impact.d3.Joint* constraint,bool disableCollisionsBetweenLinkedBodies)
   --  {
   --          m_constraints.push_back(constraint);
   --          if (disableCollisionsBetweenLinkedBodies)
   --          {
   --                  constraint->getRigidBodyA().addConstraintRef(constraint);
   --                  constraint->getRigidBodyB().addConstraintRef(constraint);
   --          }
   --  }

   overriding procedure removeConstraint (Self       : in out Item;
                               constraint : access impact.d3.Joint.Item'Class)
   is
   begin
      raise Program_Error; -- tbd

--        Self.m_constraints.remove (constraint);
--        constraint.getRigidBodyA.removeConstraintRef (constraint);
--        constraint.getRigidBodyB.removeConstraintRef (constraint);
   end removeConstraint;

   --  void        impact.d3.Space.dynamic.discrete::removeConstraint(impact.d3.Joint* constraint)
   --  {
   --          m_constraints.remove(constraint);
   --          constraint->getRigidBodyA().removeConstraintRef(constraint);
   --          constraint->getRigidBodyB().removeConstraintRef(constraint);
   --  }

   procedure addAction (Self             : in out Item;
                        Action_Interface : access impact.d3.Action.Item'Class)
   is
   begin
      raise Program_Error; -- tbd
--        Self.m_actions.push_back (action);
   end addAction;

   --  void        impact.d3.Space.dynamic.discrete::addAction(impact.d3.Action* action)
   --  {
   --          m_actions.push_back(action);
   --  }

   procedure removeAction (Self             : in out Item;
                           Action_Interface : access impact.d3.Action.Item'Class)
   is
   begin
      raise Program_Error; -- tbd
--        Self.m_actions.remove (action);
   end removeAction;

   --  void        impact.d3.Space.dynamic.discrete::removeAction(impact.d3.Action* action)
   --  {
   --          m_actions.remove(action);
   --  }

   function getSimulationIslandManager (Self : in Item) return access impact.d3.simulation_island_Manager.Item'Class
   is
   begin
      return Self.m_islandManager;
   end getSimulationIslandManager;


   --          impact.d3.simulation_island_Manager*        getSimulationIslandManager()
   --          {
   --                  return m_islandManager;
   --          }

--     function getCollisionWorld (Self : access Item) return impact.d3.Space.view
--     is
--     begin
--        return impact.d3.Space.view (Self);
--     end getCollisionWorld;

   --          impact.d3.Space*        getCollisionWorld()
   --          {
   --                  return this;
   --          }

   overriding procedure setGravity (Self    : in out Item;
                         gravity : in Vector_3)
   is
      use impact.d3.Object.rigid;
      use type Flags;
   begin
      Self.m_gravity := gravity;
      for I in 1 .. Integer (Self.m_nonStaticRigidBodies.Length) loop
         declare
            the_body : constant access impact.d3.Object.rigid.Item := Self.m_nonStaticRigidBodies.Element (I);
         begin
            if the_body.isActive and then not ((the_body.getFlags and BT_DISABLE_WORLD_GRAVITY) /= 0) then
               the_body.setGravity (gravity);
            end if;
         end;
      end loop;
   end setGravity;

   --  void        impact.d3.Space.dynamic.discrete::setGravity(const impact.d3.Vector& gravity)
   --  {
   --          m_gravity = gravity;
   --          for ( int i=0;i<m_nonStaticRigidBodies.size();i++)
   --          {
   --                  impact.d3.Object.rigid* body = m_nonStaticRigidBodies[i];
   --                  if (body->isActive() && !(body->getFlags() &BT_DISABLE_WORLD_GRAVITY))
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

   --  impact.d3.Vector impact.d3.Space.dynamic.discrete::getGravity () const
   --  {
   --          return m_gravity;
   --  }

--     procedure addCollisionObject (Self                 : in out Item;
--                                   collisionObject      : access impact.d3.Object.impact.d3.Object'Class;
--                                   collisionFilterGroup : in CollisionFilterGroups := StaticFilter;
--                                   collisionFilterMask  : in CollisionFilterGroups := AllFilter xor StaticFilter)
--     is
--     begin
--        impact.d3.Space.addCollisionObject (impact.d3.Space.item (Self),
--                                             collisionObject,
--                                             collisionFilterGroup,
--                                             collisionFilterMask);
--     end addCollisionObject;

   --  void        impact.d3.Space.dynamic.discrete::addCollisionObject(impact.d3.Object* collisionObject,short int collisionFilterGroup,short int collisionFilterMask)
   --  {
   --          impact.d3.Space::addCollisionObject(collisionObject,collisionFilterGroup,collisionFilterMask);
   --  }

   overriding procedure addRigidBody (Self     : in out Item;
                           the_body : access impact.d3.Object.rigid.Item'Class)
   is
      use impact.d3.Object.rigid;
      use impact.d3.Object;
      use type Flags;
   begin
      if         not the_body.isStaticOrKinematicObject
        and then (the_body.getFlags and BT_DISABLE_WORLD_GRAVITY) = 0
      then
         the_body.setGravity (Self.m_gravity);
      end if;

      if the_body.getCollisionShape /= null then
         if not the_body.isStaticObject then
            Self.m_nonStaticRigidBodies.append (the_body.all'Access);
         else
            the_body.setActivationState (ISLAND_SLEEPING);
         end if;

         declare
            function Crap_A (Exp : Boolean) return CollisionFilterGroups is
            begin
               if Exp then
                  return impact.d3.collision.Proxy.DefaultFilter;
               else
                  return impact.d3.collision.Proxy.StaticFilter;
               end if;
            end Crap_A;

            function Crap_B (Exp : Boolean) return CollisionFilterGroups is
            begin
               if Exp then
                  return impact.d3.collision.Proxy.AllFilter;
               else
                  return impact.d3.collision.Proxy.AllFilter xor impact.d3.collision.Proxy.StaticFilter;
               end if;
            end Crap_B;

            isDynamic            : constant Boolean := not (the_body.isStaticObject or the_body.isKinematicObject);
            collisionFilterGroup : constant CollisionFilterGroups := Crap_A (isDynamic);
            collisionFilterMask  : constant CollisionFilterGroups := Crap_B (isDynamic);
         begin
            addCollisionObject (Self, the_body.all'Access, collisionFilterGroup, collisionFilterMask);
         end;
      end if;
   end addRigidBody;

   --  void        impact.d3.Space.dynamic.discrete::addRigidBody(impact.d3.Object.rigid* body)
   --  {
   --          if (!body->isStaticOrKinematicObject() && !(body->getFlags() &BT_DISABLE_WORLD_GRAVITY))
   --          {
   --                  body->setGravity(m_gravity);
   --          }
   --
   --          if (body->getCollisionShape())
   --          {
   --                  if (!body->isStaticObject())
   --                  {
   --                          m_nonStaticRigidBodies.push_back(body);
   --                  } else
   --                  {
   --                          body->setActivationState(ISLAND_SLEEPING);
   --                  }
   --
   --                  bool isDynamic = !(body->isStaticObject() || body->isKinematicObject());
   --                  short collisionFilterGroup = isDynamic? short(impact.d3.collision.Proxy::DefaultFilter) : short(impact.d3.collision.Proxy::StaticFilter);
   --                  short collisionFilterMask = isDynamic?         short(impact.d3.collision.Proxy::AllFilter) :         short(impact.d3.collision.Proxy::AllFilter ^ impact.d3.collision.Proxy::StaticFilter);
   --
   --                  addCollisionObject(body,collisionFilterGroup,collisionFilterMask);
   --          }
   --  }

   procedure addRigidBody (Self     : in out Item;
                           the_body : access impact.d3.Object.rigid.Item'Class;
                           group    : in CollisionFilterGroups;
                           mask     : in CollisionFilterGroups                  )
   is
      use impact.d3.Object.rigid;
      use impact.d3.Object;
      use type Flags;
   begin
      if not the_body.isStaticOrKinematicObject and then not ((the_body.getFlags and BT_DISABLE_WORLD_GRAVITY) /= 0) then
         the_body.setGravity (Self.m_gravity);
      end if;

      if the_body.getCollisionShape /= null then
         if not the_body.isStaticObject then
            Self.m_nonStaticRigidBodies.append (the_body.all'Access);
         else
            the_body.setActivationState (ISLAND_SLEEPING);
         end if;

         addCollisionObject (Self, the_body, group, mask);
      end if;

   end addRigidBody;

   --  void        impact.d3.Space.dynamic.discrete::addRigidBody(impact.d3.Object.rigid* body, short group, short mask)
   --  {
   --          if (!body->isStaticOrKinematicObject() && !(body->getFlags() &BT_DISABLE_WORLD_GRAVITY))
   --          {
   --                  body->setGravity(m_gravity);
   --          }
   --
   --          if (body->getCollisionShape())
   --          {
   --                  if (!body->isStaticObject())
   --                  {
   --                          m_nonStaticRigidBodies.push_back(body);
   --                  }
   --                   else
   --                  {
   --                          body->setActivationState(ISLAND_SLEEPING);
   --                  }
   --                  addCollisionObject(body,group,mask);
   --          }
   --  }

   procedure removeRigidBody (Self     : in out Item;
                              the_body : access impact.d3.Object.rigid.Item'Class)
   is
      the_Index : constant Index := Self.m_nonStaticRigidBodies.find_Index (the_body.all'Access);
   begin
      if the_Index /= 0 then
         Self.m_nonStaticRigidBodies.delete (the_Index);
      end if;

      impact.d3.Space.removeCollisionObject (impact.d3.Space.item (Self), the_body);
   end removeRigidBody;

   --  void        impact.d3.Space.dynamic.discrete::removeRigidBody(impact.d3.Object.rigid* body)
   --  {
   --          m_nonStaticRigidBodies.remove(body);
   --          impact.d3.Space::removeCollisionObject(body);
   --  }

   overriding procedure removeCollisionObject (Self            : in out Item;
                                    collisionObject : access impact.d3.Object.item'Class)
   is
      the_body : constant impact.d3.Object.rigid.view := impact.d3.Object.rigid.view (collisionObject);
   begin
      if the_body /= null then
         Self.removeRigidBody (the_body);
      else
         impact.d3.Space.removeCollisionObject (impact.d3.Space.item (Self), collisionObject);
      end if;
   end removeCollisionObject;

   --  void        impact.d3.Space.dynamic.discrete::removeCollisionObject(impact.d3.Object* collisionObject)
   --  {
   --          impact.d3.Object.rigid* body = impact.d3.Object.rigid::upcast(collisionObject);
   --          if (body)
   --                  removeRigidBody(body);
   --          else
   --                  impact.d3.Space::removeCollisionObject(collisionObject);
   --  }

   procedure setConstraintSolver (Self   : in out Item;
                                  solver : access impact.d3.constraint_Solver.Item'Class)
   is
      procedure free is new ada.Unchecked_Deallocation (impact.d3.constraint_Solver.Item'Class, impact.d3.constraint_Solver.View);

   begin
      if Self.m_ownsConstraintSolver then
         free (Self.m_constraintSolver);
      end if;
      Self.m_ownsConstraintSolver := False;
      Self.m_constraintSolver     := solver.all'Access;
   end setConstraintSolver;

   --  void        impact.d3.Space.dynamic.discrete::setConstraintSolver(impact.d3.constraint_Solver* solver)
   --  {
   --          if (m_ownsConstraintSolver)
   --          {
   --                  btAlignedFree( m_constraintSolver);
   --          }
   --          m_ownsConstraintSolver = false;
   --          m_constraintSolver = solver;
   --  }

   overriding function getConstraintSolver (Self : in Item) return access impact.d3.constraint_Solver.Item'Class
   is
   begin
      return Self.m_constraintSolver;
   end getConstraintSolver;

   --  impact.d3.constraint_Solver* impact.d3.Space.dynamic.discrete::getConstraintSolver()
   --  {
   --          return m_constraintSolver;
   --  }

   overriding function getNumConstraints (Self : in Item) return Integer
   is
   begin
      return Integer (Self.m_constraints.Length);
   end getNumConstraints;

   --  int                impact.d3.Space.dynamic.discrete::getNumConstraints() const
   --  {
   --          return int(m_constraints.size());
   --  }

   overriding function getConstraint (Self  : in Item;
                           index : in Integer) return access impact.d3.Joint.Item'Class
   is
   begin
      return Self.m_constraints.Element (index);
   end getConstraint;

   --  impact.d3.Joint* impact.d3.Space.dynamic.discrete::getConstraint(int index)
   --  {
   --          return m_constraints[index];
   --  }

   overriding function getWorldType (Self : in Item) return impact.d3.Space.dynamic.Kind
   is
      pragma Unreferenced (Self);
--        BT_DISCRETE_DYNAMICS_WORLD : impact.d3.Space.dynamic.Kind;
   begin
      return discrete_Kind; -- BT_DISCRETE_DYNAMICS_WORLD;
   end getWorldType;

   --          virtual impact.d3.Space.dynamicType        getWorldType() const
   --          {
   --                  return BT_DISCRETE_DYNAMICS_WORLD;
   --          }

   overriding procedure clearForces (Self : in out Item)
   is
   begin
      for I in 1 .. Integer (Self.m_nonStaticRigidBodies.Length) loop
         declare
            The_Body : constant access impact.d3.Object.rigid.Item'Class := Self.m_nonStaticRigidBodies.Element (I);
         begin
            The_Body.clearForces;
         end;
      end loop;

   end clearForces;

   --  void        impact.d3.Space.dynamic.discrete::clearForces()
   --  {
   --          ///@todo: iterate over awake simulation islands!
   --          for ( int i=0;i<m_nonStaticRigidBodies.size();i++)
   --          {
   --                  impact.d3.Object.rigid* body = m_nonStaticRigidBodies[i];
   --                  //need to check if next line is ok
   --                  //it might break backward compatibility (people applying forces on sleeping objects get never cleared and accumulate on wake-up
   --                  body->clearForces();
   --          }
   --  }
   --






   procedure applyGravity (Self : in out Item)
   is
   begin
      for I in 1 .. Integer (Self.m_nonStaticRigidBodies.Length) loop
         declare
            The_Body : constant access impact.d3.Object.rigid.Item'Class := Self.m_nonStaticRigidBodies.Element (I);
         begin
            if The_Body.isActive then
               The_Body.applyGravity;
            end if;
         end;
      end loop;
   end applyGravity;



   --  ///apply gravity, call this once per timestep
   --  void        impact.d3.Space.dynamic.discrete::applyGravity()
   --  {
   --          ///@todo: iterate over awake simulation islands!
   --          for ( int i=0;i<m_nonStaticRigidBodies.size();i++)
   --          {
   --                  impact.d3.Object.rigid* body = m_nonStaticRigidBodies[i];
   --                  if (body->isActive())
   --                  {
   --                          body->applyGravity();
   --                  }
   --          }
   --  }

   --  procedure setNumTasks (Self     : in Item;
   --                         numTasks : in Integer)
   --  is
   --  begin
   --     null;
   --  end setNumTasks;

   procedure setSynchronizeAllMotionStates (Self           : in out Item;
                                            synchronizeAll : in     Boolean)
   is
   begin
      Self.m_synchronizeAllMotionStates := synchronizeAll;
   end setSynchronizeAllMotionStates;

   --          void        setSynchronizeAllMotionStates(bool synchronizeAll)
   --          {
   --                  m_synchronizeAllMotionStates = synchronizeAll;
   --          }

   function getSynchronizeAllMotionStates (Self : in Item) return Boolean
   is
   begin
      return Self.m_synchronizeAllMotionStates;
   end getSynchronizeAllMotionStates;

   --          bool getSynchronizeAllMotionStates() const
   --          {
   --                  return m_synchronizeAllMotionStates;
   --          }

--     procedure serialize (Self       : in Item;
--                          serializer : access btSerializer.Item'Class)
--     is
--     begin
--        null;
--     end serialize;


   procedure predictUnconstraintMotion (Self     : in out Item;
                                        timeStep : in Real)
   is
      use impact.d3.Object;
   begin
      for I in 1 .. Integer (Self.m_nonStaticRigidBodies.Length) loop
         declare
            the_body : constant access impact.d3.Object.rigid.Item := Self.m_nonStaticRigidBodies.Element (I);
         begin
            if not the_body.isStaticOrKinematicObject then
               the_body.integrateVelocities (timeStep);
               the_body.applyDamping (timeStep);
               the_body.predictIntegratedTransform (timeStep, the_body.getInterpolationWorldTransform.all);
            end if;
         end;
      end loop;
   end predictUnconstraintMotion;

   --  void        impact.d3.Space.dynamic.discrete::predictUnconstraintMotion(impact.d3.Scalar timeStep)
   --  {
   --          BT_PROFILE("predictUnconstraintMotion");
   --          for ( int i=0;i<m_nonStaticRigidBodies.size();i++)
   --          {
   --                  impact.d3.Object.rigid* body = m_nonStaticRigidBodies[i];
   --                  if (!body->isStaticOrKinematicObject())
   --                  {
   --                          body->integrateVelocities( timeStep);
   --                          //damping
   --                          body->applyDamping(timeStep);
   --
   --                          body->predictIntegratedTransform(timeStep,body->getInterpolationWorldTransform());
   --                  }
   --          }
   --  }

   procedure calculateSimulationIslands (Self : access Item)
   is
      numConstraints : constant Integer := Integer (Self.m_constraints.Length);
   begin
      Self.getSimulationIslandManager.updateActivationState (Self, -- .getCollisionWorld,
                                                             Self.getDispatcher);
      for I in 1 .. numConstraints loop
         declare
            constraint : constant access   impact.d3.Joint.Item := Self.m_constraints.Element (I);

            colObj0    : constant impact.d3.Object.rigid.view       := constraint.getRigidBodyA;
            colObj1    : constant impact.d3.Object.rigid.view       := constraint.getRigidBodyB;
         begin
            if         colObj0 /= null and then not colObj0.isStaticOrKinematicObject
              and then colObj1 /= null and then not colObj1.isStaticOrKinematicObject
            then
               if colObj0.isActive or else colObj1.isActive then
                  Self.getSimulationIslandManager.getUnionFind.unite (colObj0.getIslandTag,
                                                                      colObj1.getIslandTag);
               end if;
            end if;
         end;
      end loop;


      --  Store the island id in each body
      --
      Self.getSimulationIslandManager.storeIslandActivationState (Self);

   end calculateSimulationIslands;

   --  void        impact.d3.Space.dynamic.discrete::calculateSimulationIslands()
   --  {
   --          BT_PROFILE("calculateSimulationIslands");
   --
   --          getSimulationIslandManager()->updateActivationState(getCollisionWorld(),getCollisionWorld()->getDispatcher());
   --
   --          {
   --                  int i;
   --                  int numConstraints = int(m_constraints.size());
   --                  for (i=0;i< numConstraints ; i++ )
   --                  {
   --                          impact.d3.Joint* constraint = m_constraints[i];
   --
   --                          const impact.d3.Object.rigid* colObj0 = &constraint->getRigidBodyA();
   --                          const impact.d3.Object.rigid* colObj1 = &constraint->getRigidBodyB();
   --
   --                          if (((colObj0) && (!(colObj0)->isStaticOrKinematicObject())) &&
   --                                  ((colObj1) && (!(colObj1)->isStaticOrKinematicObject())))
   --                          {
   --                                  if (colObj0->isActive() || colObj1->isActive())
   --                                  {
   --
   --                                          getSimulationIslandManager()->getUnionFind().unite((colObj0)->getIslandTag(),
   --                                                  (colObj1)->getIslandTag());
   --                                  }
   --                          }
   --                  }
   --          }
   --
   --          //Store the island id in each body
   --          getSimulationIslandManager()->storeIslandActivationState(getCollisionWorld());
   --
   --
   --  }






   function btGetConstraintIslandId (lhs : in impact.d3.Joint.Item'Class) return Integer
   is
      rcolObj0 : impact.d3.Object.item'Class renames impact.d3.Object.item'Class (lhs.getRigidBodyA.all);
      rcolObj1 : impact.d3.Object.item'Class renames impact.d3.Object.item'Class (lhs.getRigidBodyB.all);
      islandId : Integer;
   begin
      if rcolObj0.getIslandTag >= 0 then
         islandId := rcolObj0.getIslandTag;
      else
         islandId := rcolObj1.getIslandTag;
      end if;

      return islandId;
   end btGetConstraintIslandId;



--
--  SIMD_FORCE_INLINE        int        btGetConstraintIslandId(const impact.d3.Joint* lhs)
--  {
--          int islandId;
--
--          const impact.d3.Object& rcolObj0 = lhs->getRigidBodyA();
--          const impact.d3.Object& rcolObj1 = lhs->getRigidBodyB();
--          islandId= rcolObj0.getIslandTag()>=0?rcolObj0.getIslandTag():rcolObj1.getIslandTag();
--          return islandId;
--
--  }









   overriding procedure ProcessIsland (Self      : access InplaceSolverIslandCallback;
                            bodies    : access impact.d3.Object.Vector;
                            manifolds : access impact.d3.Manifold.Vector;
                            islandId  : in     Integer                  )
   is
      use type ada.Containers.Count_type;

      unused : math.Real;
      pragma Unreferenced (unused);
   begin
      if islandId < 0 then
         if manifolds.Length + Self.m_sortedConstraints.Length > 0 then
            --  we don't split islands, so all constraints/contact manifolds/bodies are passed into the solver regardless the island id
            unused := Self.m_solver.solveGroup (bodies,    -- Integer (bodies.Length),
                                                manifolds, -- Integer (manifolds),
                                                Self.m_sortedConstraints, -- Self.m_numConstraints,
                                                Self.m_solverInfo.all,
                                                Self.m_dispatcher.all);
            null;
         end if;
      else
         --  also add all non-contact constraints/joints for this island
         declare
            use impact.d3.Joint.Vectors;

            --                 startConstraint   : access impact.d3.Joint.Item := null;
            startConstraint   : Integer;
            numCurConstraints : Integer  := 0;
            the_island_Constraints : aliased impact.d3.Joint.Vector;

            unused            : math.Real;
            pragma Unreferenced (unused);

            i : Integer       := 0;
         begin
            --  find the first constraint for this island
            --
            while i < Integer (Self.m_sortedConstraints.Length)
            loop
               if btGetConstraintIslandId (Self.m_sortedConstraints.Element (i + 1).all) = islandId then
                  --                       startConstraint := Self.m_sortedConstraints.all (I);
                  startConstraint := I;
                  exit;
               end if;

               i := i + 1;
            end loop;

            --  count the number of constraints in this island
            --
            while i < Integer (Self.m_sortedConstraints.Length)
            loop
               if btGetConstraintIslandId (Self.m_sortedConstraints.Element (i + 1).all) = islandId then
                  the_island_Constraints.append (Self.m_sortedConstraints.Element (i + 1));
                  numCurConstraints := numCurConstraints + 1;
               end if;

               i := i + 1;
            end loop;


            if Self.m_solverInfo.m_minimumSolverBatchSize <= 1 then
               --  only call solveGroup if there is some work: avoid virtual function call, its overhead can be excessive
               --
               if Integer (manifolds.Length) + numCurConstraints /= 0 then
                  unused := Self.m_solver.solveGroup (bodies   .all'Access,
                                                      manifolds.all'Access,
                                                      the_island_Constraints'Access,
                                                      Self.m_solverInfo.all,
                                                      Self.m_dispatcher.all);
               end if;

            else
               for I in 1 .. Integer (bodies.Length) loop
                  Self.m_bodies.append (bodies.Element (I));
               end loop;

               for I in 1 .. Integer (manifolds.Length) loop
                  Self.m_manifolds.append (manifolds.Element (I));
               end loop;

               for I in 1 .. numCurConstraints loop
                  Self.m_constraints.append (Self.m_sortedConstraints.Element (startConstraint + I - 1));
               end loop;

               if  Integer (Self.m_constraints.Length) + Integer (Self.m_manifolds.Length) > Self.m_solverInfo.m_minimumSolverBatchSize then
                  processConstraints (Self.all);
               else
                  null;
               end if;
            end if;
         end;
      end if;

   end ProcessIsland;






   procedure processConstraints (Self : in out InplaceSolverIslandCallback)
   is
      use type ada.Containers.Count_Type;
   begin
      if Self.m_manifolds.Length + Self.m_constraints.Length > 0 then
         declare
            bodies      : constant access impact.d3.Object.Vector   := Self.m_bodies     'Access;
            manifold    : constant access impact.d3.Manifold.Vector := Self.m_manifolds  'Access;
            constraints : constant access impact.d3.Joint.Vector    := Self.m_constraints'Access;

            unused      : math.Real;
            pragma Unreferenced (unused);
         begin
            --  m_solver->solveGroup( bodies,m_bodies.size(),manifold, m_manifolds.size(),constraints, m_constraints.size() ,m_solverInfo,m_debugDrawer,m_stackAlloc,m_dispatcher);
            unused := Self.m_solver.solveGroup (bodies,
                                                manifold,
                                                constraints,
                                                Self.m_solverInfo.all,
                                                Self.m_dispatcher.all);
         end;
      end if;

      Self.m_bodies     .reserve_Capacity (0);
      Self.m_bodies.Clear;

      Self.m_manifolds  .reserve_Capacity (0);
      Self.m_manifolds.Clear;

      Self.m_constraints.reserve_Capacity (0);
      Self.m_constraints.Clear;
   end processConstraints;






   procedure solveConstraints (Self       : access Item;
                               solverInfo : access impact.d3.contact_solver_Info.Item'Class)
   is


      function to_InplaceSolverIslandCallback (solverInfo        : access impact.d3.contact_solver_Info.Item'Class;
                                               solver            : access impact.d3.constraint_Solver.Item'Class;
                                               sortedConstraints : access impact.d3.Joint.Vector;
                                               dispatcher        : access impact.d3.Dispatcher.item'Class)
                                              return InplaceSolverIslandCallback
      is
         Self : InplaceSolverIslandCallback;
      begin
         Self.m_solverInfo        := solverInfo;
         Self.m_solver            := solver;
         Self.m_sortedConstraints := sortedConstraints;
         Self.m_dispatcher        := dispatcher;

         return Self;
      end to_InplaceSolverIslandCallback;


   begin
      declare
         use impact.d3.Joint.Vectors;

         sortedConstraints : aliased impact.d3.Joint.Vector;


         function "<" (L, R : in impact.d3.Joint.view) return Boolean
         is
            rIslandId0,
            lIslandId0 : Integer;
         begin
            rIslandId0 := btGetConstraintIslandId (R.all);
            lIslandId0 := btGetConstraintIslandId (L.all);

            return lIslandId0 < rIslandId0;
         end "<";

         package Sorter is new impact.d3.Joint.Vectors.Generic_Sorting ("<");


--  class btSortConstraintOnIslandPredicate
--  {
--          public:
--
--                  bool operator() ( const impact.d3.Joint* lhs, const impact.d3.Joint* rhs )
--                  {
--                          int rIslandId0,lIslandId0;
--                          rIslandId0 = btGetConstraintIslandId(rhs);
--                          lIslandId0 = btGetConstraintIslandId(lhs);
--                          return lIslandId0 < rIslandId0;
--                  }
--  };




      begin
         sortedConstraints.reserve_Capacity (Self.m_constraints.Length);   -- tbd: probly unneeded.

         sortedConstraints := Self.m_constraints;
--           for I in 1 .. Integer (getNumConstraints (Self.all)) loop
--              sortedConstraints (I) := Self.m_constraints (I);
--           end loop;


         Sorter.sort (sortedConstraints);
--           sortedConstraints.quickSort (btSortConstraintOnIslandPredicate (Self));

         declare
--              constraintsPtr : access impact.d3.Joint.Item := sortedConstraints'access;

            solverCallback : aliased InplaceSolverIslandCallback
              := to_InplaceSolverIslandCallback
                (solverInfo, Self.m_constraintSolver, sortedConstraints'Unchecked_Access, Self.getDispatcher);

         begin
            Self.m_constraintSolver.prepareSolve (Self.getNumCollisionObjects, Self.getDispatcher.getNumManifolds);

            --  Solve all the constraints for this island.
            --
            Self.m_islandManager.buildAndProcessIslands (Self.getDispatcher, Self, solverCallback'Access);
            solverCallback.processConstraints;
            Self.m_constraintSolver.allSolved (solverInfo.all);
         end;

      end;
   end solveConstraints;



   --          sortedConstraints.quickSort(btSortConstraintOnIslandPredicate());
   --
   --          impact.d3.Joint** constraintsPtr = getNumConstraints() ? &sortedConstraints[0] : 0;
   --
   --          InplaceSolverIslandCallback        solverCallback(        solverInfo,        m_constraintSolver, constraintsPtr,sortedConstraints.size(),        m_debugDrawer,m_stackAlloc,m_dispatcher1);
   --
   --          m_constraintSolver->prepareSolve(getCollisionWorld()->getNumCollisionObjects(), getCollisionWorld()->getDispatcher()->getNumManifolds());
   --
   --          /// solve all the constraints for this island
   --          m_islandManager->buildAndProcessIslands(getCollisionWorld()->getDispatcher(),getCollisionWorld(),&solverCallback);
   --
   --          solverCallback.processConstraints();
   --
   --          m_constraintSolver->allSolved(solverInfo, m_debugDrawer, m_stackAlloc);





   --  void        impact.d3.Space.dynamic.discrete::solveConstraints(impact.d3.contact_solver_Info& solverInfo)
   --  {



   --          struct InplaceSolverIslandCallback : public impact.d3.simulation_island_Manager::IslandCallback
   --          {
   --
   --                  impact.d3.contact_solver_Info&        m_solverInfo;
   --                  impact.d3.constraint_Solver*                m_solver;
   --                  impact.d3.Joint**                m_sortedConstraints;
   --                  int                                                m_numConstraints;
   --                  btIDebugDraw*                        m_debugDrawer;
   --                  btStackAlloc*                        m_stackAlloc;
   --                  impact.d3.Dispatcher*                        m_dispatcher;
   --
   --                  btAlignedObjectArray<impact.d3.Object*> m_bodies;
   --                  btAlignedObjectArray<impact.d3.Manifold*> m_manifolds;
   --                  btAlignedObjectArray<impact.d3.Joint*> m_constraints;
   --
   --
   --                  InplaceSolverIslandCallback(
   --                          impact.d3.contact_solver_Info& solverInfo,
   --                          impact.d3.constraint_Solver*        solver,
   --                          impact.d3.Joint** sortedConstraints,
   --                          int        numConstraints,
   --                          btIDebugDraw*        debugDrawer,
   --                          btStackAlloc*                        stackAlloc,
   --                          impact.d3.Dispatcher* dispatcher)
   --                          :m_solverInfo(solverInfo),
   --                          m_solver(solver),
   --                          m_sortedConstraints(sortedConstraints),
   --                          m_numConstraints(numConstraints),
   --                          m_debugDrawer(debugDrawer),
   --                          m_stackAlloc(stackAlloc),
   --                          m_dispatcher(dispatcher)
   --                  {
   --
   --                  }
   --
   --
   --                  InplaceSolverIslandCallback& operator=(InplaceSolverIslandCallback& other)
   --                  {
   --                          btAssert(0);
   --                          (void)other;
   --                          return *this;
   --                  }
   --                  virtual        void        ProcessIsland(impact.d3.Object** bodies,int numBodies,impact.d3.Manifold**        manifolds,int numManifolds, int islandId)
   --                  {
   --                          if (islandId<0)
   --                          {
   --                                  if (numManifolds + m_numConstraints)
   --                                  {
   --                                          ///we don't split islands, so all constraints/contact manifolds/bodies are passed into the solver regardless the island id
   --                                          m_solver->solveGroup( bodies,numBodies,manifolds, numManifolds,&m_sortedConstraints[0],m_numConstraints,m_solverInfo,m_debugDrawer,m_stackAlloc,m_dispatcher);
   --                                  }
   --                          } else
   --                          {
   --                                          //also add all non-contact constraints/joints for this island
   --                                  impact.d3.Joint** startConstraint = 0;
   --                                  int numCurConstraints = 0;
   --                                  int i;
   --
   --                                  //find the first constraint for this island
   --                                  for (i=0;i<m_numConstraints;i++)
   --                                  {
   --                                          if (btGetConstraintIslandId(m_sortedConstraints[i]) == islandId)
   --                                          {
   --                                                  startConstraint = &m_sortedConstraints[i];
   --                                                  break;
   --                                          }
   --                                  }
   --                                  //count the number of constraints in this island
   --                                  for (;i<m_numConstraints;i++)
   --                                  {
   --                                          if (btGetConstraintIslandId(m_sortedConstraints[i]) == islandId)
   --                                          {
   --                                                  numCurConstraints++;
   --                                          }
   --                                  }
   --
   --                                  if (m_solverInfo.m_minimumSolverBatchSize<=1)
   --                                  {
   --                                          ///only call solveGroup if there is some work: avoid virtual function call, its overhead can be excessive
   --                                          if (numManifolds + numCurConstraints)
   --                                          {
   --                                                  m_solver->solveGroup( bodies,numBodies,manifolds, numManifolds,startConstraint,numCurConstraints,m_solverInfo,m_debugDrawer,m_stackAlloc,m_dispatcher);
   --                                          }
   --                                  } else
   --                                  {
   --
   --                                          for (i=0;i<numBodies;i++)
   --                                                  m_bodies.push_back(bodies[i]);
   --                                          for (i=0;i<numManifolds;i++)
   --                                                  m_manifolds.push_back(manifolds[i]);
   --                                          for (i=0;i<numCurConstraints;i++)
   --                                                  m_constraints.push_back(startConstraint[i]);
   --                                          if ((m_constraints.size()+m_manifolds.size())>m_solverInfo.m_minimumSolverBatchSize)
   --                                          {
   --                                                  processConstraints();
   --                                          } else
   --                                          {
   --                                                  //printf("deferred\n");
   --                                          }
   --                                  }
   --                          }
   --                  }
   --                  void        processConstraints()
   --                  {
   --                          if (m_manifolds.size() + m_constraints.size()>0)
   --                          {
   --
   --                                  impact.d3.Object** bodies = m_bodies.size()? &m_bodies[0]:0;
   --                                  impact.d3.Manifold** manifold = m_manifolds.size()?&m_manifolds[0]:0;
   --                                  impact.d3.Joint** constraints = m_constraints.size()?&m_constraints[0]:0;
   --
   --                                  m_solver->solveGroup( bodies,m_bodies.size(),manifold, m_manifolds.size(),constraints, m_constraints.size() ,m_solverInfo,m_debugDrawer,m_stackAlloc,m_dispatcher);
   --                          }
   --                          m_bodies.resize(0);
   --                          m_manifolds.resize(0);
   --                          m_constraints.resize(0);
   --
   --                  }
   --
   --          };
   --




   --
   --
   --          //sorted version of all impact.d3.Joint, based on islandId
   --          btAlignedObjectArray<impact.d3.Joint*>        sortedConstraints;
   --          sortedConstraints.resize( m_constraints.size());
   --          int i;
   --          for (i=0;i<getNumConstraints();i++)
   --          {
   --                  sortedConstraints[i] = m_constraints[i];
   --          }
   --
   --  //        btAssert(0);
   --
   --
   --
   --          sortedConstraints.quickSort(btSortConstraintOnIslandPredicate());
   --
   --          impact.d3.Joint** constraintsPtr = getNumConstraints() ? &sortedConstraints[0] : 0;
   --
   --          InplaceSolverIslandCallback        solverCallback(        solverInfo,        m_constraintSolver, constraintsPtr,sortedConstraints.size(),        m_debugDrawer,m_stackAlloc,m_dispatcher1);
   --
   --          m_constraintSolver->prepareSolve(getCollisionWorld()->getNumCollisionObjects(), getCollisionWorld()->getDispatcher()->getNumManifolds());
   --
   --          /// solve all the constraints for this island
   --          m_islandManager->buildAndProcessIslands(getCollisionWorld()->getDispatcher(),getCollisionWorld(),&solverCallback);
   --
   --          solverCallback.processConstraints();
   --
   --          m_constraintSolver->allSolved(solverInfo, m_debugDrawer, m_stackAlloc);
   --  }

















   procedure updateActivationState (Self     : in Item;
                                    timeStep : in Real)
   is
      use impact.d3.Object.rigid;
   begin
      for I in 1 .. Integer (Self.m_nonStaticRigidBodies.Length) loop
         declare
            use impact.d3.Object;
            the_body : constant access impact.d3.Object.rigid.Item := Self.m_nonStaticRigidBodies.Element (I);
         begin
            if the_body /= null then
               the_body.updateDeactivation (timeStep);

               if the_body.wantsSleeping then

                  if the_body.isStaticOrKinematicObject then
                     the_body.setActivationState (ISLAND_SLEEPING);

                  else
                     if the_body.getActivationState = ACTIVE_TAG then
                        the_body.setActivationState (WANTS_DEACTIVATION);
                     end if;

                     if the_body.getActivationState = ISLAND_SLEEPING then
                        the_body.setAngularVelocity ((0.0, 0.0, 0.0));
                        the_body.setLinearVelocity  ((0.0, 0.0, 0.0));
                     end if;

                  end if;

               else
                  if the_body.getActivationState /= DISABLE_DEACTIVATION then
                     the_body.setActivationState (ACTIVE_TAG);
                  end if;
               end if;

            end if;

         end;
      end loop;
   end updateActivationState;

   --  void        impact.d3.Space.dynamic.discrete::updateActivationState(impact.d3.Scalar timeStep)
   --  {
   --          BT_PROFILE("updateActivationState");
   --
   --          for ( int i=0;i<m_nonStaticRigidBodies.size();i++)
   --          {
   --                  impact.d3.Object.rigid* body = m_nonStaticRigidBodies[i];
   --                  if (body)
   --                  {
   --                          body->updateDeactivation(timeStep);
   --
   --                          if (body->wantsSleeping())
   --                          {
   --                                  if (body->isStaticOrKinematicObject())
   --                                  {
   --                                          body->setActivationState(ISLAND_SLEEPING);
   --                                  } else
   --                                  {
   --                                          if (body->getActivationState() == ACTIVE_TAG)
   --                                                  body->setActivationState( WANTS_DEACTIVATION );
   --                                          if (body->getActivationState() == ISLAND_SLEEPING)
   --                                          {
   --                                                  body->setAngularVelocity(impact.d3.Vector(0,0,0));
   --                                                  body->setLinearVelocity(impact.d3.Vector(0,0,0));
   --                                          }
   --
   --                                  }
   --                          } else
   --                          {
   --                                  if (body->getActivationState() != DISABLE_DEACTIVATION)
   --                                          body->setActivationState( ACTIVE_TAG );
   --                          }
   --                  }
   --          }
   --  }
   --

   procedure updateActions (Self     : in out Item;
                            timeStep : in Real)
   is
   begin
      null;   -- tbd

--        for I in 1 .. Self.m_actions.Length loop
--           Self.m_actions(I).updateAction (Self, timeStep);
--        end loop;
   end updateActions;

   --  void        impact.d3.Space.dynamic.discrete::updateActions(impact.d3.Scalar timeStep)
   --  {
   --          BT_PROFILE("updateActions");
   --
   --          for ( int i=0;i<m_actions.size();i++)
   --          {
   --                  m_actions[i]->updateAction( this, timeStep);
   --          }
   --  }

   procedure startProfiling (Self     : in Item;
                             timeStep : in Real)
   is
   begin
      null;
   end startProfiling;

   procedure internalSingleStepSimulation (Self     : access item;
                                           timeStep : in Real)
   is
      use impact.d3.Space.dynamic;
   begin
      if Self.m_internalPreTickCallback /= null then
         Self.m_internalPreTickCallback.all (Self, timeStep);
      end if;

      predictUnconstraintMotion (Self.all, timeStep);

      declare
         dispatchInfo : constant access impact.d3.Dispatcher.DispatcherInfo := Self.getDispatchInfo;
      begin
         dispatchInfo.m_timeStep  := timeStep;
         dispatchInfo.m_stepCount := 0;
      end;

      Self.performDiscreteCollisionDetection;

      Self.calculateSimulationIslands;

      --  tbd ...
      Self.getSolverInfo.m_timeStep := timeStep;

      Self.solveConstraints (Self.getSolverInfo);

      Self.integrateTransforms (timeStep);

      Self.updateActions (timeStep);

      Self.updateActivationState (timeStep);

      if Self.m_internalTickCallback /= null then
         Self.m_internalPreTickCallback.all (Self, timeStep);
      end if;
   end internalSingleStepSimulation;

   --  void        impact.d3.Space.dynamic.discrete::internalSingleStepSimulation(impact.d3.Scalar timeStep)
   --  {
   --
   --          BT_PROFILE("internalSingleStepSimulation");
   --
   --          if(0 != m_internalPreTickCallback) {
   --                  (*m_internalPreTickCallback)(this, timeStep);
   --          }
   --
   --          ///apply gravity, predict motion
   --          predictUnconstraintMotion(timeStep);
   --
   --          impact.d3.DispatcherInfo& dispatchInfo = getDispatchInfo();
   --
   --          dispatchInfo.m_timeStep = timeStep;
   --          dispatchInfo.m_stepCount = 0;
   --          dispatchInfo.m_debugDraw = getDebugDrawer();
   --
   --
   --          ///perform collision detection
   --          performDiscreteCollisionDetection();
   --
   --
   --          calculateSimulationIslands();
   --
   --
   --          getSolverInfo().m_timeStep = timeStep;
   --
   --
   --
   --          ///solve contact and other joint constraints
   --          solveConstraints(getSolverInfo());
   --
   --          ///CallbackTriggers();
   --
   --          ///integrate transforms
   --          integrateTransforms(timeStep);
   --
   --          ///update vehicle simulation
   --          updateActions(timeStep);
   --
   --          updateActivationState( timeStep );
   --
   --          if(0 != m_internalTickCallback) {
   --                  (*m_internalTickCallback)(this, timeStep);
   --          }
   --  }

   procedure saveKinematicState (Self     : in out Item;
                                 timeStep : in Real)
   is
   begin
      for I in 1 .. Integer (Self.m_nonStaticRigidBodies.Length) loop
         declare
            colObj   : constant impact.d3.Object.view := Self.get_m_collisionObjects.Element (I).all'Access;
            the_body : constant impact.d3.Object.rigid.view := impact.d3.Object.rigid.view (colObj);
         begin
            if the_body /= null and then the_body.getActivationState /= Object.ISLAND_SLEEPING then
               if the_body.isKinematicObject then
                  the_body.saveKinematicState (timeStep);
               end if;
            end if;
         end;
      end loop;
   end saveKinematicState;

   --  void        impact.d3.Space.dynamic.discrete::saveKinematicState(impact.d3.Scalar timeStep)
   --  {
   --  ///would like to iterate over m_nonStaticRigidBodies, but unfortunately old API allows
   --  ///to switch status _after_ adding kinematic objects to the world
   --  ///fix it for Bullet 3.x release
   --          for (int i=0;i<m_collisionObjects.size();i++)
   --          {
   --                  impact.d3.Object* colObj = m_collisionObjects[i];
   --                  impact.d3.Object.rigid* body = impact.d3.Object.rigid::upcast(colObj);
   --                  if (body && body->getActivationState() != ISLAND_SLEEPING)
   --                  {
   --                          if (body->isKinematicObject())
   --                          {
   --                                  //to calculate velocities next frame
   --                                  body->saveKinematicState(timeStep);
   --                          }
   --                  }
   --          }
   --
   --  }










   --- btClosestNotMeConvexResultCallback
   --

   type btClosestNotMeConvexResultCallback is new impact.d3.Space.ClosestConvexResultCallback with
      record
        m_me                 : access impact.d3.Object.item'Class;
        m_allowedPenetration :        math.Real;
        m_pairCache          : access impact.d3.collision.overlapped_pair_Callback.cached.item'Class;
        m_dispatcher         : access impact.d3.Dispatcher.item'Class;
      end record;



   overriding function needsCollision (Self   : in     btClosestNotMeConvexResultCallback;
                            proxy0 : access impact.d3.collision.Proxy.item'Class) return Boolean;

   overriding function  addSingleResult (Self               : access btClosestNotMeConvexResultCallback;
                              convexResult       : access impact.d3.Space.LocalConvexResult;
                              normalInWorldSpace : in     Boolean                          ) return math.Real;




   function to_btClosestNotMeConvexResultCallback (me         : access impact.d3.Object.item'Class;
                                                   fromA, toA : in     math.Vector_3;
                                                   pairCache  : access impact.d3.collision.overlapped_pair_Callback.cached.item'Class;
                                                   dispatcher : access impact.d3.Dispatcher.item'Class) return btClosestNotMeConvexResultCallback
   is
      Self : constant btClosestNotMeConvexResultCallback := (impact.d3.Space.Forge.to_ClosestConvexResultCallback (fromA, toA) with
                                                      m_me                 => me,
                                                    m_allowedPenetration => 0.0,
                                                    m_pairCache          => pairCache,
                                                    m_dispatcher         => dispatcher);
   begin
      return Self;
   end to_btClosestNotMeConvexResultCallback;





   overriding function  addSingleResult (Self               : access btClosestNotMeConvexResultCallback;
                              convexResult       : access impact.d3.Space.LocalConvexResult;
                              normalInWorldSpace : in     Boolean                          ) return math.Real
   is
      use impact.d3.Space;
      use math.Vectors;
      use impact.d3.Vector;
   begin
      if convexResult.m_hitCollisionObject = Self.m_me then
         return 1.0;
      elsif not convexResult.m_hitCollisionObject.hasContactResponse then
         return 1.0;
      end if;

      declare
         linVelA          : constant Vector_3 := Self.m_convexToWorld - Self.m_convexFromWorld;
         linVelB          : constant Vector_3 := (0.0, 0.0, 0.0);

         relativeVelocity : constant Vector_3 := linVelA - linVelB;
      begin
         if dot (convexResult.m_hitNormalLocal, relativeVelocity) >= -Self.m_allowedPenetration then
            return 1.0;
         end if;
      end;

      return impact.d3.Space.addSingleResult (ClosestConvexResultCallback_view (Self), convexResult, normalInWorldSpace);

   end addSingleResult;


   overriding function needsCollision (Self   : in     btClosestNotMeConvexResultCallback;
                            proxy0 : access impact.d3.collision.Proxy.item'Class) return Boolean
   is
   begin
      if proxy0.m_clientObject = Self.m_me then
         return False;

      elsif not impact.d3.Space.needsCollision (ClosestConvexResultCallback (Self), proxy0) then
         return False;
      end if;

      declare
         otherObj : constant impact.d3.Object.view := impact.d3.Object.view (proxy0.m_clientObject);
      begin
         if Self.m_dispatcher.needsResponse (Self.m_me, otherObj) then
            return True;
         end if;
      end;

      return False;
   end needsCollision;








--  class btClosestNotMeConvexResultCallback : public impact.d3.Space::ClosestConvexResultCallback
--  {
--  public:
--
--          impact.d3.Object* m_me;
--          impact.d3.Scalar m_allowedPenetration;
--          impact.d3.collision.overlapped_pair_Callback.cached* m_pairCache;
--          impact.d3.Dispatcher* m_dispatcher;

--  public:
--          btClosestNotMeConvexResultCallback (impact.d3.Object* me,const impact.d3.Vector& fromA,const impact.d3.Vector& toA,impact.d3.collision.overlapped_pair_Callback.cached* pairCache,impact.d3.Dispatcher* dispatcher) :
--            impact.d3.Space::ClosestConvexResultCallback(fromA,toA),
--                  m_me(me),
--                  m_allowedPenetration(0.0f),
--                  m_pairCache(pairCache),
--                  m_dispatcher(dispatcher)
--          {
--          }


--          virtual impact.d3.Scalar addSingleResult(impact.d3.Space::LocalConvexResult& convexResult,bool normalInWorldSpace)
--          {
--                  if (convexResult.m_hitCollisionObject == m_me)
--                          return 1.0f;
--
--                  //ignore result if there is no contact response
--                  if(!convexResult.m_hitCollisionObject->hasContactResponse())
--                          return 1.0f;
--
--                  impact.d3.Vector linVelA,linVelB;
--                  linVelA = m_convexToWorld-m_convexFromWorld;
--                  linVelB = impact.d3.Vector(0,0,0);//toB.getOrigin()-fromB.getOrigin();
--
--                  impact.d3.Vector relativeVelocity = (linVelA-linVelB);
--                  //don't report time of impact for motion away from the contact normal (or causes minor penetration)
--                  if (convexResult.m_hitNormalLocal.dot(relativeVelocity)>=-m_allowedPenetration)
--                          return 1.f;
--
--                  return ClosestConvexResultCallback::addSingleResult (convexResult, normalInWorldSpace);
--          }


--          virtual bool needsCollision(impact.d3.collision.Proxy* proxy0) const
--          {
--                  //don't collide with itself
--                  if (proxy0->m_clientObject == m_me)
--                          return false;
--
--                  ///don't do CCD when the collision filters are not matching
--                  if (!ClosestConvexResultCallback::needsCollision(proxy0))
--                          return false;
--
--                  impact.d3.Object* otherObj = (impact.d3.Object*) proxy0->m_clientObject;
--
--                  //call needsResponse, see http://code.google.com/p/bullet/issues/detail?id=179
--                  if (m_dispatcher->needsResponse(m_me,otherObj))
--                  {
--                          return true;
--                  }
--
--                  return false;
--          }
--
--
--  };

   procedure integrateTransforms (Self     : in out Item;
                                  timeStep : in Real)
   is
      use impact.d3.Transform;
      use impact.d3.Vector;
      use math.Vectors;
      use impact.d3.Shape.convex.internal.sphere;

      predictedTrans : Transform_3d;
   begin
      for I in 1 .. Integer (Self.m_nonStaticRigidBodies.Length) loop
         declare
            the_body : constant access impact.d3.Object.rigid.Item := Self.m_nonStaticRigidBodies.Element (I);
            Continue : Boolean := False;
         begin
            the_body.setHitFraction (1.0);

            if        the_body.isActive
              and not the_body.isStaticOrKinematicObject
            then
               the_body.predictIntegratedTransform (timeStep, predictedTrans);

               declare
                  squareMotion : math.Real :=
                    Length2 (getOrigin (predictedTrans) - getOrigin (the_body.getWorldTransform.all));
               begin

                  if         Self.getDispatchInfo.m_useContinuous
                    and then the_body.getCcdSquareMotionThreshold /= 0.0
                    and then the_body.getCcdSquareMotionThreshold < squareMotion
                  then
                     declare
                        sweepResults : aliased btClosestNotMeConvexResultCallback
                          := to_btClosestNotMeConvexResultCallback (the_body,
                                                                    getOrigin (the_body.getWorldTransform.all),
                                                                    getOrigin (predictedTrans),
                                                                    Self.getPairCache,
                                                                    Self.getDispatcher);

                        tmpSphere : constant impact.d3.Shape.convex.internal.sphere.Item
                          := to_sphere_Shape (the_body.getCcdSweptSphereRadius);
                     begin
                        sweepResults.m_allowedPenetration   := Self.getDispatchInfo.m_allowedCcdPenetration;
                        sweepResults.m_collisionFilterGroup := the_body.getBroadphaseProxy.m_collisionFilterGroup;
                        sweepResults.m_collisionFilterMask  := the_body.getBroadphaseProxy.m_collisionFilterMask;

                        declare
                           modifiedPredictedTrans : Transform_3d := predictedTrans;
                           depth                  : Real := 0.0;
                           appliedImpulse         : Real := 0.0;
                           pragma Unreferenced (appliedImpulse);
                        begin
                           setBasis (modifiedPredictedTrans, getBasis (the_body.getWorldTransform.all));
                           Self.convexSweepTest (tmpSphere, the_body.getWorldTransform.all, modifiedPredictedTrans, sweepResults'Access);

                           if         hasHit (sweepResults)
                             and then sweepResults.m_closestHitFraction < 1.0
                           then
                              the_body.setHitFraction (sweepResults.m_closestHitFraction);
                              the_body.predictIntegratedTransform (timeStep * the_body.getHitFraction, predictedTrans);
                              the_body.setHitFraction (0.0);
                              the_body.proceedToTransform (predictedTrans);

                              appliedImpulse := impact.d3.Joint.contact.resolveSingleCollision (the_body,
                                                                                            sweepResults.m_hitCollisionObject,
                                                                                            sweepResults.m_hitPointWorld,
                                                                                            sweepResults.m_hitNormalWorld,
                                                                                            self.getSolverInfo.all,
                                                                                            depth);
                              continue := True;
                           end if;

                        end;
                     end;
                  end if;

               end;

            else
               predictedTrans := math.null_Transform_3d;
            end if;


            if not continue then
               the_body.proceedToTransform (predictedTrans);
            end if;

         end;
      end loop;
   end integrateTransforms;

--  void        impact.d3.Space.dynamic.discrete::integrateTransforms(impact.d3.Scalar timeStep)
--  {
--          BT_PROFILE("integrateTransforms");
--          impact.d3.Transform predictedTrans;
--          for ( int i=0;i<m_nonStaticRigidBodies.size();i++)
--          {
--                  impact.d3.Object.rigid* body = m_nonStaticRigidBodies[i];
--                  body->setHitFraction(1.f);
--
--                  if (body->isActive() && (!body->isStaticOrKinematicObject()))
--                  {
--
--                          body->predictIntegratedTransform(timeStep, predictedTrans);
--
--                          impact.d3.Scalar squareMotion = (predictedTrans.getOrigin()-body->getWorldTransform().getOrigin()).length2();
--
--
--
--                          if (getDispatchInfo().m_useContinuous && body->getCcdSquareMotionThreshold() && body->getCcdSquareMotionThreshold() < squareMotion)
--                          {
--                                  BT_PROFILE("CCD motion clamping");
--                                  if (body->getCollisionShape()->isConvex())
--                                  {
--                                          gNumClampedCcdMotions++;
--  #ifdef USE_STATIC_ONLY
--                                          class StaticOnlyCallback : public btClosestNotMeConvexResultCallback
--                                          {
--                                          public:
--
--                                                  StaticOnlyCallback (impact.d3.Object* me,const impact.d3.Vector& fromA,const impact.d3.Vector& toA,impact.d3.collision.overlapped_pair_Callback.cached* pairCache,impact.d3.Dispatcher* dispatcher) :
--                                                    btClosestNotMeConvexResultCallback(me,fromA,toA,pairCache,dispatcher)
--                                                  {
--                                                  }
--
--                                                    virtual bool needsCollision(impact.d3.collision.Proxy* proxy0) const
--                                                  {
--                                                          impact.d3.Object* otherObj = (impact.d3.Object*) proxy0->m_clientObject;
--                                                          if (!otherObj->isStaticOrKinematicObject())
--                                                                  return false;
--                                                          return btClosestNotMeConvexResultCallback::needsCollision(proxy0);
--                                                  }
--                                          };
--
--                                          StaticOnlyCallback sweepResults(body,body->getWorldTransform().getOrigin(),predictedTrans.getOrigin(),getBroadphase()->getOverlappingPairCache(),getDispatcher());
--  #else
--                                          btClosestNotMeConvexResultCallback sweepResults(body,body->getWorldTransform().getOrigin(),predictedTrans.getOrigin(),getBroadphase()->getOverlappingPairCache(),getDispatcher());
--  #endif
--                                          //impact.d3.Shape.convex* convexShape = static_cast<impact.d3.Shape.convex*>(body->getCollisionShape());
--                                          impact.d3.Shape.convex.internal.sphere tmpSphere(body->getCcdSweptSphereRadius());//impact.d3.Shape.convex* convexShape = static_cast<impact.d3.Shape.convex*>(body->getCollisionShape());
--                                          sweepResults.m_allowedPenetration=getDispatchInfo().m_allowedCcdPenetration;
--
--                                          sweepResults.m_collisionFilterGroup = body->getBroadphaseProxy()->m_collisionFilterGroup;
--                                          sweepResults.m_collisionFilterMask  = body->getBroadphaseProxy()->m_collisionFilterMask;
--                                          impact.d3.Transform modifiedPredictedTrans = predictedTrans;
--                                          modifiedPredictedTrans.setBasis(body->getWorldTransform().getBasis());
--
--                                          convexSweepTest(&tmpSphere,body->getWorldTransform(),modifiedPredictedTrans,sweepResults);
--                                          if (sweepResults.hasHit() && (sweepResults.m_closestHitFraction < 1.f))
--                                          {
--
--                                                  //printf("clamped integration to hit fraction = %f\n",fraction);
--                                                  body->setHitFraction(sweepResults.m_closestHitFraction);
--                                                  body->predictIntegratedTransform(timeStep*body->getHitFraction(), predictedTrans);
--                                                  body->setHitFraction(0.f);
--                                                  body->proceedToTransform( predictedTrans);
--
--                                                  //response  between two dynamic objects without friction, assuming 0 penetration depth
--                                                  impact.d3.Scalar appliedImpulse = 0.f;
--                                                  impact.d3.Scalar depth = 0.f;
--                                                  appliedImpulse = resolveSingleCollision(body,sweepResults.m_hitCollisionObject,sweepResults.m_hitPointWorld,sweepResults.m_hitNormalWorld,getSolverInfo(), depth);
--
--
--                                          continue;
--                                          }
--                                  }
--                          }
--
--
--                          body->proceedToTransform( predictedTrans);
--                  }
--          }
--  }



end impact.d3.Space.dynamic.discrete;










--
--
--
--




--  void        impact.d3.Space.dynamic.discrete::addVehicle(impact.d3.Action* vehicle)
--  {
--          addAction(vehicle);
--  }




--  void        impact.d3.Space.dynamic.discrete::removeVehicle(impact.d3.Action* vehicle)
--  {
--          removeAction(vehicle);
--  }





--  void        impact.d3.Space.dynamic.discrete::addCharacter(impact.d3.Action* character)
--  {
--          addAction(character);
--  }




--  void        impact.d3.Space.dynamic.discrete::removeCharacter(impact.d3.Action* character)
--  {
--          removeAction(character);
--  }






--  ///internal debugging variable. this value shouldn't be too high
--  int gNumClampedCcdMotions=0;







