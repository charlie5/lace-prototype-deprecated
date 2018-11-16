with impact.d3.Space,
     impact.d3.Joint,
     impact.d3.contact_solver_Info,
     impact.d3.constraint_Solver,
     impact.d3.Object.rigid,
     Lace.Any;
with impact.d3.Dispatcher;
with impact.d3.collision.Broadphase;
with impact.d3.collision.Configuration;




package impact.d3.Space.dynamic
is



   type Item is abstract new impact.d3.Space.item with private;
   type View is access all Item'Class;




   --- Forge
   --

   package Forge
   is
      procedure define (Self : out Item;    dispatcher             : access impact.d3.Dispatcher.item'Class;
                                            broadphase             : access impact.d3.collision.Broadphase.item'Class;
                                            collisionConfiguration : access impact.d3.collision.Configuration.item'Class);
   end Forge;



--                  impact.d3.Space.dynamic(impact.d3.Dispatcher* dispatcher,impact.d3.collision.Broadphase* broadphase,impact.d3.collision.Configuration* collisionConfiguration)
--                  :impact.d3.Space(dispatcher,broadphase,collisionConfiguration), m_internalTickCallback(0),m_internalPreTickCallback(0), m_worldUserInfo(0)
--                  {
--                  }


   procedure destruct (Self : in out Item)   is null;





   --- Attributes
   --



   --  Type for the callback for each tick
   --
   type  btInternalTickCallback is access procedure (world    : access impact.d3.Space.dynamic.item'Class;
                                                     timeStep : in     math.Real);


   type Kind is (simple_Kind,
                 discrete_Kind,
                 Continuous,
                 Soft_Rigid);


   for Kind use (simple_Kind     => 1,
                 discrete_Kind   => 2,
                 Continuous      => 3,
                 Soft_Rigid      => 4);

   --  You can disable subdividing the timestep/substepping by passing maxSubSteps=0 as second argument to stepSimulation, but in that case you have to keep the timeStep constant.
   --  virtual int stepSimulation( impact.d3.Scalar timeStep,int maxSubSteps=1, impact.d3.Scalar fixedTimeStep=impact.d3.Scalar(1.)/impact.d3.Scalar(60.))=0;

   function stepSimulation (Self          : access Item;
                            timeStep      : Real;
                            maxSubSteps   : Integer := 1;
                            fixedTimeStep : Real := Real (1.0) / Real (60.0)) return Integer is abstract;




   --  virtual void        addConstraint(impact.d3.Joint* constraint, bool disableCollisionsBetweenLinkedBodies=false)

   procedure addConstraint (Self                                 : in out Item;
                            constraint                           : access impact.d3.Joint.Item'Class;
                            disableCollisionsBetweenLinkedBodies : Boolean                           );


   --  virtual void        removeConstraint(impact.d3.Joint* constraint) {(void)constraint;}

   procedure removeConstraint (Self       : in out Item;
                               constraint : access impact.d3.Joint.Item'Class);




   --- tbd: leave these for the time being (rak) ...
   --
--     -- virtual void        addAction(impact.d3.Action* action) = 0;
--
--     procedure addAction (Self   : in out Item;
--                          action : access impact.d3.Action.Item'Class) is abstract;
--
--     -- virtual void        removeAction(impact.d3.Action* action) = 0;
--
--     procedure removeAction (Self   : in out Item;
--                             action : access impact.d3.Action.Item'Class) is abstract;





   --  once a rigidbody is added to the dynamics world, it will get this gravity assigned
   --  existing rigidbodies in the world get gravity assigned too, during this method
   --  virtual void        setGravity(const impact.d3.Vector& gravity) = 0;

   procedure setGravity (Self    : in out Item;
                         gravity : Vector_3 ) is abstract;

   --  virtual impact.d3.Vector getGravity () const = 0;

   function getGravity (Self : Item) return Vector_3 is abstract;

   --  virtual void        synchronizeMotionStates() = 0;

   procedure synchronizeMotionStates (Self : in out Item) is abstract;




   --- Rigid bodies.
   --

   --  virtual void        addRigidBody(impact.d3.Object.rigid* body) = 0;4а

   procedure addRigidBody (Self     : in out Item;
                           The_Body : access impact.d3.Object.rigid.Item'Class) is abstract;

   --  virtual void        addRigidBody(impact.d3.Object.rigid* body, short group, short mask) = 0;

   procedure addRigidBody (The_Body    : access impact.d3.Object.rigid.Item'Class;
                           group, mask : in     Flags                          ) is abstract;

   --  virtual void        removeRigidBody(impact.d3.Object.rigid* body) = 0;

   procedure removeRigidBody (The_Body : access impact.d3.Object.rigid.Item'Class) is abstract;

   --  virtual void        setConstraintSolver(impact.d3.constraint_Solver* solver) = 0






   --- Constraints (ie Joints).
   --

   procedure setConstraintSolver (solver : access impact.d3.constraint_Solver.Item'Class) is abstract;

   --  virtual impact.d3.constraint_Solver* getConstraintSolver() = 0;

   function getConstraintSolver (Self : Item) return access impact.d3.constraint_Solver.Item'Class is abstract;


   --  virtual        int                getNumConstraints() const {        return 0;                }

   function getNumConstraints (Self : Item) return Integer;

   --  virtual impact.d3.Joint* getConstraint(int index)                {        (void)index;                return 0;                }

   function getConstraint (Self  : Item;
                           index : Integer) return access impact.d3.Joint.Item'Class;



   --  virtual impact.d3.Space.dynamicType        getWorldType() const=0;

   function getWorldType (Self : in Item) return impact.d3.Space.dynamic.Kind is abstract;

   --  virtual void        clearForces() = 0;

   procedure clearForces (Self : in out Item) is abstract;

   --  void setInternalTickCallback(btInternalTickCallback cb,        void* worldUserInfo=0,bool isPreTick=false)

   procedure setInternalTickCallback (Self          : in out Item;
                                      cb            : in     btInternalTickCallback;
                                      worldUserInfo : access lace.Any.item'Class   := null;
                                      isPreTick     : in     Boolean               := False);

   --  void        setWorldUserInfo(void* worldUserInfo)

   procedure setWorldUserInfo (Self          : in out Item;
                               worldUserInfo : access lace.Any.item'Class);

   --  void*        getWorldUserInfo() const

   procedure getWorldUserInfo (Self : in out Item);






   --- 'Protected'
   --

   function m_internalTickCallback    (Self : in     Item) return btInternalTickCallback;
   function m_internalPreTickCallback (Self : in     Item) return btInternalTickCallback;

   function getSolverInfo             (Self : access Item) return access impact.d3.contact_solver_Info.item;



private

   type Item is abstract new impact.d3.Space.item with
      record
         m_internalTickCallback    : btInternalTickCallback;
         m_internalPreTickCallback : btInternalTickCallback;
         m_worldUserInfo           : access lace.Any.item'Class;
         m_solverInfo              : aliased impact.d3.contact_solver_Info.item;
      end record;


end impact.d3.Space.dynamic;





--  #include "BulletCollision/CollisionDispatch/impact.d3.Space.h"
--  #include "BulletDynamics/ConstraintSolver/impact.d3.contact_solver_Info.h"


--  class impact.d3.Joint;
--  class impact.d3.Action;
--  class impact.d3.constraint_Solver;
--  class impact.d3.Space.dynamic;






--  enum impact.d3.Space.dynamicType
--  {
--          BT_SIMPLE_DYNAMICS_WORLD=1,
--          BT_DISCRETE_DYNAMICS_WORLD=2,
--          BT_CONTINUOUS_DYNAMICS_WORLD=3,
--          BT_SOFT_RIGID_DYNAMICS_WORLD=4
--  };




--  ///The impact.d3.Space.dynamic is the interface class for several dynamics implementation, basic, discrete, parallel, and continuous etc.
--
--  class impact.d3.Space.dynamic : public impact.d3.Space
--  {
--
--  protected:
--                  btInternalTickCallback m_internalTickCallback;
--                  btInternalTickCallback m_internalPreTickCallback;
--                  void*        m_worldUserInfo;
--
--                  impact.d3.contact_solver_Info        m_solverInfo;
--
--  public:
--
--
--
--                  ///stepSimulation proceeds the simulation over 'timeStep', units in preferably in seconds.
--                  ///By default, Bullet will subdivide the timestep in constant substeps of each 'fixedTimeStep'.
--                  ///in order to keep the simulation real-time, the maximum number of substeps can be clamped to 'maxSubSteps'.
--                  ///You can disable subdividing the timestep/substepping by passing maxSubSteps=0 as second argument to stepSimulation, but in that case you have to keep the timeStep constant.
--                  virtual int                stepSimulation( impact.d3.Scalar timeStep,int maxSubSteps=1, impact.d3.Scalar fixedTimeStep=impact.d3.Scalar(1.)/impact.d3.Scalar(60.))=0;
--
--                  virtual void        debugDrawWorld() = 0;
--
--                  virtual void        addConstraint(impact.d3.Joint* constraint, bool disableCollisionsBetweenLinkedBodies=false)
--                  {
--                          (void)constraint; (void)disableCollisionsBetweenLinkedBodies;
--                  }
--
--                  virtual void        removeConstraint(impact.d3.Joint* constraint) {(void)constraint;}
--
--                  virtual void        addAction(impact.d3.Action* action) = 0;
--
--                  virtual void        removeAction(impact.d3.Action* action) = 0;
--
--                  //once a rigidbody is added to the dynamics world, it will get this gravity assigned
--                  //existing rigidbodies in the world get gravity assigned too, during this method
--                  virtual void        setGravity(const impact.d3.Vector& gravity) = 0;
--                  virtual impact.d3.Vector getGravity () const = 0;
--
--                  virtual void        synchronizeMotionStates() = 0;
--
--                  virtual void        addRigidBody(impact.d3.Object.rigid* body) = 0;
--
--                  virtual void        addRigidBody(impact.d3.Object.rigid* body, short group, short mask) = 0;
--
--                  virtual void        removeRigidBody(impact.d3.Object.rigid* body) = 0;
--
--                  virtual void        setConstraintSolver(impact.d3.constraint_Solver* solver) = 0;
--
--                  virtual impact.d3.constraint_Solver* getConstraintSolver() = 0;
--
--                  virtual        int                getNumConstraints() const {        return 0;                }
--
--                  virtual impact.d3.Joint* getConstraint(int index)                {        (void)index;                return 0;                }
--
--                  virtual const impact.d3.Joint* getConstraint(int index) const        {        (void)index;        return 0;        }
--
--                  virtual impact.d3.Space.dynamicType        getWorldType() const=0;
--
--                  virtual void        clearForces() = 0;
--
--                  /// Set the callback for when an internal tick (simulation substep) happens, optional user info
--                  void setInternalTickCallback(btInternalTickCallback cb,        void* worldUserInfo=0,bool isPreTick=false)
--                  {
--                          if (isPreTick)
--                          {
--                                  m_internalPreTickCallback = cb;
--                          } else
--                          {
--                                  m_internalTickCallback = cb;
--                          }
--                          m_worldUserInfo = worldUserInfo;
--                  }
--
--                  void        setWorldUserInfo(void* worldUserInfo)
--                  {
--                          m_worldUserInfo = worldUserInfo;
--                  }
--
--                  void*        getWorldUserInfo() const
--                  {
--                          return m_worldUserInfo;
--                  }
--
--                  impact.d3.contact_solver_Info& getSolverInfo()
--                  {
--                          return m_solverInfo;
--                  }
--
--
--                  ///obsolete, use addAction instead.
--                  virtual void        addVehicle(impact.d3.Action* vehicle) {(void)vehicle;}
--                  ///obsolete, use removeAction instead
--                  virtual void        removeVehicle(impact.d3.Action* vehicle) {(void)vehicle;}
--                  ///obsolete, use addAction instead.
--                  virtual void        addCharacter(impact.d3.Action* character) {(void)character;}
--                  ///obsolete, use removeAction instead
--                  virtual void        removeCharacter(impact.d3.Action* character) {(void)character;}
--
--
--  };
