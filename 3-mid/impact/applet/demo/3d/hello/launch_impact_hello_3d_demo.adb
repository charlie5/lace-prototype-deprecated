with
     impact.d3.Collision.Configuration.default,
     impact.d3.Collision.Broadphase.bounding_volume_Tree,

     impact.d3.Dispatcher.collision,
     impact.d3.constraint_Solver.sequential_impulse,

     impact.d3.Space.dynamic.discrete,

     impact.d3.Shape.convex.internal.sphere,
     impact.d3.Shape.convex.internal.polyhedral.box,

     impact.d3.Object.rigid,

     impact.d3.motion_State.default,
     impact.d3.Transform,

     ada.Text_IO;


procedure launch_impact_hello_3d_Demo
--
--  A simple 'Hello World' style program, demonstrating a basic Impact physics simulation.
--
--  The simulation drops a dynamic ball onto a static box.
--
is
   use Impact.d3,

       impact.d3.Collision.Configuration.default,
       impact.d3.Transform,
       impact.Math,

       ada.text_IO;


   collisionConfiguration : access Collision.Configuration.item'Class
     := new_default_Configuration;
   --
   -- Collision configuration contains default setup for memory, collision setup.
   -- Advanced users can create their own configuration.



   -- Use the default collision dispatcher. For parallel processing you can use a diffent dispatcher (see Extras/BulletMultiThreaded).
   --
   the_Dispatcher : access Dispatcher.collision.item
     := new Dispatcher.collision.item' (Dispatcher.collision.to_Dispatcher (collisionConfiguration.all'unchecked_access));



   -- 'btDbvtBroadphase' is a good general purpose broadphase. You can also try out btAxis3Sweep.
   --
   overlappingPairCache : Collision.Broadphase.bounding_volume_Tree.view
     := Collision.Broadphase.bounding_volume_Tree.new_Broadphase;


   -- The default constraint solver. For parallel processing you can use a different solver (see Extras/BulletMultiThreaded)
   --
   solver : access constraint_Solver.sequential_impulse.item'Class
     := new constraint_Solver.sequential_impulse.item;

   dynamicsWorld : access Space.dynamic.discrete.item'Class
     := new Space.dynamic.discrete.item' (Space.dynamic.discrete.Forge.to_Space (the_Dispatcher,
                                                                                 overlappingPairCache,
                                                                                 solver.all'access,
                                                                                 collisionConfiguration));

   -- Create a few basic rigid bodies.
   --
   groundShape : Shape.convex.internal.polyhedral.box.view
     := new Shape.convex.internal.polyhedral.box.item' (Shape.convex.internal.polyhedral.box.to_box_Shape ((50.0, 50.0, 50.0)));

   ballShape   : Shape.convex.internal.sphere.view
     := new Shape.convex.internal.sphere.item' (Shape.convex.internal.sphere.to_sphere_Shape (1.0));



   groundMotionState : motion_State.default.view;
   ballMotionState   : motion_State.default.view;

   localInertia      : Vector_3     := (0.0, 0.0, 0.0);
   the_Transform     : Transform_3d := transform.getIdentity;

   unused            : Integer;

begin
   dynamicsWorld.setGravity ((0.0, -10.0, 0.0));


   groundShape.calculateLocalInertia (0.0, localInertia);
   setOrigin (the_Transform, (0.0, -56.0, 0.0));
   groundMotionState := new motion_State.default.item' (motion_State.default.to_motion_State (the_Transform));

   declare
      rbInfo     : access Object.rigid.ConstructionInfo
        := new Object.rigid.ConstructionInfo' (Object.rigid.to_ConstructionInfo (0.0,  groundMotionState.all'access,  groundShape,  localInertia));

      the_Ground : impact.d3.Object.rigid.view := Object.rigid.new_rigid_Object (rbInfo.all);

   begin
      dynamicsWorld.addRigidBody (the_Ground);     -- Add the body to the dynamics world.
   end;


   ballShape.calculateLocalInertia (1.0, localInertia);
   setOrigin (the_Transform, (2.0,  -0.0,  0.0));
   ballMotionState := new motion_State.default.item' (motion_State.default.to_motion_State (the_Transform));

   declare
      rbInfo : access Object.rigid.ConstructionInfo
        := new Object.rigid.ConstructionInfo'
          (Object.rigid.to_ConstructionInfo
               (1.0,
                ballMotionState.all'access,
                ballShape,
                localInertia));

      the_Ball : Object.rigid.View
        := Object.rigid.new_rigid_Object (rbInfo.all);
   begin
      dynamicsWorld.addRigidBody (the_Ball);       -- Add the body to the dynamics world.
--        the_Ball.setSleepingThresholds (0.0, 0.0);
   end;


   --  Do some simulation.
   --
   for i in 1 .. 100
   loop
      unused := dynamicsWorld.stepSimulation (1.0/60.0,  10);

      ballMotionState.getWorldTransform (the_Transform);

      put      ("   X: " & Image (the_Transform.Translation (1), 6));
      put      ("   Y: " & Image (the_Transform.Translation (2), 6));
      put_Line ("   Z: " & Image (the_Transform.Translation (3), 6));
   end loop;


   -- todo: ...
   --

--  	//cleanup in the reverse order of creation/initialization
--
--  	//remove the rigidbodies from the dynamics world and delete them
--  	for (i=dynamicsWorld->getNumCollisionObjects()-1; i>=0 ;i--)
--  	{
--  		btCollisionObject* obj = dynamicsWorld->getCollisionObjectArray()[i];
--  		btRigidBody* body = btRigidBody::upcast(obj);
--  		if (body && body->getMotionState())
--  		{
--  			delete body->getMotionState();
--  		}
--  		dynamicsWorld->removeCollisionObject( obj );
--  		delete obj;
--  	}
--
--  	//delete collision shapes
--  	for (int j=0;j<collisionShapes.size();j++)
--  	{
--  		btCollisionShape* shape = collisionShapes[j];
--  		collisionShapes[j] = 0;
--  		delete shape;
--  	}
--
--  	//delete dynamics world
--  	delete dynamicsWorld;
--
--  	//delete solver
--  	delete solver;
--
--  	//delete broadphase
--  	delete overlappingPairCache;
--
--  	//delete dispatcher
--  	delete dispatcher;
--
--  	delete collisionConfiguration;
--
--  	//next line is optional: it will be cleared by the destructor when the array goes out of scope
--  	collisionShapes.clear();

end launch_impact_hello_3d_Demo;
