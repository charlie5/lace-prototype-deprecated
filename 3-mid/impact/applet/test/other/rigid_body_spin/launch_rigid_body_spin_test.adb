with impact.d3.Collision.Configuration.default,
     impact.d3.Collision.Broadphase.bounding_volume_Tree,

     impact.d3.Dispatcher.collision,
     impact.d3.constraint_Solver.sequential_impulse,

     impact.d3.Space.dynamic.discrete,

     impact.d3.Shape.convex.internal.sphere,
     impact.d3.Shape.convex.internal.polyhedral.box,

     impact.d3.Object.rigid,

     impact.d3.motion_State.default,
     impact.d3.Transform,

     ada.text_IO;
with float_math.Algebra.linear.d3;



procedure launch_rigid_body_spin_Test
--
-- This is a test of sphere on sphere collision.
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
   solver : access constraint_Solver.sequential_impulse.item'Class := new constraint_Solver.sequential_impulse.item;



   dynamicsWorld : access Space.dynamic.discrete.item'Class
     := new Space.dynamic.discrete.item' (Space.dynamic.discrete.Forge.to_Space (the_Dispatcher,
                                                                                 overlappingPairCache,
                                                                                 solver.all'access,
                                                                                 collisionConfiguration));


   -- Create a few basic rigid bodies.
   --
   ballShape_1       : Shape.convex.internal.sphere.view
     := new Shape.convex.internal.sphere.item' (Shape.convex.internal.sphere.to_sphere_Shape (1.0));

   the_Ball          : impact.d3.Object.rigid.view;

   ballMotionState   : motion_State.default.view;

   localInertia      : Vector_3     := (0.0, 0.0, 0.0);
   the_Transform     : Transform_3d := transform.getIdentity;

   unused            : Integer;


begin
--     dynamicsWorld.setGravity ((0.0, -10.0, 0.0));


   ballShape_1.calculateLocalInertia (1.0, localInertia);
   setOrigin (the_Transform, (5.0, 5.0, 0.0));
   ballMotionState := new motion_State.default.item' (motion_State.default.to_motion_State (the_Transform));

   declare
      use float_math.Algebra.linear.d3;
      rbInfo     : access Object.rigid.ConstructionInfo
        := new Object.rigid.ConstructionInfo' (Object.rigid.to_ConstructionInfo (0.0,  ballMotionState.all'access,  ballShape_1,  localInertia));


   begin
      the_Ball := Object.rigid.new_rigid_Object (rbInfo.all);
      dynamicsWorld.addRigidBody (the_Ball);     -- Add the body to the dynamics world.

      the_Ball.Spin_is (z_Rotation_from (to_Radians (90.0)));
   end;







   --- Do some simulation.
   --
   unused := dynamicsWorld.stepSimulation (1.0/60.0,  10);

   ballMotionState.getWorldTransform (the_Transform);

   put      ("   X: " & Image (the_Transform.Translation (1), 6));
   put      ("   Y: " & Image (the_Transform.Translation (2), 6));
   put_Line ("   Z: " & Image (the_Transform.Translation (3), 6));


   the_Ball.Site_is ((-5.0, -5.0, 0.0));


   unused := dynamicsWorld.stepSimulation (1.0/60.0,  10);

   ballMotionState.getWorldTransform (the_Transform);

   put      ("   X: " & Image (the_Transform.Translation (1), 6));
   put      ("   Y: " & Image (the_Transform.Translation (2), 6));
   put_Line ("   Z: " & Image (the_Transform.Translation (3), 6));


end launch_rigid_body_spin_Test;
