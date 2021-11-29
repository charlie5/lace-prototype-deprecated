with impact.d3.Collision.Configuration.default,
     impact.d3.Collision.Broadphase.bounding_volume_Tree,

     impact.d3.Dispatcher.collision,
     impact.d3.constraint_Solver.sequential_impulse,

     impact.d3.Space.dynamic.discrete,

     impact.d3.Shape.convex.internal.polyhedral.box,

     impact.d3.Object.rigid,

     impact.d3.motion_State.default,
     impact.d3.Transform,

     ada.Text_IO;


procedure launch_box_box_collision_Test
--
-- This is a test of sphere on sphere collision.
--
is
   use Impact.d3,

       impact.d3.Collision.Configuration.default,
       impact.d3.Transform,
       impact.Math,

       ada.text_IO;



   collisionConfiguration : constant Collision.Configuration.view
     := new_default_Configuration;
   --
   -- Collision configuration contains default setup for memory, collision setup.
   -- Advanced users can create their own configuration.



   -- Use the default collision dispatcher. For parallel processing you can use a diffent dispatcher (see Extras/BulletMultiThreaded).
   --
   the_Dispatcher : constant Dispatcher.collision.view
     := new Dispatcher.collision.item' (Dispatcher.collision.to_Dispatcher (collisionConfiguration.all'unchecked_access));



   -- 'btDbvtBroadphase' is a good general purpose broadphase. You can also try out btAxis3Sweep.
   --
   overlappingPairCache : constant Collision.Broadphase.bounding_volume_Tree.view
     := Collision.Broadphase.bounding_volume_Tree.new_Broadphase;


   -- The default constraint solver. For parallel processing you can use a different solver (see Extras/BulletMultiThreaded)
   --
   solver : constant constraint_Solver.sequential_impulse.view := new constraint_Solver.sequential_impulse.item;



   dynamicsWorld : constant Space.dynamic.discrete.view
     := new Space.dynamic.discrete.item' (Space.dynamic.discrete.Forge.to_Space (the_Dispatcher,
                                                                                 overlappingPairCache,
                                                                                 solver.all'unchecked_access,
                                                                                 collisionConfiguration));


   -- Create a few basic rigid bodies.
   --
   ground_Shape   : constant Shape.convex.internal.polyhedral.box.view
     := new Shape.convex.internal.polyhedral.box.item' (Shape.convex.internal.polyhedral.box.to_box_Shape ((50.0, 1.0, 50.0)));

   box_Shape_1   : constant Shape.convex.internal.polyhedral.box.view
     := new Shape.convex.internal.polyhedral.box.item' (Shape.convex.internal.polyhedral.box.to_box_Shape ((1.0, 1.0, 1.0)));

   box_Shape_2   : constant Shape.convex.internal.polyhedral.box.view
     := new Shape.convex.internal.polyhedral.box.item' (Shape.convex.internal.polyhedral.box.to_box_Shape ((1.0, 1.0, 1.0)));



   ground_motion_State : motion_State.default.view;
   box_motion_State    : motion_State.default.view;

   localInertia        : Vector_3     := (0.0, 0.0, 0.0);
   the_Transform       : Transform_3d := transform.getIdentity;

   unused              : Integer;

begin
   dynamicsWorld.setGravity ((0.0, -10.0, 0.0));


   ground_Shape.calculateLocalInertia (0.0, localInertia);
   setOrigin (the_Transform, (0.0,  -2.0,  0.0));
   ground_motion_State := new motion_State.default.item' (motion_State.default.to_motion_State (the_Transform));

   declare
      rbInfo   : constant Object.rigid.ConstructionInfo_view
        := new Object.rigid.ConstructionInfo' (Object.rigid.to_ConstructionInfo (0.0,  ground_motion_State.all'access,  ground_Shape,  localInertia));

      the_Ground : constant Object.rigid.View := Object.rigid.new_rigid_Object (rbInfo.all);

   begin
      dynamicsWorld.addRigidBody (the_Ground);       -- Add the body to the dynamics world.
   end;



   box_Shape_1.calculateLocalInertia (1.0, localInertia);
   setOrigin (the_Transform, (0.0, 2.501, 0.0));
   setBasis  (the_Transform, impact.linear_Algebra_3d.X_Rotation_from (to_Radians (40.0)));
   box_motion_State := new motion_State.default.item' (motion_State.default.to_motion_State (the_Transform));

   declare
      rbInfo     : constant Object.rigid.ConstructionInfo_view
        := new Object.rigid.ConstructionInfo' (Object.rigid.to_ConstructionInfo (1.0,  box_motion_State.all'access,  box_Shape_1,  localInertia));

      Box_1 : constant impact.d3.Object.rigid.view := Object.rigid.new_rigid_Object (rbInfo.all);

   begin
      dynamicsWorld.addRigidBody (Box_1);     -- Add the body to the dynamics world.
   end;



--     box_Shape_2.calculateLocalInertia (1.0, localInertia);
--     setOrigin (the_Transform, (0.0,  5.0,  0.0));
--     box_motion_State := new motion_State.default.item' (motion_State.default.to_motion_State (the_Transform));
--
--     declare
--        rbInfo   : access Object.rigid.ConstructionInfo
--          := new Object.rigid.ConstructionInfo' (Object.rigid.to_ConstructionInfo (1.0,  box_motion_State.all'access,  box_Shape_2,  localInertia));
--
--        Box_2 : Object.rigid.View := Object.rigid.new_rigid_Object (rbInfo.all);
--
--     begin
--        dynamicsWorld.addRigidBody (Box_2);       -- Add the body to the dynamics world.
--     end;




   --- Do some simulation.
   --
   for i in 1 .. 500
   loop
      unused := dynamicsWorld.stepSimulation (1.0/60.0,  10);

      box_motion_State.getWorldTransform (the_Transform);

      put      ("   X: " & Image (the_Transform.Translation (1), 9));
      put      ("   Y: " & Image (the_Transform.Translation (2), 9));
      put_Line ("   Z: " & Image (the_Transform.Translation (3), 9));
   end loop;


end launch_box_box_collision_Test;
