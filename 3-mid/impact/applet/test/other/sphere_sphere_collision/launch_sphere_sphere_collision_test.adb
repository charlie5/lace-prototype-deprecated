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



procedure launch_sphere_sphere_collision_Test
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
--     ground_Shape   : Shape.convex.internal.sphere.view
--       := new Shape.convex.internal.sphere.item' (Shape.convex.internal.sphere.to_sphere_Shape (1.0));

--     ground_Shape   : Shape.convex.internal.polyhedral.box.view
--       := new impact.d3.Shape.convex.internal.polyhedral.box.item' (impact.d3.Shape.convex.internal.polyhedral.box.to_box_Shape ((10.0, 1.0, 10.0)));

   ballShape_1   : Shape.convex.internal.sphere.view
     := new Shape.convex.internal.sphere.item' (Shape.convex.internal.sphere.to_sphere_Shape (1.0));

   ballShape_2   : Shape.convex.internal.sphere.view
     := new Shape.convex.internal.sphere.item' (Shape.convex.internal.sphere.to_sphere_Shape (1.0));





--     groundMotionState : motion_State.default.view;
   ballMotionState_1 : motion_State.default.view;
   ballMotionState_2 : motion_State.default.view;

   localInertia      : Vector_3     := (0.0, 0.0, 0.0);
   the_Transform     : Transform_3d := transform.getIdentity;

   unused            : Integer;


begin
--     declare
--        function feenableexcept (Val : Integer) return Integer;
--        pragma import (C, feenableexcept);
--
--        Status : Integer;
--     begin
--        null;
--        Status := feenableexcept (16#01#);
--        Status := feenableexcept (16#02#);
--        Status := feenableexcept (16#04#);
--        Status := feenableexcept (16#08#);
--  --      Status := feenableexcept (16#10#);
--  --      Status := feenableexcept (16#20#);
--     end;


   dynamicsWorld.setGravity ((0.0, -10.0, 0.0));
--     dynamicsWorld.setGravity ((0.0, 0.0, 0.0));


--     ground_Shape.calculateLocalInertia (0.0, localInertia);
--     setOrigin (the_Transform, (0.0, 0.0, 0.0));
--     groundMotionState := new motion_State.default.item' (motion_State.default.to_motion_State (the_Transform));
--
--     declare
--        rbInfo     : access Object.rigid.ConstructionInfo
--          := new Object.rigid.ConstructionInfo' (Object.rigid.to_ConstructionInfo (0.0,  groundMotionState.all'access,  ground_Shape,  localInertia));
--
--        the_Ground : impact.d3.Object.rigid.view := Object.rigid.new_rigid_Object (rbInfo.all);
--
--     begin
--        dynamicsWorld.addRigidBody (the_Ground);     -- Add the body to the dynamics world.
--     end;



   ballShape_1.calculateLocalInertia (0.0, localInertia);
   setOrigin (the_Transform, (0.0,  0.0,  0.0));
   ballMotionState_1 := new motion_State.default.item' (motion_State.default.to_motion_State (the_Transform));

   declare
      rbInfo   : access Object.rigid.ConstructionInfo
        := new Object.rigid.ConstructionInfo' (Object.rigid.to_ConstructionInfo (0.0,  ballMotionState_1.all'access,  ballShape_1,  localInertia));

      the_Ball : Object.rigid.View := Object.rigid.new_rigid_Object (rbInfo.all);

   begin
      dynamicsWorld.addRigidBody (the_Ball);       -- Add the body to the dynamics world.
   end;



   ballShape_2.calculateLocalInertia (1.0, localInertia);
   setOrigin (the_Transform, (-0.1,  3.0,  0.0));
   ballMotionState_2 := new motion_State.default.item' (motion_State.default.to_motion_State (the_Transform));

   declare
      rbInfo   : access Object.rigid.ConstructionInfo
        := new Object.rigid.ConstructionInfo' (Object.rigid.to_ConstructionInfo (1.0,  ballMotionState_2.all'access,  ballShape_2,  localInertia));

      the_Ball : Object.rigid.View := Object.rigid.new_rigid_Object (rbInfo.all);

   begin
      dynamicsWorld.addRigidBody (the_Ball);       -- Add the body to the dynamics world.
   end;




   --- Do some simulation.
   --
   for i in 1 .. 100
   loop
      unused := dynamicsWorld.stepSimulation (1.0/60.0,  10);

      ballMotionState_2.getWorldTransform (the_Transform);

      put      ("   X: " & Image (the_Transform.Translation (1), 6));
      put      ("   Y: " & Image (the_Transform.Translation (2), 6));
      put      ("   Z: " & Image (the_Transform.Translation (3), 6));
      put      ("      ");

      ballMotionState_1.getWorldTransform (the_Transform);

      put      ("   X: " & Image (the_Transform.Translation (1), 6));
      put      ("   Y: " & Image (the_Transform.Translation (2), 6));
      put_Line ("   Z: " & Image (the_Transform.Translation (3), 6));

--        new_Line (2);
--        put_Line (Image (the_Transform.Rotation));

--        if the_Transform.Translation (2) >= 3.011087 then
--           put_Line ("H");
--        end if;

   end loop;


end launch_sphere_sphere_collision_Test;
