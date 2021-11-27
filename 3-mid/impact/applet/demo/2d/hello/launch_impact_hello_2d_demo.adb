with impact.d2.World,
     impact.d2.Solid,
     impact.d2.Shape.polygon,
     impact.d2.Fixture,
     impact.d2.Math,

     ada.text_IO;



procedure launch_impact_hello_2d_Demo
--   This is a simple example of building and running a simulation
--   using Box2D. Here we create a large ground box and a small dynamic box.
--
--   There are no graphics for this example. Box2D is meant to be used
--   with your rendering engine in your game engine.
is
   use impact.d2,
       impact.d2.World,
       impact.d2.Solid,
       impact.d2.Shape.polygon,
       impact.d2.Math,

       ada.text_IO;


   type Solid_view is access all b2Body'Class;
   type World_view is access all b2World'Class;



   --- World
   --
   gravity       : constant b2Vec2     := (0.0, -10.0);
   doSleep       : Boolean    := True;                                           -- Do we want to let bodies sleep ?
   world         : constant World_view := new b2World' (to_b2World (gravity));   -- Construct a world object, which will hold and simulate the rigid bodies.


   --- static Terrain
   --
   groundBox     : aliased b2PolygonShape := to_Polygon;                                     -- Define the ground box shape.
   groundBodyDef : constant b2BodyDef              := (position => (0.0, -10.0),                      -- Define the ground body.
                                              others   => <>          );
   groundBody    : constant Solid_view             := Solid_view (world.createBody (groundBodyDef));
   --
   -- Call the body factory which allocates memory for the ground body
   -- from a pool and creates the ground box shape (also from a pool).
   -- The body is also added to the world.



   --- dyamic Box
   --
   dynamicBox : aliased b2PolygonShape     := to_Polygon;                                     -- Define another box shape for our dynamic body.
   fixtureDef : constant fixture.b2FixtureDef := (shape    => dynamicBox'unchecked_access,       -- Define the dynamic body fixture.
                                               density  => 1.0,                               -- Set the box density to be non-zero, so it will be dynamic.
                                               friction => 0.3,                               -- Override the default friction.
                                               restitution => 0.0,
                                               others   => <>);
   solidDef   : constant b2BodyDef                  := (position => (0.0, 4.0),
                                               kind     => b2_dynamicBody,
                                               others   => <>            );
   the_solid  : constant Solid_view                 := Solid_view (world.createBody (solidDef));        -- Define the dynamic body. We set its position and call the body factory.


   --- misc
   --
   unused : Fixture.view;


begin
   groundBox.SetAsBox (50.0, 10.0);                                         -- The extents are the half-widths of the box.
   unused := groundBody.CreateFixture (groundBox'unchecked_access, 0.0);    -- Add the ground fixture to the ground body.

   dynamicBox.SetAsBox (1.0, 1.0);                                          -- The extents are the half-widths of the box.
   unused := the_solid.CreateFixture (fixtureDef);                          -- Add the shape to the body..


   -- Prepare for simulation. Typically we use a time step of 1/60 of a
   -- second (60Hz) and 10 iterations. This provides a high quality simulation
   -- in most game scenarios.
   --
   declare
      timeStep           : constant float32 := 1.0 / 60.0;
      velocityIterations : constant int32   := 6;
      positionIterations : constant int32   := 2;
   begin
      -- This is our little game loop.
      --
      for i in 1 .. 60 loop
         -- Instruct the world to perform a single step of simulation.
         -- It is generally best to keep the time step and iterations fixed.
         --
         world.Step (timeStep,  velocityIterations, positionIterations);

         -- Clear applied body forces. We didn't apply any forces, but you
         -- should know about this function.
         --
         World.clearForces;

         -- Now print the position and angle of the body.
         put_Line (  "x: "        & float32'Image (the_Solid.getPosition.x)
                   & "   y: "     & float32'Image (the_Solid.getPosition.y)
                   & "   angle: " & float32'Image (the_Solid.getAngle));

      end loop;

   end;


   World.destruct;    -- When the world destructor is called, all bodies and joints are freed. This can
                      -- create orphaned pointers, so be careful about your world management.
end launch_impact_hello_2d_Demo;
