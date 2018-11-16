with impact.d3.collision.Detector.discrete,
     impact.d3.Shape.convex.internal.sphere,
     impact.d3.Shape.convex.internal.polyhedral.triangle;



package impact.d3.collision.Detector.discrete.sphere_triangle
--
--  Sphere-triangle to match the impact.d3.collision.Detector.discrete.
--
is
   use impact.d3.collision.Detector.discrete;


   type Item is new impact.d3.collision.Detector.discrete.item with private;



   function to_sphere_triangle_Detector (sphere                   : access impact.d3.Shape.convex.internal.sphere  .Item'Class;
                                         triangle                 : access impact.d3.Shape.convex.internal.polyhedral.triangle.Item'Class;
                                         contactBreakingThreshold : in     math.Real               ) return Item;





   overriding procedure getClosestPoints (Self : in out Item;   input       : in     ClosestPointInput;
                                                     output      : in out Result'Class;
                                                     swapResults : in     Boolean          := False);


   function collide (Self : in Item;   sphereCenter             : in     math.Vector_3;
                                       point                    : access math.Vector_3;
                                       resultNormal             : access math.Vector_3;
                                       depth                    : access math.Real;
                                       timeOfImpact             : access math.Real;
                                       contactBreakingThreshold : in     math.Real) return Boolean;






private


   type Item is new impact.d3.collision.Detector.discrete.item with
      record
         m_sphere   : access impact.d3.Shape.convex.internal.sphere  .Item'Class;
         m_triangle : access impact.d3.Shape.convex.internal.polyhedral.triangle.Item'Class;

         m_contactBreakingThreshold : math.Real;
      end record;



   function pointInTriangle (Self : in Item;   vertices : in     Vector_3_array;
                                               normal   : in     math.Vector_3;
                                               p        : access math.Vector_3) return Boolean;

   function facecontains    (Self : in Item;   p        : in     math.Vector_3;
                                               vertices : in     Vector_3_array;
                                               normal   : access math.Vector_3) return Boolean;

end impact.d3.collision.Detector.discrete.sphere_triangle;
