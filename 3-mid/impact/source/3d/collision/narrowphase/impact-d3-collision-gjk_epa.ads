with impact.d3.Shape.convex;

--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.h"


package impact.d3.collision.gjk_epa
--
--  GJK-EPA collision solver by Nathanael Presson, 2008
--
is
   use Math;




   --  btGjkEpaSolver contributed under zlib by Nathanael Presson
   --
   package btGjkEpaSolver2
   is


      type eStatus is (Separated,         -- Shapes doesnt penetrate
                       Penetrating,       -- Shapes are penetrating
                       GJK_Failed,        -- GJK phase fail, no big issue, shapes are probably just 'touching'
                       EPA_Failed);       -- EPA phase fail, bigger problem, need to save parameters, and debug

      status : eStatus;


      type Witnesses is array (1 .. 2) of math.Vector_3;


      type sResults is
         record
            witnesses : btGjkEpaSolver2.Witnesses;
            normal    : math.Vector_3;
            distance  : math.Real;
            status    : eStatus;
         end record;




      function StackSizeRequirement return Integer;


      function Distance (shape0  : in     impact.d3.Shape.convex.view;
                         wtrs0   : in     Transform_3d;
                         shape1  : in     impact.d3.Shape.convex.view;
                         wtrs1   : in     Transform_3d;
                         guess   : in     math.Vector_3;
                         results : access sResults) return Boolean;


      function Penetration (shape0     : in     impact.d3.Shape.convex.view;
                            wtrs0      : in     Transform_3d;
                            shape1     : in     impact.d3.Shape.convex.view;
                            wtrs1      : in     Transform_3d;
                            guess      : in     math.Vector_3;
                            results    : access sResults;
                            usemargins : in     Boolean := True) return Boolean;


      function SignedDistance (position  : in     math.Vector_3;
                               margin    : in     math.Real;
                               shape0    : in     impact.d3.Shape.convex.view;
                               wtrs0     : in     Transform_3d;
                               results   : access sResults) return math.Real;


      function SignedDistance (shape0     : in     impact.d3.Shape.convex.view;
                               wtrs0      : in     Transform_3d;
                               shape1     : in     impact.d3.Shape.convex.view;
                               wtrs1      : in     Transform_3d;
                               guess      : in     math.Vector_3;
                               results    : access sResults) return Boolean;

   end btGjkEpaSolver2;



end impact.d3.collision.gjk_epa;
