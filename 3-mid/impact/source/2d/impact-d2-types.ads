with
     impact.d2.Math;


package Impact.d2.Types
--
--  Internal types.
--
is

   use impact.d2.Math;



   --  This is an internal structure.
   --
   type b2Position is
      record
         c : b2Vec2;
         a : float32;
      end record;



   --  This is an internal structure.
   --
   type b2Velocity is
      record
         v : b2Vec2;
         w : float32;
      end record;


   type Position_view is access all b2Position;
   type Velocity_view is access all b2Velocity;


   type Position_views is array (int32 range <>) of Position_view;
   type Velocity_views is array (int32 range <>) of Velocity_view;


   type access_Position_views is access all Position_views;
   type access_Velocity_views is access all Velocity_views;


end Impact.d2.Types;
