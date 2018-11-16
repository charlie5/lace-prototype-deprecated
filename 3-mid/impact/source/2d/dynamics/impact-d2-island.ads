with impact.d2.Math,
     impact.d2.Solid,
     impact.d2.Contact.Solver,
     impact.d2.Joint,
     impact.d2.world_Callbacks,
     impact.d2.Types;


package impact.d2.Island
--
--
--
is
   use impact.d2.Math,
       impact.d2.Types;



--     --  This is an internal structure.
--     --
--     type b2Position is
--        record
--           x : b2Vec2;
--           a : float32;
--        end record;
--
--
--
--     --  This is an internal structure.
--     --
--     type b2Velocity is
--        record
--           v : b2Vec2;
--           w : float32;
--        end record;


   type Solid_view   is access all Solid.b2Body'Class;
   type Joint_view   is access all Joint.b2Joint'Class;

--     type Position_view is access all b2Position;
--     type Velocity_view is access all b2Velocity;


   type Solid_views   is array (int32 range <>) of Solid_view;
   type Joint_views   is array (int32 range <>) of Joint_view;

--     type Position_views is array (int32 range <>) of Position_view;
--     type Velocity_views is array (int32 range <>) of Velocity_view;


   type access_Solid_views   is access all Solid_views;
   type access_Contact_views is access all Contact.views;
   type access_Joint_views   is access all Joint_views;

--     type access_Position_views is access all Position_views;
--     type access_Velocity_views is access all Velocity_views;



   --  This is an internal class.
   --
   type b2Island is tagged
      record
         --          b2StackAllocator* m_allocator;
         m_listener         : access world_callbacks.b2ContactListener;

         m_bodies           : access_Solid_views;
         m_contacts         : access_Contact_views;
         m_joints           : access_Joint_views;

         m_positions        : access_Position_views;
         m_velocities       : access_Velocity_views;

         m_bodyCount,
         m_jointCount,
         m_contactCount     : int32;

         m_bodyCapacity,
         m_contactCapacity,
         m_jointCapacity    : int32;

         m_positionIterationCount : int32;
      end record;


   function to_b2Island (bodyCapacity    : int32;
                         contactCapacity : int32;
                         jointCapacity   : int32;
                         --                          b2StackAllocator* allocator,
                         listener        : access world_callbacks.b2ContactListener'Class) return b2Island;

   procedure destruct (Self : in out b2Island);

   procedure Clear    (Self : in out b2Island);

--     procedure Solve    (Self : in out b2Island;   step       : in b2TimeStep;
--                                                   gravity    : in b2Vec2;
--                                                   allowSleep : in Boolean);

--     procedure SolveTOI (Self : in out b2Island;   subStep    : in b2TimeStep;
--                                                   toiIndexA,
--                                                   toiIndexB  : in int32 );

   procedure Add (Self : in out b2Island;   Solid       : access impact.d2.Solid.b2Body'Class);
   procedure Add (Self : in out b2Island;   Contact     : access impact.d2.Contact.b2Contact'Class);
   procedure Add (Self : in out b2Island;   Joint       : access impact.d2.Joint.b2Joint'Class);

--     procedure Report (Self : in out b2Island;   constraints : access contact.solver.b2ContactConstraints);


--  {
--  public:
--          b2Island(int32 bodyCapacity, int32 contactCapacity, int32 jointCapacity,
--                          b2StackAllocator* allocator, b2ContactListener* listener);
--          ~b2Island();
--
--          void Clear()
--          {
--                  m_bodyCount = 0;
--                  m_contactCount = 0;
--                  m_jointCount = 0;
--          }
--
--          void Solve(const b2TimeStep& step, const b2Vec2& gravity, bool allowSleep);
--
--          void Add(b2Body* body)
--          {
--                  b2Assert(m_bodyCount < m_bodyCapacity);
--                  body->m_islandIndex = m_bodyCount;
--                  m_bodies[m_bodyCount++] = body;
--          }
--
--          void Add(b2Contact* contact)
--          {
--                  b2Assert(m_contactCount < m_contactCapacity);
--                  m_contacts[m_contactCount++] = contact;
--          }
--
--          void Add(b2Joint* joint)
--          {
--                  b2Assert(m_jointCount < m_jointCapacity);
--                  m_joints[m_jointCount++] = joint;
--          }
--
--          void Report(const b2ContactConstraint* constraints);
--  };
--
--  #endif

end impact.d2.Island;
