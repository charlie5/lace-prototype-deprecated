with impact.d3.Containers,
     impact.d3.manifold_Point,
     impact.d3.Containers,

     ada.containers.Vectors;



package impact.d3.Manifold
--
--  impact.d3.Manifold is a contact point cache, it stays persistent as long as objects are overlapping in the broadphase.
--
--  Those contact points are created by the collision narrow phase.
--
--  The cache can be empty, or hold 1,2,3 or 4 points. Some collision algorithms (GJK) might only add one point at a time.
--  updates/refreshes old contact points, and throw them away if necessary (distance becomes too large)
--  reduces the cache to 4 points. When more then 4 points are added, using following rules:
--
--     - The contact point with deepest penetration is always kept, and it tries to maximuze the area covered by the points
--
--  Note that some pairs of objects might have more then one contact manifold.
--
is
   use manifold_Point, Math;



   gContactBreakingThreshold : math.Real := 0.02;   -- maximum contact breaking and merging threshold



   type ContactDestroyedCallback is access function (userPersistentData : in Containers.Any_view) return Boolean;

   type ContactProcessedCallback is access function (cp : in manifold_Point.item'Class;   body0,
                                                                                          body1 : in Containers.Any_view) return Boolean;

   gContactDestroyedCallback : ContactDestroyedCallback;
   gContactProcessedCallback : ContactProcessedCallback;


   type btContactManifoldTypes is  (MIN_CONTACT_MANIFOLD_TYPE, BT_PERSISTENT_MANIFOLD_TYPE);

   for  btContactManifoldTypes use (MIN_CONTACT_MANIFOLD_TYPE   => 1024,      -- the enum starts at 1024 to avoid type conflicts with impact.d3.Joint
                                    BT_PERSISTENT_MANIFOLD_TYPE => 1025);



   MANIFOLD_CACHE_SIZE : constant := 4;




   type Public is tagged
      record
         m_companionIdA,
         m_companionIdB,

         m_index1a     : Integer := 0;
      end record;


   type Item is new Public with private;
   type View is access all Item'Class;






   --- Containers
   --
   type    Views   is array (Positive range <>) of View;


   package Vectors is new ada.Containers.Vectors (Positive, View);
   subtype Vector  is     Vectors.Vector;


--     type Vector is tagged private;
--
--     function Length (Self : in Vector) return ada.Containers.Count_Type;
--
--     procedure append (Self : in out Vector;   New_Item  : impact.d3.Manifold.Item'Class;
--                                               Count     : Ada.Containers.Count_Type := 1);



   --- Forge
   --

   function to_Manifold (body0,
                                     body1                      : in     Containers.Any_view;
                                     unused                     : in     Integer;
                                     contactBreakingThreshold   : in     Real;
                                     contactProcessingThreshold : in     Real) return Item;

   procedure destruct (Self : in out Item);





   --- Attributes
   --

   function  getBody0 (Self : in Item) return  Containers.Any_view;
   function  getBody1 (Self : in Item) return  Containers.Any_view;

   procedure setBodies (Self : in out Item;   body0,
                                              body1 : in Containers.Any_view);

   procedure clearUserCache (Self : in out Item;   pt : access manifold_Point.item'Class);


   function  getNumContacts (Self : in Item) return Integer;

   function  getContactPoint (Self : in     Item;   index : in Integer) return        manifold_Point.item'Class;
   function  getContactPoint (Self : access Item;   index : in Integer) return access manifold_Point.item'Class;

   function  getContactBreakingThreshold   (Self : in Item) return Real;    -- todo: get this margin from the current physics/collision environment
   function  getContactProcessingThreshold (Self : in Item) return Real;

   function  getCacheEntry (Self : in Item;   newPoint : in manifold_Point.item'Class) return Integer;

   function  addManifoldPoint (Self : access Item;   newPoint : in manifold_Point.item) return Integer;


   procedure removeContactPoint  (Self : in out Item;   index       : in Integer);
   procedure replaceContactPoint (Self : in out Item;   newPoint    : in manifold_Point.item;
                                                                        insertIndex : in Integer);

   procedure refreshContactPoints (Self : in out Item;   trA, trB : in Transform_3d);   -- calculated new worldspace coordinates and depth, and reject points that exceed the collision margin


   procedure clearManifold  (Self : in out Item);





private




   type Item is new Public with
      record
         m_pointCache                 : manifold_Points (1 .. MANIFOLD_CACHE_SIZE) := (others => to_manifold_Point);

         m_body0,                                               -- These two body pointers can point to the physics rigidbody class.
         m_body1                      : Containers.Any_view;       -- Access to Any'Class will allow any rigidbody class.

         m_cachedPoints               : Integer := 0;

         m_contactBreakingThreshold,
         m_contactProcessingThreshold : Real;
      end record;


   function sortCachedPoints (Self : in Item;   pt : in manifold_Point.item'Class) return Integer;   -- sort cached points so most isolated points come first


--     function findContactPoint (Self : in impact.d3.Manifold;   unUsed    : access impact.d3.manifold_Point.impact.d3.manifold_Point;
--                                                                  numUnused : in     Integer;
--                                                                  pt        : in     impact.d3.manifold_Point.impact.d3.manifold_Point) return Integer;


--     package Vectors is new ada.containers.Vectors (Positive, Item);
--     type    Vector  is new Vectors.Vector with null record;


--     function Length (Self : in Vector) return ada.Containers.Count_Type
--       renames Vectors.Length;


end impact.d3.Manifold;
