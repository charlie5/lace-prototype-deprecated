with impact.d3.union_Find;
with impact.d3.Manifold;

--  #include "btCollisionCreateFunc.h"
--  #include "LinearMath/btAlignedObjectArray.h"
with impact.d3.Object;

--
--  class impact.d3.Object;
--  class impact.d3.Space;
--  class impact.d3.Dispatcher;
--  class impact.d3.Manifold;


with impact.d3.Space;
with impact.d3.Dispatcher;




package impact.d3.simulation_island_Manager
--
--  SimulationIslandManager creates and handles simulation islands, using impact.d3.union_Find.
--
is


   type Item is tagged private;
   type View is access all Item'Class;




   --- Containers
   --

--     type    impact.d3.Manifold_view is access all impact.d3.Manifold.Item'Class;
--
--     package impact.d3.Manifold_Vectors is new ada.Containers.Vectors (Positive, impact.d3.Manifold_view);
--     subtype impact.d3.Manifold_Vector  is     impact.d3.Manifold_Vectors.Vector;



--     type    impact.d3.Object_view is access all impact.d3.Object.impact.d3.Object'Class;
--
--     package impact.d3.Object_Vectors is new ada.Containers.Vectors (Positive, impact.d3.Object_view);
--     subtype impact.d3.Object_Vector  is     impact.d3.Object_Vectors.Vector;




   --- IslandCallback
   --

   type IslandCallback is abstract tagged null record;

   procedure destruct      (Self : in out IslandCallback)                                               is null;
   procedure ProcessIsland (Self : access IslandCallback;   bodies    : access impact.d3.Object.Vector;
                                                            manifolds : access impact.d3.Manifold.Vector;
--                                                              manifolds : in impact.d3.Manifold.views;
                                                            islandId  : in Integer                  ) is abstract;



   --- Forge
   --

   procedure destruct (Self : in out Item);





   --- Attributes
   --

   procedure initUnionFind (Self : in out Item;   n : in Integer);



   function  getUnionFind (Self : access Item) return impact.d3.union_Find.view;



   procedure findUnions (Self : in out Item;   dispatcher : access impact.d3.Dispatcher.item'Class;
                                               colWorld   : access impact.d3.Space.Item    'Class);



   procedure updateActivationState (Self : in out Item;   colWorld   : access impact.d3.Space.Item    'Class;
                                                          dispatcher : access impact.d3.Dispatcher.item'Class);



   procedure storeIslandActivationState (Self : in out Item;   World   : access impact.d3.Space.Item'Class);




   function  getSplitIslands (Self : in     Item)                return Boolean;
   procedure setSplitIslands (Self : in out Item;   doSplitIslands : in Boolean);







   --- Operations
   --

   procedure buildAndProcessIslands (Self : access Item;   dispatcher : access impact.d3.Dispatcher.item'Class;
                                                           colWorld   : access impact.d3.Space.Item    'Class;
                                                           callback   : access IslandCallback           'Class);

   procedure buildIslands           (Self : access Item;   dispatcher : access impact.d3.Dispatcher.item'Class;
                                                           colWorld   : access impact.d3.Space.Item    'Class);






private

   type Item is tagged
      record
         m_unionFind      : aliased impact.d3.union_Find.item;

         m_islandmanifold : impact.d3.Manifold.Vector;
         m_islandBodies   : aliased impact.d3.Object.Vector;

         m_splitIslands   : Boolean := True;
      end record;





end impact.d3.simulation_island_Manager;
