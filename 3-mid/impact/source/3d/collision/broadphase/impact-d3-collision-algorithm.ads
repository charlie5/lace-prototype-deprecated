with impact.d3.Manifold,
     impact.d3.Dispatcher,
     impact.d3.Object,
     impact.d3.collision.manifold_Result,
     ada.containers.Vectors;


package impact.d3.Collision.Algorithm
--
--  impact.d3.collision.Algorithm is an collision interface that is compatible with the Broadphase and impact.d3.Dispatcher.
--  It is persistent over frames.
--
is


--     type btManifoldArray is array (Natural range <>) of access impact.d3.Manifold.Item;


   type Manifold_view is access all impact.d3.Manifold.Item'Class;

   package btManifoldArrays is new ada.containers.Vectors (Positive, Manifold_view);
   subtype btManifoldArray  is btManifoldArrays.Vector;



   type AlgorithmConstructionInfo is
      record
        m_dispatcher1 : access impact.d3.Dispatcher.item'Class;
        m_manifold    : access impact.d3.Manifold.Item;

         --     int    getDispatcherId();
      end record;


   function to_AlgorithmConstructionInfo (dispatcher : access impact.d3.Dispatcher.item'Class;   temp : in Integer) return AlgorithmConstructionInfo;







   type Item is abstract tagged private;


--     function  to_impact.d3.collision.Algorithm (ci : in impact.d3.collision.AlgorithmConstructionInfo) return Item'Class;
   procedure define   (Self : in out Item;   ci : in AlgorithmConstructionInfo);
   procedure destruct (Self : in out Item);


   procedure processCollision (Self : in out Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                     resultOut    :    out impact.d3.collision.manifold_Result.item)   is abstract;

   function  calculateTimeOfImpact (Self : in Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                      dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                      resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real  is abstract;

   procedure getAllContactManifolds (Self : in out Item;   manifoldArray :    out btManifoldArray)   is abstract;


   procedure set_m_dispatcher (Self : in out Item;   To : access impact.d3.Dispatcher.item'Class);
   function  get_m_dispatcher (Self : in     Item) return access impact.d3.Dispatcher.item'Class;





private

   type Item is abstract tagged
     record
         m_dispatcher : access impact.d3.Dispatcher.item'Class;
         --  int    getDispatcherId();
     end record;


end impact.d3.Collision.Algorithm;
