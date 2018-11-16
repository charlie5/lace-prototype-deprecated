with impact.d3.collision.Proxy,
     impact.d3.Dispatcher;



package impact.d3.collision.overlapped_pair_Callback
--
--  The impact.d3.collision.overlapped_pair_Callback class is an additional optional broadphase user callback for adding/removing overlapping pairs, similar
--  interface to impact.d3.collision.overlapped_pair_Callback.cached.
--
is

   type Item is abstract tagged null record;



   --- Forge
   --

   procedure destruct (Self : in out Item) is null;





   --- Attributes
   --

   function  addOverlappingPair    (Self : access Item;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class) return access impact.d3.collision.Proxy.btBroadphasePair
                                    is abstract;

   function  removeOverlappingPair (Self : access Item;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class;
                                                          dispatcher     : access impact.d3.Dispatcher.item'Class) return access Any'Class
                                    is abstract;

   procedure removeOverlappingPairsContainingProxy (Self : access Item;   proxy0     : access impact.d3.collision.Proxy.item'Class;
                                                                          dispatcher : access impact.d3.Dispatcher.item'Class)
                                    is abstract;


end impact.d3.collision.overlapped_pair_Callback;




   --  class impact.d3.Dispatcher;

   --  struct  btBroadphasePair;



--  class impact.d3.collision.overlapped_pair_Callback
--  {
--  public:
--          virtual void*        removeOverlappingPair(impact.d3.collision.Proxy* proxy0,impact.d3.collision.Proxy* proxy1,impact.d3.Dispatcher* dispatcher) = 0;
--
--          virtual void        removeOverlappingPairsContainingProxy(impact.d3.collision.Proxy* proxy0,impact.d3.Dispatcher* dispatcher) = 0;
--
--  };

