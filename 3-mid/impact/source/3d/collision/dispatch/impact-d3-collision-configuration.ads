with impact.d3.collision.Proxy,
     impact.d3.collision.create_Func;

--  with btCollisionCreateFunc;   use btCollisionCreateFunc;
--  with impact.d3.collision.Proxy;


package impact.d3.Collision.Configuration
--
--  Allows to configure stack allocator size, default collision algorithms and persistent manifold pool size
--  for collision detection.
--
is

   type Item is abstract tagged null record;


   procedure destruct (Self : in out Item) is null;


   function getCollisionAlgorithmCreateFunc (Self : in Item;   proxyType0,
                                                               proxyType1 : impact.d3.collision.Proxy.BroadphaseNativeTypes) return access impact.d3.collision.create_Func.item'Class
                                             is abstract;


end impact.d3.Collision.Configuration;




--  struct impact.d3.collision.AlgorithmCreateFunc;


--  class        impact.d3.collision.Configuration
--  {
--
--  public:
--
--
--
--          virtual impact.d3.collision.AlgorithmCreateFunc* getCollisionAlgorithmCreateFunc(int proxyType0,int proxyType1) =0;
--
--  };

