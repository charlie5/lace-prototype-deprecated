with impact.d3.Shape.convex,
     impact.d3.collision.Margin;




package impact.d3.Shape.convex.internal
--
--  The impact.d3.Shape.convex.internal is an internal base class, shared by most convex shape implementations.
--
--  The impact.d3.Shape.convex.internal uses a default collision margin set to CONVEX_DISTANCE_MARGIN.
--
--  This collision margin used by Gjk and some other algorithms, see also impact.d3.collision.Margin.h
--
--  Note that when creating small shapes (derived from impact.d3.Shape.convex.internal),
--  you need to make sure to set a smaller collision margin, using the 'setMargin' API
--
--  There is a automatic mechanism 'setSafeMargin' used by impact.d3.Shape.convex.internal.polyhedral.box and impact.d3.Shape.convex.internal.cylinder
--
is



   type Item is abstract new impact.d3.Shape.convex.item with private;



   overriding function  localGetSupportingVertex (Self : in     Item;   vec : in math.Vector_3) return math.Vector_3;


   function  getImplicitShapeDimensions (Self : in     Item) return math.Vector_3;

   procedure setImplicitShapeDimensions (Self : in out Item;   dimensions : in math.Vector_3);
   --
   --
   --          warning: use setImplicitShapeDimensions with care
   --
   --          changing a collision shape while the body is in the world is not recommended,
   --          it is best to remove the body from the world, then make the change, and re-add it
   --          alternatively flush the contact points, see documentation for 'cleanProxyFromPairs'




   procedure setSafeMargin (Self : in out Item'Class;   minDimension            : in math.Real;
                                                        defaultMarginMultiplier : in math.Real := 0.1);


   procedure setSafeMargin (Self : in out Item'Class;   halfExtents             : in math.Vector_3;
                                                        defaultMarginMultiplier : in math.Real := 0.1);


   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                      aabbMin, aabbMax :    out math.Vector_3);
   --
   --  getAabb's default implementation is brute force, expected derived classes to implement a fast dedicated version




   overriding procedure getAabbSlow (Self : in Item;     t                : in     Transform_3d;
                          aabbMin, aabbMax :    out math.Vector_3);


   overriding procedure setLocalScaling (Self : in out Item;     scaling : in math.Vector_3);



   overriding function  getLocalScaling   (Self : in     Item) return math.Vector_3;

   function  getLocalScalingNV (Self : in     Item) return math.Vector_3;




   overriding procedure setMargin (Self : in out Item;     margin : in math.Real);
   overriding function  getMargin (Self : in     Item)          return math.Real;

   function  getMarginNV (Self : in     Item)          return math.Real;


   overriding function  getNumPreferredPenetrationDirections (Self : in     Item)          return Integer;


   overriding procedure getPreferredPenetrationDirection (Self : in Item;     Index             : in      Integer;
                                                                   penetrationVector :     out math.Vector_3);









   ---  btConvexInternalAabbCachingShape
   --
   --  adds local aabb caching for convex shapes, to avoid expensive bounding box calculations
   --

   type btConvexInternalAabbCachingShape is abstract new impact.d3.Shape.convex.internal.Item with private;



   overriding procedure setLocalScaling (Self : in out btConvexInternalAabbCachingShape;   scaling : in math.Vector_3);


   overriding procedure getAabb         (Self : in     btConvexInternalAabbCachingShape;   t                : in     Transform_3d;
                                                                                aabbMin, aabbMax :    out math.Vector_3);



   procedure recalcLocalAabb (Self : in out btConvexInternalAabbCachingShape'Class);




   procedure set_m_localScaling (Self :     out Item;   To : in math.Vector_3);




   --- 'protected'

   procedure getCachedLocalAabb (Self : in     btConvexInternalAabbCachingShape;   aabbMin, aabbMax : out math.Vector_3);





private

   type Item is abstract new impact.d3.Shape.convex.item with
      record
         m_localScaling            : math.Vector_3 := (1.0, 1.0, 1.0);           -- local scaling. collisionMargin is not scaled !
         m_implicitShapeDimensions : math.Vector_3;

         m_collisionMargin : math.Real := impact.d3.collision.Margin.CONVEX_DISTANCE_MARGIN;
         m_padding         : math.Real;
      end record;







   type btConvexInternalAabbCachingShape is abstract new impact.d3.Shape.convex.internal.Item with
      record
         m_localAabbMin     : math.Vector_3 := (1.0,  1.0,  1.0);
         m_localAabbMax     : math.Vector_3 := (-1.0, -1.0, -1.0);

         m_isLocalAabbValid : Boolean       := False;
      end record;




   procedure setCachedLocalAabb (Self : in out btConvexInternalAabbCachingShape;   aabbMin, aabbMax : in math.Vector_3);




   procedure getNonvirtualAabb (Self : in     btConvexInternalAabbCachingShape'Class;   trans            : in     Transform_3d;
                                                                                        aabbMin, aabbMax :    out math.Vector_3;
                                                                                        margin           : in     math.Real);


end impact.d3.Shape.convex.internal;
