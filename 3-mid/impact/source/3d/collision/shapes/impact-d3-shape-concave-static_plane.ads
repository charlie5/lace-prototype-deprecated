with impact.d3.Shape.concave,
     impact.d3.triangle_Callback;



package impact.d3.Shape.concave.static_plane
--
--  Simulates an infinite non-moving (static) collision plane.
--
is




   type Item is new impact.d3.Shape.concave.Item with private;



   --- Forge
   --

   function to_static_plane_Shape (planeNormal   : in math.Vector_3;
                                   planeConstant : in math.Real  ) return Item;

   overriding procedure destruct (Self : in out Item);




   --- Attributes
   --

--     function localGetSupportingVertex                     (Self : in Item;   vec : in Vector_3) return Vector_3;
--     function localGetSupportingVertexWithoutMargin        (Self : in Item;   vec : in Vector_3) return Vector_3;




--     procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
--                                                                                    supportVerticesOut :    out Vector_3_array;
--                                                                                    numVectors         : in     Integer );


   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                                        aabbMin, aabbMax :    out Vector_3)     ;
   --
   --  'getAabb's default implementation is brute force, expected derived classes to implement a fast dedicated version




--     procedure setMargin (Self : in out Item;   margin : in Scalar)                 ;
--     function  getMargin (Self : in     Item)        return Scalar                  ;





   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3);

   overriding function  getName   (Self : in     Item)        return String     ;



--     function  getRadius   (Self : in     Item)        return math.Real;




   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3);
   overriding function  getLocalScaling (Self : in     Item)         return math.Vector_3;

   function  getPlaneNormal   (Self : in     Item)         return math.Vector_3;
   function  getPlaneConstant (Self : in     Item)         return math.Real;


   overriding procedure processAllTriangles (Self : in     Item;   callback         : access impact.d3.triangle_Callback.Item'Class;
                                                        aabbMin, aabbMax : in     math.Vector_3);




private

   type Item is new impact.d3.Shape.concave.Item with
      record
         m_localAabbMin,
         m_localAabbMax  : math.Vector_3;

         m_planeNormal   : math.Vector_3;
         m_planeConstant : math.Real;

         m_localScaling  : math.Vector_3;
      end record;

end impact.d3.Shape.concave.static_plane;






--  ATTRIBUTE_ALIGNED16(class) impact.d3.Shape.concave.static_plane : public impact.d3.Shape.concave
--  {
--  public:
--
--
--          const impact.d3.Vector&        getPlaneNormal() const
--          {
--                  return        m_planeNormal;
--          }
--
--          const impact.d3.Scalar&        getPlaneConstant() const
--          {
--                  return        m_planeConstant;
--          }
--
--          //debugging
--          virtual const char*        getName()const {return "STATICPLANE";}
--
--  };

