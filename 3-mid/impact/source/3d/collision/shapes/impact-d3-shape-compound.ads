with impact.d3.Shape;
--       btBvt,
with impact.d3.collision.bounding_volume_Tree;
with ada.Containers.Vectors;
with impact.d3.collision.Proxy;



--  #include "impact.d3.Shape.h"
--
--  #include "LinearMath/impact.d3.Vector.h"
--  #include "LinearMath/impact.d3.Transform.h"
--  #include "LinearMath/impact.d3.Matrix.h"
--  #include "impact.d3.collision.Margin.h"
--  #include "LinearMath/btAlignedObjectArray.h"



--  //class impact.d3.collision.quantized_Bvh.optimized;
--  struct impact.d3.collision.bounding_volume_Tree;



package impact.d3.Shape.compound
--
--  The impact.d3.Shape.compound allows to store multiple other impact.d3.Shapes
--  This allows for moving concave collision objects. This is more general then the static concave impact.d3.Shape.concave.triangle_mesh.bvh.
--  It has an (optional) dynamic aabb tree to accelerate early rejection tests.
--  @todo: This aabb tree can also be use to speed up ray tests on impact.d3.Shape.compound, see http://code.google.com/p/bullet/issues/detail?id=25
--  Currently, removal of child shapes is only supported when disabling the aabb tree (pass 'false' in the constructor of impact.d3.Shape.compound)
--
is


   type Item is new impact.d3.Shape.item with private;
   type View is access all Item'Class;



   --- Forge
   --

   function to_compound_Shape (enableDynamicAabbTree : in Boolean := True) return Item;

   overriding procedure destruct (Self : in out Item);






   --- impact.d3.Shape.compoundChild
   --

   type Child is
      record
         m_transform      : access  Transform_3d           := new Transform_3d;
         m_childShape     : access  impact.d3.Shape.Item'Class;
         m_childShapeType :         impact.d3.collision.Proxy.BroadphaseNativeTypes;
         m_childMargin    :         math.Real;
         m_node           : access  impact.d3.collision.bounding_volume_Tree.Node'Class;
      end record;



   overriding function "=" (L, R : in impact.d3.Shape.compound.Child) return Boolean;



   package Child_Vectors is new ada.Containers.Vectors (Positive, impact.d3.Shape.compound.Child);
   subtype Child_Vector  is     impact.d3.Shape.compound.Child_Vectors.Vector;






   --- Attributes
   --

   overriding
   procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                                        aabbMin, aabbMax :    out math.Vector_3);
   --
   --  getAabb's default implementation is brute force, expected derived classes to implement a fast dedicated version





   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3) ;
   overriding function  getLocalScaling (Self : in     Item)         return math.Vector_3  ;




   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3);

   overriding function  getName (Self : in Item) return String;



   overriding procedure setMargin (Self : in out Item;   margin : in math.Real);
   overriding function  getMargin (Self : in     Item)        return math.Real;






   function  getChildShape     (Self : in Item;   index : in Integer) return access impact.d3.Shape.Item'Class;
   function  getChildTransform (Self : in Item;   index : in Integer) return access Transform_3d;



   function  getDynamicAabbTree (Self : in Item) return access impact.d3.collision.bounding_volume_Tree.Item'Class;






   function  getNumChildShapes (Self : in Item) return Natural;



   procedure addChildShape    (Self : in out Item;   localTransform : in Transform_3d;
                                                     shape          : access impact.d3.Shape.Item'Class);

   procedure removeChildShape (Self : in out Item;   shape          : access impact.d3.Shape.Item'Class);
   --
   --  Remove all children shapes that contain the specified shape.



   procedure removeChildShapeByIndex (Self : in out Item;   childShapeindex : in Integer);



   procedure updateChildTransform (Self : in out Item;   childIndex        : in Integer;
                                                         newChildTransform : in Transform_3d;
                                                         shouldRecalculateLocalAabb : in Boolean := True);
   --
   --  Set a new transform for a child, and update internal data structures (local aabb and dynamic tree).



   function getChildList (Self : access Item) return access impact.d3.Shape.compound.Child_Vector;



   procedure recalculateLocalAabb (Self : in out Item);
   --
   --  Re-calculate the local Aabb. Is called at the end of removeChildShapes.
   --  Use this yourself if you modify the children or their transforms.


   procedure createAabbTreeFromChildren (Self : in out Item);


   procedure calculatePrincipalAxisTransform (Self : in Item;   masses    : in     math.Vector;        -- impact.d3.Scalar*   masses
                                                                principal : access Transform_3d;
                                              inertia   : in out math.Vector_3);
   --
   --  Computes the exact moment of inertia and the transform from the coordinate system defined by the principal axes of the moment of inertia
   --  and the center of mass to the current coordinate system. "masses" points to an array of masses of the children. The resulting transform
   --  "principal" has to be applied inversely to all children transforms in order for the local coordinate system of the compound
   --  shape to be centered at the center of mass and to coincide with the principal axes. This also necessitates a correction of the world transform
   --  of the collision object by the principal transform.



   function getUpdateRevision (Self : in Item) return Integer;





private


   type Item is new impact.d3.Shape.item with
      record
         m_children        : aliased impact.d3.Shape.compound.Child_Vector;   -- btAlignedObjectArray<impact.d3.Shape.compoundChild> ;

         m_localAabbMin,
         m_localAabbMax    : math.Vector_3;

         m_dynamicAabbTree : impact.d3.collision.bounding_volume_Tree.view;
         m_updateRevision  : Integer;        -- increment m_updateRevision when adding/removing/replacing child shapes, so that some caches can be updated

         m_collisionMargin : math.Real;
         m_localScaling    : math.Vector_3;
      end record;


end impact.d3.Shape.compound;
