with ada.unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with impact.d3.Matrix;
with impact.d3.Vector,
     impact.d3.Transform;
with impact.d3.collision.bounding_volume_Tree;
--  with bttransform;



--  #include "impact.d3.Shape.compound.h"
--  #include "impact.d3.Shape.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.bounding_volume_Tree.h"
--  #include "LinearMath/btSerializer.h"



package body impact.d3.Shape.compound
is




   type Any_view is access all Any'Class;
   function to_any_Class is new ada.unchecked_Conversion (Integer, Any_view);







   function to_compound_Shape (enableDynamicAabbTree : in Boolean := True) return Item
   is
      Self : Item := (impact.d3.Shape.item with
                      m_localAabbMin    => (BT_LARGE_FLOAT,  BT_LARGE_FLOAT,  BT_LARGE_FLOAT),
                      m_localAabbMax    => (-BT_LARGE_FLOAT, -BT_LARGE_FLOAT, -BT_LARGE_FLOAT),
                      m_dynamicAabbTree => null,
                      m_children        => <>,
                      m_updateRevision  => 1,
                      m_collisionMargin => 0.0,
                      m_localScaling    => (1.0, 1.0, 1.0));
   begin
      Self.setShapeType (impact.d3.collision.Proxy.COMPOUND_SHAPE_PROXYTYPE);

      if enableDynamicAabbTree then
         Self.m_dynamicAabbTree := new impact.d3.collision.bounding_volume_Tree.item;
      end if;

      return Self;
   end to_compound_Shape;









   overriding procedure destruct (Self : in out Item)
   is
      use impact.d3.collision.bounding_volume_Tree;
      procedure free is new ada.unchecked_Deallocation (impact.d3.collision.bounding_volume_Tree.item'Class, impact.d3.collision.bounding_volume_Tree.view);
   begin
      if Self.m_dynamicAabbTree /= null then
         Self.m_dynamicAabbTree.destruct;
         free (Self.m_dynamicAabbTree);
      end if;
   end destruct;








   overriding function "=" (L, R : in impact.d3.Shape.compound.Child) return Boolean
   is
      use type
          impact.d3.collision.Proxy.BroadphaseNativeTypes;
   begin
      return     L.m_transform      = R.m_transform
        and then L.m_childShape     = R.m_childShape
        and then L.m_childShapeType = R.m_childShapeType
        and then L.m_childMargin    = R.m_childMargin;
   end;






   procedure addChildShape (Self : in out Item;   localTransform : in Transform_3d;
                            shape          : access impact.d3.Shape.Item'Class)
   is
      use type impact.d3.collision.bounding_volume_Tree.view;

      child        : compound.Child;

      localAabbMin,
      localAabbMax : math.Vector_3;

      index        : Integer;

   begin
      Self.m_updateRevision := Self.m_updateRevision + 1;

      --  m_childTransforms.push_back(localTransform);
      --  m_childShapes.push_back(shape);

      child.m_node           := null;
      child.m_transform.all  := localTransform;
      child.m_childShape     := shape;
      child.m_childShapeType := shape.getShapeType;
      child.m_childMargin    := shape.getMargin;


      --  extend the local aabbMin/aabbMax
      shape.getAabb (localTransform, localAabbMin, localAabbMax);

      for i in 1 .. 3
      loop
         if Self.m_localAabbMin (i) > localAabbMin (i) then
            Self.m_localAabbMin (i) := localAabbMin (i);
         end if;

         if Self.m_localAabbMax (i) < localAabbMax (i) then
            Self.m_localAabbMax (i) := localAabbMax (i);
         end if;
      end loop;


      if Self.m_dynamicAabbTree /= null then
         declare
            bounds : impact.d3.collision.bounding_volume_Tree.Volume renames impact.d3.collision.bounding_volume_Tree.FromMM (localAabbMin, localAabbMax);
         begin
            index        := Integer (Self.m_children.Length);
            child.m_node := Self.m_dynamicAabbTree.insert (bounds, to_any_Class (index));
         end;
      end if;


      Self.m_children.append (child);
   end addChildShape;





   procedure removeChildShape (Self : in out Item;   shape          : access impact.d3.Shape.Item'Class)
   is
   begin
      Self.m_updateRevision := Self.m_updateRevision + 1;

      --  Find the children containing the shape specified, and remove those children.
      --  note: there might be multiple children using the same shape!
      --
      for i in reverse 1 .. Integer (Self.m_children.Length)
      loop
         if Self.m_children.Element (i).m_childShape = shape then
            Self.removeChildShapeByIndex (i);
         end if;
      end loop;


      Self.recalculateLocalAabb;
   end removeChildShape;







   procedure removeChildShapeByIndex (Self : in out Item;   childShapeindex : in Integer)
   is
      use impact.d3.collision.bounding_volume_Tree;
   begin
      Self.m_updateRevision := Self.m_updateRevision + 1;
      pragma Assert (childShapeIndex >= 1 and then childShapeIndex <= Integer (Self.m_children.Length));

      if Self.m_dynamicAabbTree /= null then
         Self.m_dynamicAabbTree.remove (Self.m_children.Element (childShapeIndex).m_node);
      end if;

      Self.m_children.swap (childShapeIndex,  Integer (Self.m_children.Length) - 1);

      if Self.m_dynamicAabbTree /= null then
         Self.m_children.Element (childShapeIndex).m_node.state.dataAsInt := childShapeIndex;
      end if;

      Self.m_children.delete_Last;
   end removeChildShapeByIndex;





   procedure updateChildTransform (Self : in out Item;   childIndex        : in Integer;
                                                         newChildTransform : in Transform_3d;
                                                         shouldRecalculateLocalAabb : in Boolean := True)
   is
      use type impact.d3.collision.bounding_volume_Tree.view;
   begin
      Self.m_children.Element (childIndex).m_transform.all := newChildTransform;

      if Self.m_dynamicAabbTree /= null then
         declare                                  -- update the dynamic aabb tree
            use impact.d3.collision.bounding_volume_Tree;

            localAabbMin,
            localAabbMax : math.Vector_3;

            bounds       : impact.d3.collision.bounding_volume_Tree.Volume;
         begin
               Self.m_children.Element (childIndex).m_childShape.getAabb (newChildTransform,  localAabbMin, localAabbMax);
               bounds := FromMM (localAabbMin, localAabbMax);
               --  int index = m_children.size()-1;
               Self.m_dynamicAabbTree.update (Self.m_children.Element (childIndex).m_node,  bounds);
         end;
      end if;

      if shouldRecalculateLocalAabb then
         Self.recalculateLocalAabb;
      end if;
   end updateChildTransform;








   procedure recalculateLocalAabb (Self : in out Item)
   is
      localAabbMin,
      localAabbMax : math.Vector_3;

   begin
      --  Recalculate the local aabb
      --  Brute force, it iterates over all the shapes left.

      Self.m_localAabbMin := (BT_LARGE_FLOAT,  BT_LARGE_FLOAT,  BT_LARGE_FLOAT);
      Self.m_localAabbMax := (-BT_LARGE_FLOAT, -BT_LARGE_FLOAT, -BT_LARGE_FLOAT);

      --  extend the local aabbMin/aabbMax
      for j in 1 .. Integer (Self.m_children.Length)
      loop
         Self.m_children.Element (j).m_childShape.getAabb (Self.m_children.Element (j).m_transform.all,  localAabbMin, localAabbMax);
         for i in 1 .. 3
         loop
            if Self.m_localAabbMin (i) > localAabbMin (i) then
               Self.m_localAabbMin (i) := localAabbMin (i);
            end if;
            if Self.m_localAabbMax (i) < localAabbMax (i) then
               Self.m_localAabbMax (i) := localAabbMax (i);
            end if;
         end loop;
      end loop;
   end recalculateLocalAabb;








   procedure createAabbTreeFromChildren (Self : in out Item)
   is
      use type impact.d3.collision.bounding_volume_Tree.view;
   begin
      if Self.m_dynamicAabbTree = null then

         Self.m_dynamicAabbTree := new impact.d3.collision.bounding_volume_Tree.item;


         for index in 1 .. Integer (Self.m_children.Length)
         loop
            declare
               child        : compound.Child := Self.m_children.Element (index);

               localAabbMin,
               localAabbMax : math.Vector_3;

               bounds       : impact.d3.collision.bounding_volume_Tree.Volume;
            begin
               --  extend the local aabbMin/aabbMax
               child.m_childShape.getAabb (child.m_transform.all,  localAabbMin, localAabbMax);

               bounds       := impact.d3.collision.bounding_volume_Tree.FromMM (localAabbMin, localAabbMax);
               child.m_node := Self.m_dynamicAabbTree.insert (bounds,  to_Any_Class (index));
               --  Self.m_children.replace_Element (index, child);  -- tbd: check if this is needed.
            end;
         end loop;
      end if;
   end createAabbTreeFromChildren;







   procedure calculatePrincipalAxisTransform (Self : in Item;   masses    : in     math.Vector;        -- impact.d3.Scalar*   masses
                                                                principal : access Transform_3d;
                                              inertia   : in out math.Vector_3)
   is
      use math.Vectors,  impact.d3.Matrix,  impact.d3.Transform;

      n         : constant Integer       := Integer (Self.m_children.Length);

      totalMass : math.Real     := 0.0;
      center    : math.Vector_3 := (0.0, 0.0, 0.0);

      tensor    : aliased math.Matrix_3x3 := ((0.0, 0.0, 0.0),
                                              (0.0, 0.0, 0.0),
                                              (0.0, 0.0, 0.0));
   begin
      for k in 1 .. n
      loop
         pragma Assert (masses (k) > 0.0);

         center    := center    + getOrigin (Self.m_children.Element (k).m_transform.all) * masses (k);
         totalMass := totalMass + masses (k);
      end loop;

      pragma Assert (totalMass > 0.0);

      center := center / totalMass;
      setOrigin (principal.all,  center);

      for k in 1 .. n
      loop
         declare
            use impact.d3.Vector;

            i  : math.Vector_3;

            t  : access Transform_3d;
            o  : math.Vector_3;
            o2 : math.Real;

            j  : aliased math.Matrix_3x3;

         begin
            Self.m_children.Element (k).m_childShape.calculateLocalInertia (masses (k),  i);

            t := Self.m_children.Element (k).m_transform;
            o := getOrigin (t).all - center;

            --  compute inertia tensor in coordinate system of compound shape
            --
            j                     := Transpose (getBasis (t).all);
            Row (j'Access, 1).all := getRow (j, 1)  *  i (1);
            Row (j'Access, 2).all := getRow (j, 2)  *  i (2);
            Row (j'Access, 3).all := getRow (j, 3)  *  i (3);
            j                     := getBasis (t).all * j;

            --  add inertia tensor
            --
            Row (tensor'Access, 1).all := getRow (tensor, 1)  +  getRow (j, 1);
            Row (tensor'Access, 2).all := getRow (tensor, 2)  +  getRow (j, 2);
            Row (tensor'Access, 3).all := getRow (tensor, 3)  +  getRow (j, 3);

            --  compute inertia tensor of pointmass at o
            --
            o2 := length2 (o);

            Row (j'Access, 1).all := (o2,  0.0,  0.0);
            Row (j'Access, 2).all := (0.0,   o2,  0.0);
            Row (j'Access, 3).all := (0.0,  0.0,   o2);
--              j[1].setValue(0, o2, 0);
--              j[2].setValue(0, 0, o2);

            Row (j'Access, 1).all := getRow (j, 1)  +  o * (-o (1));
            Row (j'Access, 2).all := getRow (j, 2)  +  o * (-o (2));
            Row (j'Access, 3).all := getRow (j, 3)  +  o * (-o (3));
--              j[0] += o * -o.x();
--              j[1] += o * -o.y();
--              j[2] += o * -o.z();

            --  add inertia tensor of pointmass
            --
            Row (tensor'Access, 1).all := getRow (tensor, 1)  +  masses (k) * getRow (j, 1);
            Row (tensor'Access, 2).all := getRow (tensor, 2)  +  masses (k) * getRow (j, 2);
            Row (tensor'Access, 3).all := getRow (tensor, 3)  +  masses (k) * getRow (j, 3);
         end;
      end loop;


      diagonalize (tensor,  getBasis (principal),  0.00001,  20);

      inertia := (tensor (1, 1),  tensor (2, 2),  tensor (3, 3));
   end calculatePrincipalAxisTransform;









   overriding procedure getAabb (Self : in Item;   t                : in     Transform_3d;
                      aabbMin, aabbMax :    out math.Vector_3)
   is
      use impact.d3.Vector, impact.d3.Matrix, impact.d3.Transform,
          linear_Algebra_3d,  Math.Vectors;

      localHalfExtents : math.Vector_3 := 0.5 * (Self.m_localAabbMax - Self.m_localAabbMin);
      localCenter      : math.Vector_3 := 0.5 * (Self.m_localAabbMax + Self.m_localAabbMin);

      abs_b  : aliased math.Matrix_3x3;
      center,
      extent : math.Vector_3;

   begin
      if Self.m_children.is_Empty then   -- avoid an illegal AABB when there are no children
         localHalfExtents := (0.0, 0.0, 0.0);
         localCenter      := (0.0, 0.0, 0.0);
      end if;

      localHalfExtents := localHalfExtents + (Self.getMargin, Self.getMargin, Self.getMargin);


      abs_b   := absolute (getBasis (t));
      center  := t * localCenter;

      extent  := (dot (Row (abs_b'Access, 1).all,  localHalfExtents),
                  dot (Row (abs_b'Access, 2).all,  localHalfExtents),
                  dot (Row (abs_b'Access, 3).all,  localHalfExtents));

      aabbMin := center - extent;
      aabbMax := center + extent;
   end getAabb;









   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3)
   is
      use impact.d3.Vector, impact.d3.Transform, Math.Vectors;

      childTrans : Transform_3d;
      childScale : math.Vector_3;

      childScale_Factor : math.Vector_3;
   begin
      for i in 1 .. Integer (Self.m_children.Length)
      loop
         childTrans := Self.getChildTransform (i).all;
         childScale := Self.m_children.Element (i).m_childShape.getLocalScaling;
         --              childScale = childScale * (childTrans.getBasis() * scaling);  -- commented out in bullet code also

         childScale_Factor := (scaling (1) / Self.m_localScaling (1),
                               scaling (2) / Self.m_localScaling (2),
                               scaling (3) / Self.m_localScaling (3));

         childScale := Scaled (childScale, childScale_Factor); --- (scaling / Self.m_localScaling);   -- tbd: check order of evaluation is correct.

         Self.m_children.Element (i).m_childShape.setLocalScaling (childScale);
         setOrigin (childTrans,  Scaled (getOrigin (childTrans), scaling));
         Self.updateChildTransform (i, childTrans, False);
      end loop;


      Self.m_localScaling := scaling;
      Self.recalculateLocalAabb;
   end setLocalScaling;





   overriding function  getLocalScaling (Self : in     Item)         return math.Vector_3
   is
   begin
      return Self.m_localScaling;
   end getLocalScaling;







   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3)
   is
      use impact.d3.Transform, math.Vectors;

      ident       : Transform_3d;

      aabbMin,
      aabbMax     : math.Vector_3;

      halfExtents : math.Vector_3;
      lx, ly, lz  : math.Real;

   begin
      setIdentity (ident);

      --  approximation: take the inertia from the aabb for now

      Self.getAabb (ident,  aabbMin, aabbMax);

      halfExtents := (aabbMax - aabbMin) * 0.5;

      lx := 2.0 * (halfExtents (1));
      ly := 2.0 * (halfExtents (2));
      lz := 2.0 * (halfExtents (3));

      inertia (1) := mass / 12.0 * (ly * ly + lz*lz);
      inertia (2) := mass / 12.0 * (lx * lx + lz*lz);
      inertia (3) := mass / 12.0 * (lx * lx + ly*ly);
   end calculateLocalInertia;







   overriding function  getName (Self : in Item) return String
   is
      pragma Unreferenced (Self);
   begin
      return "Compound";
   end getName;




   overriding procedure setMargin (Self : in out Item;   margin : in math.Real)
   is
   begin
      Self.m_collisionMargin := margin;
   end setMargin;




   overriding function  getMargin (Self : in     Item)        return math.Real
   is
   begin
      return Self.m_collisionMargin;
   end getMargin;






   function  getChildShape (Self : in Item;   index : in Integer) return access impact.d3.Shape.Item'Class
   is
   begin
      return Self.m_children.Element (index).m_childShape;
   end getChildShape;





   function  getChildTransform (Self : in Item;   index : in Integer) return access Transform_3d
   is
   begin
      return Self.m_children.Element (index).m_transform;
   end getChildTransform;






   function  getDynamicAabbTree (Self : in Item) return access impact.d3.collision.bounding_volume_Tree.Item'Class
   is
   begin
      return Self.m_dynamicAabbTree;
   end getDynamicAabbTree;



   function getChildList (Self : access Item) return access impact.d3.Shape.compound.Child_Vector
   is
   begin
      return Self.m_children'Access;
   end getChildList;






   function  getNumChildShapes (Self : in Item) return Natural
   is
   begin
      return Natural (Self.m_children.Length);
   end getNumChildShapes;




   function getUpdateRevision (Self : in Item) return Integer
   is
   begin
      return Self.m_updateRevision;
   end getUpdateRevision;



end impact.d3.Shape.compound;
