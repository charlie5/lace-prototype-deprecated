with impact.d3.collision.Proxy;
with impact.d3.Transform;
with impact.d3.Vector;
with impact.d3.Scalar;

package body impact.d3.Shape.convex.internal.cone
--
--
--
is

   ----------
   --- Forge
   --

   function to_cone_Shape  (radius, height : in math.Real) return Item      -- Returns a Cone shape, around the Y axis.
   is
      use math.Functions;

      Self : Item;
   begin
      Self.m_radius := radius;
      Self.m_height := height;

      Self.setShapeType (impact.d3.collision.Proxy.CONE_SHAPE_PROXYTYPE);
      Self.setConeUpIndex (2);

      Self.m_sinAngle := (Self.m_radius / sqRt (Self.m_radius * Self.m_radius  +  Self.m_height * Self.m_height));


      return Self;
   end to_cone_Shape;




   function to_coneX_Shape (radius, height : in math.Real) return Item      -- Returns a Cone shape, around the X axis.
   is
      Self : Item := to_cone_Shape (radius, height);
   begin
      Self.setConeUpIndex (1);
      return Self;
   end to_coneX_Shape;




   function to_coneZ_Shape (radius, height : in math.Real) return Item      -- Returns a Cone shape, around the Z axis.
   is
      Self : Item := to_cone_Shape (radius, height);
   begin
      Self.setConeUpIndex (3);
      return Self;
   end to_coneZ_Shape;





   ---------------
   --- Attributes
   --


   function getRadius (Self : in Item) return math.Real
   is
   begin
      return Self.m_radius;
   end getRadius;




   function getHeight (Self : in Item) return math.Real
   is
   begin
      return Self.m_height;
   end getHeight;







   overriding function localGetSupportingVertex              (Self : in Item;   vec : in math.Vector_3) return math.Vector_3
   is
      use impact.d3.Vector, math.Vectors;

      supVertex : math.Vector_3 := Self.coneLocalSupport (vec);
      vecNorm   : math.Vector_3;

   begin
      if Self.getMargin /= 0.0 then
         vecnorm := vec;

         if length2 (vecnorm)  <  impact.d3.Scalar.SIMD_EPSILON * impact.d3.Scalar.SIMD_EPSILON then
            vecnorm := (-1.0,  -1.0,  -1.0);
         end if;

         normalize (vecnorm);

         supVertex := supVertex  +  Self.getMargin * vecnorm;
      end if;


      return supVertex;
   end localGetSupportingVertex;





   overriding function localGetSupportingVertexWithoutMargin (Self : in Item;   vec : in math.Vector_3) return math.Vector_3
   is
   begin
      return Self.coneLocalSupport (vec);
   end localGetSupportingVertexWithoutMargin;





   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                numVectors         : in     Integer)
   is
   begin
      for i in 1 .. numVectors
      loop
         supportVerticesOut (i) := Self.coneLocalSupport (Vectors (i));
      end loop;
   end batchedUnitVectorGetSupportingVertexWithoutMargin;






   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3)
   is
      use impact.d3.Transform, math.Vectors;

      identity : constant Transform_3d := getIdentity;

      aabbMin,
      aabbMax  : math.Vector_3;

   begin
      Self.getAabb (identity, aabbMin, aabbMax);

      declare
         halfExtents :          math.Vector_3 := (aabbMax - aabbMin) * 0.5;

         margin      : constant math.Real     := Self.getMargin;

         lx          : constant math.Real     := 2.0 * (halfExtents (1) + margin);
         ly          : constant math.Real     := 2.0 * (halfExtents (2) + margin);
         lz          : constant math.Real     := 2.0 * (halfExtents (3) + margin);

         x2          : constant math.Real     := lx * lx;
         y2          : constant math.Real     := ly * ly;
         z2          : constant math.Real     := lz * lz;

         scaledmass  : constant math.Real     := mass * 0.08333333;

      begin
         inertia := scaledmass * (y2 + z2,  x2 + z2,  x2 + y2);
      end;
   end calculateLocalInertia;







   overriding function  getName   (Self : in     Item)        return String
   is
      pragma Unreferenced (Self);
   begin
      return "Cone";
   end getName;





   procedure setConeUpIndex (Self : in out Item;   upIndex : in Integer)
   is
   begin
      case upIndex
      is
      when 1 =>
         Self.m_coneIndices (1) := 2;
         Self.m_coneIndices (2) := 1;
         Self.m_coneIndices (3) := 3;

      when 2 =>
         Self.m_coneIndices (1) := 1;
         Self.m_coneIndices (2) := 2;
         Self.m_coneIndices (3) := 3;

      when 3 =>
         Self.m_coneIndices (1) := 1;
         Self.m_coneIndices (2) := 3;
         Self.m_coneIndices (3) := 2;

      when others =>
         raise Program_Error;
      end case;
   end setConeUpIndex;





   function  getConeUpIndex (Self : in     Item)        return Integer
   is
   begin
      return Self.m_coneIndices (2);
   end getConeUpIndex;





   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3)
   is
      use math.Functions;

      axis : constant Integer := Self.m_coneIndices (2);
      r1   : constant Integer := Self.m_coneIndices (1);
      r2   : constant Integer := Self.m_coneIndices (3);

   begin
      Self.m_height   := Self.m_height  *    scaling (axis) / Self.getLocalScaling (axis);

      Self.m_radius   := Self.m_radius  *  (scaling (r1)  / Self.getLocalScaling (r1)
                                            + scaling (r2)  / Self.getLocalScaling (r2)) / 2.0;

      Self.m_sinAngle := Self.m_radius  /  sqRt (Self.m_radius * Self.m_radius
                                                 + Self.m_height * Self.m_height);


      impact.d3.Shape.convex.internal.setLocalScaling (impact.d3.Shape.convex.internal.item (Self),  scaling);   -- Call base class.
   end setLocalScaling;






   function coneLocalSupport (Self : in Item;   v : in math.Vector_3) return math.Vector_3
   is
      use impact.d3.Vector, math.Functions;
      halfHeight : constant math.Real := Self.m_height * 0.5;

      tmp        : math.Vector_3;
      s, d        : math.Real;

   begin
      if v (Self.m_coneIndices (2))  >  length (v) * Self.m_sinAngle then
         tmp (Self.m_coneIndices (1)) := 0.0;
         tmp (Self.m_coneIndices (2)) := halfHeight;
         tmp (Self.m_coneIndices (3)) := 0.0;

         return tmp;

      else
         s := sqRt (v (Self.m_coneIndices (1)) * v (Self.m_coneIndices (1))
                    + v (Self.m_coneIndices (3)) * v (Self.m_coneIndices (3)));

         if s > impact.d3.Scalar.SIMD_EPSILON then
            d := Self.m_radius / s;

            tmp (Self.m_coneIndices (1)) :=  v (Self.m_coneIndices (1)) * d;
            tmp (Self.m_coneIndices (2)) := -halfHeight;
            tmp (Self.m_coneIndices (3)) :=  v (Self.m_coneIndices (3)) * d;

            return tmp;

         else
            tmp (Self.m_coneIndices (1)) :=  0.0;
            tmp (Self.m_coneIndices (2)) := -halfHeight;
            tmp (Self.m_coneIndices (3)) :=  0.0;

            return tmp;
         end if;
      end if;
   end coneLocalSupport;



end impact.d3.Shape.convex.internal.cone;
