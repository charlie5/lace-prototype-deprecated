with impact.d3.Vector;
with impact.d3.Scalar;

package body impact.d3.jacobian_Entry
is

   use impact.d3.Vector;




   ----------
   --- Forge
   --

   function to_jacobian_Entry (world2A,   world2B  : in math.Matrix_3x3;
                                rel_pos1,  rel_pos2 : in math.Vector_3;

                                jointAxis           : in math.Vector_3;

                                inertiaInvA         : in math.Vector_3;
                                massInvA            : in math.Real    ;

                                inertiaInvB         : in math.Vector_3;
                                massInvB            : in math.Real    ) return Item
   is
      use Math;
      Self : Item;
   begin
      Self.m_linearJointAxis := jointAxis;

      Self.m_aJ      := world2A * (cross (rel_pos1,  Self.m_linearJointAxis));
      Self.m_bJ      := world2B * (cross (rel_pos2, -Self.m_linearJointAxis));

      Self.m_0MinvJt := Scaled (Self.m_aJ,  by => inertiaInvA);
      Self.m_1MinvJt := Scaled (Self.m_bJ,  by => inertiaInvB);

      Self.m_Adiag   :=  massInvA  +  dot (Self.m_0MinvJt, Self.m_aJ)
                       + massInvB  +  dot (Self.m_1MinvJt, Self.m_bJ);

      pragma Assert (Self.m_Adiag > 0.0);


      return Self;
   end to_jacobian_Entry;





   function to_jacobian_Entry (jointAxis                 : in math.Vector_3;
                                world2A,      world2B     : in math.Matrix_3x3;
                                inertiaInvA,  inertiaInvB : in math.Vector_3) return Item
   is
      use Math;
      Self : Item;
   begin
      Self.m_linearJointAxis := math.Origin_3d;

      Self.m_aJ      := world2A *   jointAxis;
      Self.m_bJ      := world2B * (-jointAxis);

      Self.m_0MinvJt := Scaled (Self.m_aJ, inertiaInvA);
      Self.m_1MinvJt := Scaled (Self.m_bJ, inertiaInvB);

      Self.m_Adiag   :=   dot (Self.m_0MinvJt, Self.m_aJ)
                        + dot (Self.m_1MinvJt, Self.m_bJ);

      pragma Assert (Self.m_Adiag > 0.0);


      return Self;
   end to_jacobian_Entry;








   function to_btJacobainEntry (axisInA,      axisInB     : in math.Vector_3;
                                inertiaInvA,  inertiaInvB : in math.Vector_3) return Item
   is
      use Math;
      Self : Item;
   begin
      Self.m_linearJointAxis := math.Origin_3d;

      Self.m_aJ      :=  axisInA;
      Self.m_bJ      := -axisInB;

      Self.m_0MinvJt := Scaled (Self.m_aJ, by => inertiaInvA);
      Self.m_1MinvJt := Scaled (Self.m_bJ, by => inertiaInvB);

      Self.m_Adiag   :=   dot (Self.m_0MinvJt, Self.m_aJ)
                        + dot (Self.m_1MinvJt, Self.m_bJ);

      pragma Assert (Self.m_Adiag > 0.0);


      return Self;
   end to_btJacobainEntry;






   function to_btJacobainEntry (world2A             : in math.Matrix_3x3;
                                rel_pos1,  rel_pos2 : in math.Vector_3;

                                jointAxis           : in math.Vector_3;

                                inertiaInvA         : in math.Vector_3;
                                massInvA            : in math.Real    ) return Item
   is
      use Math;
      Self : Item;
   begin
      Self.m_linearJointAxis := jointAxis;

      Self.m_aJ      := world2A * (cross (rel_pos1,  jointAxis));
      Self.m_bJ      := world2A * (cross (rel_pos2, -jointAxis));

      Self.m_0MinvJt := Scaled (Self.m_aJ, by => inertiaInvA);
      Self.m_1MinvJt := math.Origin_3d;
      Self.m_Adiag   := massInvA  +  dot (Self.m_0MinvJt, Self.m_aJ);

      pragma Assert (Self.m_Adiag > 0.0);

      return Self;
   end to_btJacobainEntry;







   ---------------
   --- Attributes
   --

   function getDiagonal (Self : in Item) return math.Real
   is
   begin
      return Self.m_Adiag;
   end getDiagonal;





   function getNonDiagonal (Self : in Item;   jacB     : in impact.d3.jacobian_Entry.Item'Class;
                                              massInvA : in math.Real               ) return math.Real
   is
      jacA : Item renames Self;

      lin  : constant math.Real := massInvA * dot (jacA.m_linearJointAxis, jacB.m_linearJointAxis);
      ang  : constant math.Real := dot (jacA.m_0MinvJt, jacB.m_aJ);

   begin
      return lin + ang;
   end getNonDiagonal;






   function getNonDiagonal (Self : in Item;   jacB               : in impact.d3.jacobian_Entry.Item'Class;
                                              massInvA, massInvB : in math.Real               ) return math.Real
   is
      use Math;
      jacA : Item renames Self;

      lin  : constant math.Vector_3 := cross (jacA.m_linearJointAxis, jacB.m_linearJointAxis);

      ang0 : constant math.Vector_3 := cross (jacA.m_0MinvJt, jacB.m_aJ);
      ang1 : constant math.Vector_3 := cross (jacA.m_1MinvJt, jacB.m_bJ);

      lin0 : constant math.Vector_3 := massInvA * lin;
      lin1 : constant math.Vector_3 := massInvB * lin;

      sum  : math.Vector_3 := ang0 + ang1 + lin0 + lin1;

   begin
      return sum (1) + sum (2) + sum (3);
   end getNonDiagonal;






   function getRelativeVelocity (Self : in Item;   linvelA,  angvelA,
                                                   linvelB,  angvelB : in math.Vector_3) return math.Real
   is
      use Math;
      linrel   : constant math.Vector_3 := cross (linvelA - linvelB,  Self.m_linearJointAxis);

      angvel_b  : math.Vector_3 := cross (angvelB, Self.m_bJ);
      angvel_a  : math.Vector_3 := cross (angvelA, Self.m_aJ)  +  angvelb  +  linrel;

      rel_vel2 : constant math.Real     := angvela (1) + angvela (2) + angvela (3);

   begin
      return rel_vel2 + impact.d3.Scalar.SIMD_EPSILON;
   end getRelativeVelocity;





end impact.d3.jacobian_Entry;
