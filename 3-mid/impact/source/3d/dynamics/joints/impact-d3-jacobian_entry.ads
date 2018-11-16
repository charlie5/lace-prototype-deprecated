--  #include "LinearMath/impact.d3.Vector.h"
--  #include "BulletDynamics/Dynamics/impact.d3.Object.rigid.h"


package impact.d3.jacobian_Entry
--
--  Jacobian entry is an abstraction that allows to describe constraints
--  it can be used in combination with a constraint solver
--  Can be used to relate the effect of an impulse to the constraint error
--
--  NB: Another memory optimization would be to store m_1MinvJt in the remaining 3 w components
--      which makes the impact.d3.jacobian_Entry memory layout 16 bytes
--      if you only are interested in angular part, just feed massInvA and massInvB zero
--
is

   type Item is tagged
      record
         m_linearJointAxis,
         m_aJ,
         m_bJ,
         m_0MinvJt,
         m_1MinvJt : math.Vector_3;

         m_Adiag   : math.Real;   -- Optimization: can be stored in the w/last component of one of the vectors.
      end record;

   type Items is array (Positive range <>) of Item;



   ----------
   --- Forge
   --

   function to_jacobian_Entry (world2A,   world2B  : in math.Matrix_3x3;
                                rel_pos1,  rel_pos2 : in math.Vector_3;

                                jointAxis           : in math.Vector_3;

                                inertiaInvA         : in math.Vector_3;
                                massInvA            : in math.Real    ;

                                inertiaInvB         : in math.Vector_3;
                                massInvB            : in math.Real    ) return Item;
   --
   --  Constraint between two different rigidbodies



--          impact.d3.jacobian_Entry(
--                  const impact.d3.Matrix& world2A,
--                  const impact.d3.Matrix& world2B,
--                  const impact.d3.Vector& rel_pos1,const impact.d3.Vector& rel_pos2,
--                  const impact.d3.Vector& jointAxis,
--                  const impact.d3.Vector& inertiaInvA,
--                  const impact.d3.Scalar massInvA,
--                  const impact.d3.Vector& inertiaInvB,
--                  const impact.d3.Scalar massInvB)
--                  :m_linearJointAxis(jointAxis)
--          {
--                  m_aJ = world2A*(rel_pos1.cross(m_linearJointAxis));
--                  m_bJ = world2B*(rel_pos2.cross(-m_linearJointAxis));
--                  m_0MinvJt        = inertiaInvA * m_aJ;
--                  m_1MinvJt = inertiaInvB * m_bJ;
--                  m_Adiag = massInvA + m_0MinvJt.dot(m_aJ) + massInvB + m_1MinvJt.dot(m_bJ);
--
--                  btAssert(m_Adiag > impact.d3.Scalar(0.0));
--          }






   function to_jacobian_Entry (jointAxis                 : in math.Vector_3;
                                world2A,      world2B     : in math.Matrix_3x3;
                                inertiaInvA,  inertiaInvB : in math.Vector_3) return Item;
   --
   --  Angular constraint between two different rigidbodies


--          impact.d3.jacobian_Entry(const impact.d3.Vector& jointAxis,
--                  const impact.d3.Matrix& world2A,
--                  const impact.d3.Matrix& world2B,
--                  const impact.d3.Vector& inertiaInvA,
--                  const impact.d3.Vector& inertiaInvB)
--                  :m_linearJointAxis(impact.d3.Vector(impact.d3.Scalar(0.),impact.d3.Scalar(0.),impact.d3.Scalar(0.)))
--          {
--                  m_aJ= world2A*jointAxis;
--                  m_bJ = world2B*-jointAxis;
--                  m_0MinvJt        = inertiaInvA * m_aJ;
--                  m_1MinvJt = inertiaInvB * m_bJ;
--                  m_Adiag =  m_0MinvJt.dot(m_aJ) + m_1MinvJt.dot(m_bJ);
--
--                  btAssert(m_Adiag > impact.d3.Scalar(0.0));
--          }







   function to_btJacobainEntry (axisInA,      axisInB     : in math.Vector_3;
                                inertiaInvA,  inertiaInvB : in math.Vector_3) return Item;
   --
   --  Angular constraint between two different rigidbodies


--          impact.d3.jacobian_Entry(const impact.d3.Vector& axisInA,
--                  const impact.d3.Vector& axisInB,
--                  const impact.d3.Vector& inertiaInvA,
--                  const impact.d3.Vector& inertiaInvB)
--                  : m_linearJointAxis(impact.d3.Vector(impact.d3.Scalar(0.),impact.d3.Scalar(0.),impact.d3.Scalar(0.)))
--                  , m_aJ(axisInA)
--                  , m_bJ(-axisInB)
--          {
--                  m_0MinvJt        = inertiaInvA * m_aJ;
--                  m_1MinvJt = inertiaInvB * m_bJ;
--                  m_Adiag =  m_0MinvJt.dot(m_aJ) + m_1MinvJt.dot(m_bJ);
--
--                  btAssert(m_Adiag > impact.d3.Scalar(0.0));
--          }



   function to_btJacobainEntry (world2A             : in math.Matrix_3x3;
                                rel_pos1,  rel_pos2 : in math.Vector_3;

                                jointAxis           : in math.Vector_3;

                                inertiaInvA         : in math.Vector_3;
                                massInvA            : in math.Real    ) return Item;
   --
   --  Constraint on one rigidbody.


--          impact.d3.jacobian_Entry(
--                  const impact.d3.Matrix& world2A,
--                  const impact.d3.Vector& rel_pos1,const impact.d3.Vector& rel_pos2,
--                  const impact.d3.Vector& jointAxis,
--                  const impact.d3.Vector& inertiaInvA,
--                  const impact.d3.Scalar massInvA)
--                  :m_linearJointAxis(jointAxis)
--          {
--                  m_aJ= world2A*(rel_pos1.cross(jointAxis));
--                  m_bJ = world2A*(rel_pos2.cross(-jointAxis));
--                  m_0MinvJt        = inertiaInvA * m_aJ;
--                  m_1MinvJt = impact.d3.Vector(impact.d3.Scalar(0.),impact.d3.Scalar(0.),impact.d3.Scalar(0.));
--                  m_Adiag = massInvA + m_0MinvJt.dot(m_aJ);
--
--                  btAssert(m_Adiag > impact.d3.Scalar(0.0));
--          }








   ---------------
   --- Attributes
   --

   function getDiagonal (Self : in Item) return math.Real;

--          impact.d3.Scalar        getDiagonal() const { return m_Adiag; }




   function getNonDiagonal (Self : in Item;   jacB     : in impact.d3.jacobian_Entry.Item'Class;
                                              massInvA : in math.Real               ) return math.Real;
   --
   --  For two constraints on the same rigidbody (for example vehicle friction).


--          impact.d3.Scalar        getNonDiagonal(const impact.d3.jacobian_Entry& jacB, const impact.d3.Scalar massInvA) const
--          {
--                  const impact.d3.jacobian_Entry& jacA = *this;
--                  impact.d3.Scalar lin = massInvA * jacA.m_linearJointAxis.dot(jacB.m_linearJointAxis);
--                  impact.d3.Scalar ang = jacA.m_0MinvJt.dot(jacB.m_aJ);
--                  return lin + ang;
--          }




   function getNonDiagonal (Self : in Item;   jacB               : in impact.d3.jacobian_Entry.Item'Class;
                                              massInvA, massInvB : in math.Real               ) return math.Real;
   --
   --  For two constraints on sharing two same rigidbodies (for example two contact points between two rigidbodies).


--          impact.d3.Scalar        getNonDiagonal(const impact.d3.jacobian_Entry& jacB,const impact.d3.Scalar massInvA,const impact.d3.Scalar massInvB) const
--          {
--                  const impact.d3.jacobian_Entry& jacA = *this;
--                  impact.d3.Vector lin = jacA.m_linearJointAxis * jacB.m_linearJointAxis;
--                  impact.d3.Vector ang0 = jacA.m_0MinvJt * jacB.m_aJ;
--                  impact.d3.Vector ang1 = jacA.m_1MinvJt * jacB.m_bJ;
--                  impact.d3.Vector lin0 = massInvA * lin ;
--                  impact.d3.Vector lin1 = massInvB * lin;
--                  impact.d3.Vector sum = ang0+ang1+lin0+lin1;
--                  return sum[0]+sum[1]+sum[2];
--          }






   function getRelativeVelocity (Self : in Item;   linvelA,  angvelA,
                                                   linvelB,  angvelB : in math.Vector_3) return math.Real;

--          impact.d3.Scalar getRelativeVelocity(const impact.d3.Vector& linvelA,const impact.d3.Vector& angvelA,const impact.d3.Vector& linvelB,const impact.d3.Vector& angvelB)
--          {
--                  impact.d3.Vector linrel = linvelA - linvelB;
--                  impact.d3.Vector angvela  = angvelA * m_aJ;
--                  impact.d3.Vector angvelb  = angvelB * m_bJ;
--                  linrel *= m_linearJointAxis;
--                  angvela += angvelb;
--                  angvela += linrel;
--                  impact.d3.Scalar rel_vel2 = angvela[0]+angvela[1]+angvela[2];
--                  return rel_vel2 + SIMD_EPSILON;
--          }





end impact.d3.jacobian_Entry;
