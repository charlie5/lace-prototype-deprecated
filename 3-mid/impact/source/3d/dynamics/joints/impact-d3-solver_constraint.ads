limited
with impact.d3.Object.rigid;

with ada.Containers.Vectors;




package impact.d3.solver_Constraint
--
--  1D constraint along a normal axis between bodyA and bodyB. It can be combined to solve contact and friction constraints.
--
is

   type union1_Kind is (solverBodyA, companionIdA);

   type union1 (Kind : union1_Kind := solverBodyA) is
      record
         case Kind is
            when solverBodyA  =>   m_solverBodyA  : access impact.d3.Object.rigid.Item'Class;
            when companionIdA =>   m_companionIdA :        Integer;
         end case;
      end record;



   type union2_Kind is (solverBodyB, companionIdB);

   type union2 (Kind : union2_Kind := solverBodyB) is
      record
         case Kind is
            when solverBodyB  =>   m_solverBodyB  : access impact.d3.Object.rigid.Item'Class;
            when companionIdB =>   m_companionIdB :        Integer;
         end case;
      end record;




   type solver_ConstraintType is (BT_SOLVER_CONTACT_1D,
                                  BT_SOLVER_FRICTION_1D);







   type Item is
      record
         m_relpos1CrossNormal,
         m_contactNormal        : aliased c_Vector_3;

         m_relpos2CrossNormal   : aliased c_Vector_3;
         --  impact.d3.Vector                m_contactNormal2;//usually m_contactNormal2 == -m_contactNormal

         m_angularComponentA,
         m_angularComponentB    : math.Vector_3;

         m_appliedPushImpulse,
         m_appliedImpulse       : math.Real;

         m_friction,
         m_jacDiagABInv         : math.Real;

         m_numConsecutiveRowsPerKernel : Integer;

         m_frictionIndex        : Integer;

         union1                 : impact.d3.solver_Constraint.union1;
         union2                 : impact.d3.solver_Constraint.union2;

         m_originalContactPoint : access Any'Class;

         m_rhs,
         m_cfm,
         m_lowerLimit,
         m_upperLimit           : aliased math.Real;

         m_rhsPenetration       : math.Real;
      end record;


   type View is access all Item;

   function  new_solver_Constraint return View;
   procedure free (Self : in out View);



   --- Containers
   --

   package Vectors           is new ada.Containers.Vectors (Positive, View);
   subtype btConstraintArray is Vectors.Vector;



   null_Constraint : constant Item := (m_relpos1CrossNormal |
                                       m_relpos2CrossNormal |
                                       m_contactNormal       => (0.0, 0.0, 0.0),
                                       m_angularComponentA  |
                                       m_angularComponentB   => (0.0, 0.0, 0.0),

                                       m_appliedPushImpulse |
                                       m_appliedImpulse     |
                                       m_friction           |
                                       m_jacDiagABInv        => 0.0,

                                       m_numConsecutiveRowsPerKernel |
                                       m_frictionIndex                => 0,

                                       union1 => (kind => solverBodyA,   m_solverBodyA => null),
                                       union2 => (kind => solverBodyB,   m_solverBodyB => null),

                                       m_originalContactPoint => null,

                                       m_rhs            |
                                       m_cfm            |
                                       m_lowerLimit     |
                                       m_upperLimit     |
                                       m_rhsPenetration  => 0.0);


end impact.d3.solver_Constraint;
