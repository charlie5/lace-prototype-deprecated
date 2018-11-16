with impact.d3.Containers;



package impact.d3.manifold_Point
--
--  ManifoldContactPoint collects and maintains persistent contactpoints.
--  used to improve stability and performance of rigidbody dynamics response.
--
is
   use Math;


   type btConstraintRow is                -- Don't change following order of parameters
      record
         m_normal       : Vector_3;   -- Scalars (1..3);
         m_rhs,
         m_jacDiagInv,
         m_lowerLimit,
         m_upperLimit,
         m_accumImpulse : Real;
      end record;

   type btConstraintRows is array (Positive range <>) of btConstraintRow;


   subtype PfxConstraintRow is btConstraintRow;




   type Item is new Any with
      record
         m_localPointA,
         m_localPointB : Vector_3;

         m_positionWorldOnB : Vector_3;
         m_positionWorldOnA : vector_3;                        -- m_positionWorldOnA is redundant information, see getPositionWorldOnA(), but for clarity

         m_normalWorldOnB : vector_3;

         m_distance1           : Real;
         m_combinedFriction    : Real;
         m_combinedRestitution : Real;


         m_partId0 : Integer;   -- BP mod, store contact triangles
         m_partId1 : Integer;

         m_index0 : Integer;
         m_index1 : Integer;


         m_userPersistentData : Containers.Any_view;

         m_appliedImpulse : Real := 0.0;

         m_lateralFrictionInitialized : Boolean := False;
         m_appliedImpulseLateral1     : Real  := 0.0;
         m_appliedImpulseLateral2     : Real  := 0.0;
         m_contactMotion1             : Real  := 0.0;
         m_contactMotion2             : Real  := 0.0;
         m_contactCFM1                : Real  := 0.0;
         m_contactCFM2                : Real  := 0.0;

         m_lifeTime                   : Integer := 0;                 -- lifetime of the contactpoint in frames

         m_lateralFrictionDir1 : Vector_3;
         m_lateralFrictionDir2 : Vector_3;

         mConstraintRow : btConstraintRows (1 .. 3);
      end record;

   type View is access all Item'Class;




   type manifold_Points is array (Positive range <>) of aliased Item;


   function to_manifold_Point return Item;

   function to_manifold_Point (pointA,
                                pointB   : in Vector_3;
                                normal   : in Vector_3;
                                distance : in Real) return Item;


   function  getLifeTime       (Self : in Item) return Integer;
   function  getAppliedImpulse (Self : in Item) return Real;   -- this returns the most recent applied impulse, to satisfy
                                                               --  contact constraints by the constraint solver

   function  getDistance (Self : in     Item)      return Real;
   procedure setDistance (Self : in out Item;   dist : in Real);

   function  getPositionWorldOnA (Self : in Item) return Vector_3;
   function  getPositionWorldOnB (Self : in Item) return Vector_3;





end impact.d3.manifold_Point;
