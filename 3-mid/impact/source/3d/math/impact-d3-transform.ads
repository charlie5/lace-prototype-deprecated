
package impact.d3.Transform
--
--  The impact.d3.Transform class supports rigid transforms with only translation and rotation and no scaling/shear.
--
--  It can be used in combination with impact.d3.Vector, impact.d3.Quaternion and impact.d3.Matrix linear algebra classes.
--
is

   use Math;


   function to_Transform (q : in Quaternion;                                           -- 'q'   Rotation from quaternion.
                          c : in math.Vector_3 := math.Origin_3d) return Transform_3d;         -- 'c'   Translation from Vector.
   --
   --  Constructor from impact.d3.Quaternion (with optional impact.d3.Vector offset).



   function to_Transform (m : in math.Matrix_3x3;                                            -- 'm'   Rotation matrix.
                          c : in math.Vector_3  := math.Origin_3d) return Transform_3d;         -- 'c'   Translation from Vector.
   --
   --  Constructor from a rotatin matrix and an optional Vector offset.





   procedure mult (Self : out Transform_3d;   t1,                    --  't1' Transform 1
                                           t2 : in Transform_3d);    --  't2' Transform 2
   --
   --  Set the current transform as the value of the product of two transforms
   --
   --  Self = t1 * t2



   function transformed (Self : in Transform_3d;   x : in math.Vector_3) return math.Vector_3;
   --
   --  Return the transform of the vector.


--     function "*" (Left : in Transform_3d;   Right : in math.Vector_3) return math.Vector_3;
--     --
--     -- Same as 'transformed' above.



   function "*" (Left : in Transform_3d;   Right : in Quaternion) return Quaternion;
   --
   --  Return the transform of the Quaternion.


   function getRotation (Self : in Transform_3d) return Quaternion;
   --
   --  Return a quaternion representing the rotation.



   function getBasis (Self : in     Transform_3d) return        math.Matrix_3x3;
   function getBasis (Self : access Transform_3d) return access Matrix_3x3;


   function getOrigin (Self : in     Transform_3d) return        math.Vector_3;
   function getOrigin (Self : access Transform_3d) return access math.Vector_3;


   procedure setFromOpenGLMatrix (Self : in out Transform_3d;   m : access math.Real);      -- 'm'    A pointer to a 15 element array (   12 rotation (row major padded on the right by 1),
   --                                                                                                                            and 3 translation.
   --  Set from an array.


   procedure getOpenGLMatrix     (Self : in     Transform_3d;   m : access math.Real);      -- 'm'    As above.
   --
   --  Fill an array representation.



   procedure setOrigin (Self : in out Transform_3d;   origin : in math.Vector_3);
   --
   --  Set the translational element.


   procedure setBasis (Self : in out Transform_3d;   basis : in math.Matrix_3x3);
   --
   --  Set the rotational element by impact.d3.Matrix.



   procedure setRotation (Self : in out Transform_3d;   q : in Quaternion);
   --
   --  Set the rotational element by impact.d3.Quaternion.




   procedure setIdentity (Self : in out Transform_3d);
   --
   --  Set this transformation to the identity.




--     function "*" (Left : in Transform_3d;   Right : in Transform_3d) return Transform_3d;



   function inverse (Self : in Transform_3d) return Transform_3d;



   function getIdentity return Transform_3d;




   function inverseTimes (Self : in Transform_3d;   t     : in Transform_3d) return Transform_3d;   -- Return the inverse of this transform times the other transform
   function invXform     (Self : in Transform_3d;   inVec : in math.Vector_3) return math.Vector_3;




private


   function "*" (Left : in Transform_3d;   Right : in math.Vector_3) return math.Vector_3
                 renames transformed;


end impact.d3.Transform;




--  class impact.d3.Transform {
--


--          SIMD_FORCE_INLINE impact.d3.Vector invXform(const impact.d3.Vector& inVec) const;



--    /**@brief Return the inverse of this transform times the other transform
--     * @param t The other transform
--     * return this.inverse() * the other */
--          impact.d3.Transform inverseTimes(const impact.d3.Transform& t) const;


--  };



--
--  SIMD_FORCE_INLINE impact.d3.Vector
--  impact.d3.Transform::invXform(const impact.d3.Vector& inVec) const
--  {
--          impact.d3.Vector v = inVec - m_origin;
--          return (m_basis.transpose() * v);
--  }





--  SIMD_FORCE_INLINE impact.d3.Transform
--  impact.d3.Transform::inverseTimes(const impact.d3.Transform& t) const
--  {
--          impact.d3.Vector v = t.getOrigin() - m_origin;
--                  return impact.d3.Transform(m_basis.transposeTimes(t.m_basis),
--                          v * m_basis);
--  }


