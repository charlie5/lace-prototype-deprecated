
package impact.d3.Matrix
--
--  The impact.d3.Matrix provides a 3x3 rotation matrix, to perform linear algebra in combination with impact.d3.Quaternion, impact.d3.Transform and impact.d3.Vector.
--  Make sure to only include a pure orthogonal matrix without scaling.
is

   use Math;



--     function to_Matrix (Q : in Quaternion) return Matrix_3x3;

   function to_Matrix (xx, xy, xz,                              -- Constructor with row major formatting.
                       yx, yy, yz,
                       zx, zy, zz : in Real) return Matrix_3x3;


   procedure setRotation (Self : out Matrix_3x3;   From : in Quaternion);



   function getColumn (Self : in Matrix_3x3;   Which : in math.Index) return Vector_3;   -- Get a column of the matrix as a vector
   function getRow    (Self : in Matrix_3x3;   Which : in math.Index) return Vector_3;   -- Get a row    of the matrix as a vector


   function Row (Self : access Matrix_3x3;   Which : in math.Index) return access Vector_3;   -- Get a mutable reference to a row of the matrix as a vector.


   procedure setFromOpenGLSubMatrix (Self : in out Matrix_3x3;   m : access Real);
   --
   --  Set from the rotational part of a 4x4 OpenGL matrix
   --  m:   A pointer to the beginning of the array of scalars which composes the openGL matrix.



   procedure setValue (Self : out Matrix_3x3;   xx, xy, xz,
                                                yx, yy, yz,
                                                zx, zy, zz : in Real);
   --
   --  Set the values of the matrix explicitly (row major)
   --
   --          xx  Top left
   --          xy  Top Middle
   --          xz  Top Right
   --          yx  Middle Left
   --          yy  Middle Middle
   --          yz  Middle Right
   --          zx  Bottom Left
   --          zy  Bottom Middle
   --          zz  Bottom Right



   procedure setEulerYPR (Self : out Matrix_3x3;   yaw,                    -- Yaw   about Y axis
                                                   pitch,                  -- Pitch about X axis
                                                   roll : in Real);      -- Roll  about Z axis
   --
   --  Set the matrix from euler angles using YPR around YXZ respectively.


   procedure setEulerZYX (Self : out Matrix_3x3;   eulerX,                 -- Roll  about X axis
                                                   eulerY,                 -- Pitch about Y axis
                                                   eulerZ : in Real);    -- Yaw   about Z axis
   --
   --  Set the matrix from euler angles YPR around ZYX axes
   --
   --  These angles are used to produce a rotation matrix. The euler
   --  angles are applied in ZYX order. Thais is, a vector is first rotated
   --  about X then Y and then Z.



   procedure setIdentity (Self : out Matrix_3x3);
   --
   --  Set the matrix to the identity.



   function getIdentity return Matrix_3x3;




   procedure getOpenGLSubMatrix (Self : in Matrix_3x3;   matrix : access Real);
   --
   --  Fill the rotational part of an OpenGL matrix and clear the shear/perspective.
   --
   --  'matrix'    The array to be filled.




   procedure getRotation (Self : in Matrix_3x3;   q : out Quaternion);
   --
   --  Get the matrix represented as a quaternion.
   --
   --  'q'  The quaternion which will be set.



   procedure getEulerYPR (Self : in Matrix_3x3;   yaw,                  -- Yaw   around Y axis.
                                                  pitch,                -- Pitch around X axis.
                                                  roll : out Real);   -- Roll  around Z axis.
   --
   --  Get the matrix represented as euler angles around YXZ, roundtrip with setEulerYPR.



   procedure getEulerZYX (Self : in Matrix_3x3;   yaw,                                       -- Yaw   around Z axis.
                                                  pitch,                                     -- Pitch around Y axis.
                                                  roll            :     out Real;          -- Roll  around X axis.
                                                  solution_number : in      Integer := 1);   -- Which solution of two possible solutions (1 or 2) are possible values.
   --
   --  Get the matrix represented as euler angles around ZYX



   function Scaled (Self : in Matrix_3x3;   s : in Vector_3) return Matrix_3x3;   -- 's'    Scaling vector =>  The elements of the vector will scale each column
   --
   --  Create a scaled copy of the matrix



   function adjoint (Self : in Matrix_3x3) return Matrix_3x3;                     -- Return the adjoint of the matrix.



   function cofac (Self : in Matrix_3x3;   r1,                                 -- The first row to use for calculating the cofactor
                                           c1,                                 -- The first column to use for calculating the cofactor
                                           r2,                                 -- The second row to use for calculating the cofactor
                                           c2 : in Integer) return Real;     -- The second column to use for calculating the cofactor

   --
   --  Calculate the matrix cofactor.
   --
   --  See http://en.wikipedia.org/wiki/Cofactor_(linear_algebra) for more details.



   function absolute (Self : in Matrix_3x3) return Matrix_3x3;
   --
   --  Return the matrix with all values non negative.




   function transposeTimes (Self : in Matrix_3x3;   m : in Matrix_3x3) return Matrix_3x3;
   function timesTranspose (Self : in Matrix_3x3;   m : in Matrix_3x3) return Matrix_3x3;



   function tdotx (Self : in Matrix_3x3;   v : in Vector_3) return Real;
   function tdoty (Self : in Matrix_3x3;   v : in Vector_3) return Real;
   function tdotz (Self : in Matrix_3x3;   v : in Vector_3) return Real;



   procedure diagonalize (Self : in out Matrix_3x3;   rot       : access Matrix_3x3;   -- 'rot'         Stores the rotation from the coordinate system in which the matrix is diagonal
                                                                                       --               to the original coordinate system (ie. old_this = rot * new_this * rot^T).
                                                      threshold : in  Real;          -- 'threshold'   See iteration
                                                      maxSteps  : in  Integer);        -- 'iteration'   The iteration stops when all off-diagonal elements are less than the threshold multiplied
                                                                                       --               by the sum of the absolute values of the diagonal, or when maxSteps have been executed.
   --
   --  Diagonalizes this matrix by the Jacobi method.
   --
   --  Note that this matrix is assumed to be symmetric.



end impact.d3.Matrix;
