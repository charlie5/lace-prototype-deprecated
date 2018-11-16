
package impact.d3.Scalar
--
--
--
is
   use Math;

   BT_BULLET_VERSION : constant := 279;





   SIMD_2_PI         : constant Real := 6.283185307179586232;
   SIMD_PI           : constant Real := SIMD_2_PI * 0.5;
   SIMD_HALF_PI      : constant Real := SIMD_2_PI * 0.25;
   SIMD_RADS_PER_DEG : constant Real := SIMD_2_PI / 360.0;
   SIMD_DEGS_PER_RAD : constant Real := 360.0 / SIMD_2_PI;
   SIMDSQRT12        : constant Real := 0.7071067811865475244008443621048490;


   function btRecipSqrt (x : in Real) return Real;   -- reciprocal square root


   SIMD_EPSILON  : constant Real := Real'Epsilon;   --- tbd: check this !
   SIMD_INFINITY : constant Real := Real'Last;


   function btAtan2Fast (y, x : Real) return Real;

   function btFuzzyZero  (x : Real) return Boolean;
   function btIsNegative (x : Real) return Boolean;

   function btEqual        (a, eps : Real) return Boolean;
   function btGreaterEqual (a, eps : Real) return Boolean;


   function btRadians  (x : Real) return Real;
   function btDegrees  (x : Real) return Real;


   --  #define BT_DECLARE_HANDLE(name) typedef struct name##__ { int unused; } *name


   function btNormalizeAngle (angleInRadians : Real) return Real;
   --
   --  Returns normalized value in range -SIMD_PI .. SIMD_PI.





   type btTypedObject is new impact.Any with
      record
         m_objectType : Integer;
      end record;
   --
   --  Rudimentary class to provide type info.


   --  struct btTypedObject
   --  {
   --          btTypedObject(int objectType)
   --                  :m_objectType(objectType)
   --          {
   --          }
   --          int        m_objectType;
   --          inline int getObjectType() const
   --          {
   --                  return m_objectType;
   --          }
   --  };




   function btFsel (a, b, c : in Real) return Real;




   function btFsels (a, b, c : in Real) return Real
                     renames btFsel;





end impact.d3.Scalar;
















--
--  SIMD_FORCE_INLINE bool btMachineIsLittleEndian()
--  {
--     long int i = 1;
--     const char *p = (const char *) &i;
--     if (p[0] == 1)  // Lowest address contains the least significant byte
--             return true;
--     else
--             return false;
--  }
--
--
--
--  ///btSelect avoids branches, which makes performance much better for consoles like Playstation 3 and XBox 360
--  ///Thanks Phil Knight. See also http://www.cellperformance.com/articles/2006/04/more_techniques_for_eliminatin_1.html
--  SIMD_FORCE_INLINE unsigned btSelect(unsigned condition, unsigned valueIfConditionNonZero, unsigned valueIfConditionZero)
--  {
--      // Set testNz to 0xFFFFFFFF if condition is nonzero, 0x00000000 if condition is zero
--      // Rely on positive value or'ed with its negative having sign bit on
--      // and zero value or'ed with its negative (which is still zero) having sign bit off
--      // Use arithmetic shift right, shifting the sign bit through all 32 bits
--      unsigned testNz = (unsigned)(((int)condition | -(int)condition) >> 31);
--      unsigned testEqz = ~testNz;
--      return ((valueIfConditionNonZero & testNz) | (valueIfConditionZero & testEqz));
--  }
--  SIMD_FORCE_INLINE int btSelect(unsigned condition, int valueIfConditionNonZero, int valueIfConditionZero)
--  {
--      unsigned testNz = (unsigned)(((int)condition | -(int)condition) >> 31);
--      unsigned testEqz = ~testNz;
--      return static_cast<int>((valueIfConditionNonZero & testNz) | (valueIfConditionZero & testEqz));
--  }
--  SIMD_FORCE_INLINE float btSelect(unsigned condition, float valueIfConditionNonZero, float valueIfConditionZero)
--  {
--  #ifdef BT_HAVE_NATIVE_FSEL
--      return (float)btFsel((impact.d3.Scalar)condition - impact.d3.Scalar(1.0f), valueIfConditionNonZero, valueIfConditionZero);
--  #else
--      return (condition != 0) ? valueIfConditionNonZero : valueIfConditionZero;
--  #endif
--  }
--
--  template<typename T> SIMD_FORCE_INLINE void btSwap(T& a, T& b)
--  {
--          T tmp = a;
--          a = b;
--          b = tmp;
--  }
--
--
--  //PCK: endian swapping functions
--  SIMD_FORCE_INLINE unsigned btSwapEndian(unsigned val)
--  {
--          return (((val & 0xff000000) >> 24) | ((val & 0x00ff0000) >> 8) | ((val & 0x0000ff00) << 8)  | ((val & 0x000000ff) << 24));
--  }
--
--  SIMD_FORCE_INLINE unsigned short btSwapEndian(unsigned short val)
--  {
--          return static_cast<unsigned short>(((val & 0xff00) >> 8) | ((val & 0x00ff) << 8));
--  }
--
--  SIMD_FORCE_INLINE unsigned btSwapEndian(int val)
--  {
--          return btSwapEndian((unsigned)val);
--  }
--
--  SIMD_FORCE_INLINE unsigned short btSwapEndian(short val)
--  {
--          return btSwapEndian((unsigned short) val);
--  }
--
--  ///btSwapFloat uses using char pointers to swap the endianness
--  ////btSwapFloat/btSwapDouble will NOT return a float, because the machine might 'correct' invalid floating point values
--  ///Not all values of sign/exponent/mantissa are valid floating point numbers according to IEEE 754.
--  ///When a floating point unit is faced with an invalid value, it may actually change the value, or worse, throw an exception.
--  ///In most systems, running user mode code, you wouldn't get an exception, but instead the hardware/os/runtime will 'fix' the number for you.
--  ///so instead of returning a float/double, we return integer/long long integer
--  SIMD_FORCE_INLINE unsigned int  btSwapEndianFloat(float d)
--  {
--      unsigned int a = 0;
--      unsigned char *dst = (unsigned char *)&a;
--      unsigned char *src = (unsigned char *)&d;
--
--      dst[0] = src[3];
--      dst[1] = src[2];
--      dst[2] = src[1];
--      dst[3] = src[0];
--      return a;
--  }
--
--  // unswap using char pointers
--  SIMD_FORCE_INLINE float btUnswapEndianFloat(unsigned int a)
--  {
--      float d = 0.0f;
--      unsigned char *src = (unsigned char *)&a;
--      unsigned char *dst = (unsigned char *)&d;
--
--      dst[0] = src[3];
--      dst[1] = src[2];
--      dst[2] = src[1];
--      dst[3] = src[0];
--
--      return d;
--  }
--
--
--  // swap using char pointers
--  SIMD_FORCE_INLINE void  btSwapEndianDouble(double d, unsigned char* dst)
--  {
--      unsigned char *src = (unsigned char *)&d;
--
--      dst[0] = src[7];
--      dst[1] = src[6];
--      dst[2] = src[5];
--      dst[3] = src[4];
--      dst[4] = src[3];
--      dst[5] = src[2];
--      dst[6] = src[1];
--      dst[7] = src[0];
--
--  }
--
--  // unswap using char pointers
--  SIMD_FORCE_INLINE double btUnswapEndianDouble(const unsigned char *src)
--  {
--      double d = 0.0;
--      unsigned char *dst = (unsigned char *)&d;
--
--      dst[0] = src[7];
--      dst[1] = src[6];
--      dst[2] = src[5];
--      dst[3] = src[4];
--      dst[4] = src[3];
--      dst[5] = src[2];
--      dst[6] = src[1];
--      dst[7] = src[0];
--
--          return d;
--  }

