
package body impact.d3.Scalar
is


   function btRecipSqrt (x : in Real) return Real
   is
      use math.Functions;
   begin
      return 1.0 / sqRt (x);
   end btRecipSqrt;



   function btAtan2Fast (y, x : Real) return Real
   is
      coeff_1 : constant Real := SIMD_PI / 4.0;
      coeff_2 : constant Real := 3.0 * coeff_1;
      abs_y   : constant Real := abs (y);
      angle   : Real;
      r       : Real;
   begin
      if x >= 0.0 then
         r     := (x - abs_y) / (x + abs_y);
         angle := coeff_1 - coeff_1 * r;
      else
         r     := (x + abs_y) / (abs_y - x);
         angle := coeff_2 - coeff_1 * r;
      end if;

      if y < 0.0 then
         return -angle;
      else
         return angle;
      end if;

   end btAtan2Fast;




   function btFuzzyZero (x    : Real) return Boolean
   is
   begin
      return abs (x) < SIMD_EPSILON;
   end btFuzzyZero;



   function btEqual     (a, eps : Real) return Boolean
   is
   begin
      return (a <= eps) and then not (a < -eps);

   end btEqual;



   function btGreaterEqual (a, eps : Real) return Boolean
   is
   begin
      return not (a <= eps);
   end btGreaterEqual;



   function btIsNegative (x : Real) return Boolean
   is
   begin
      if x < 0.0 then
         return True;
      else
         return False;
      end if;
   end btIsNegative;




   function btRadians  (x : Real) return Real
   is
   begin
      return x * SIMD_RADS_PER_DEG;
   end btRadians;


   function btDegrees  (x : Real) return Real
   is
   begin
      return x * SIMD_DEGS_PER_RAD;
   end btDegrees;



   function btFmod (x, y : in Real) return Real
   is
      type Sign_t is (Negative, Positive);

      function Sign (Self : in Real) return Sign_t
      is
      begin
         if Self < 0.0 then   return Negative;   else   return Positive;   end if;
      end Sign;

      f : Real;
   begin
      f := Real'Remainder (x, y);

      if Sign (x) = Sign (f) then
         return f;
      elsif Sign (y) = Sign (f) then
         return f - y;
      else
         return f + y;
      end if;
   end btFmod;




   function btNormalizeAngle (angleInRadians : Real) return Real
   is
      the_Angle : constant Real := btFmod (angleInRadians, SIMD_2_PI);   -- tbd: check Ada 'Mod matches C++ fmod
   begin
      if angleInRadians < -SIMD_PI then
         return the_Angle + SIMD_2_PI;

      elsif angleInRadians > SIMD_PI then
         return the_Angle - SIMD_2_PI;

      else
         return the_Angle;
      end if;
   end btNormalizeAngle;





   function btFsel (a, b, c : in Real) return Real
   is
   begin
      if a >= 0.0 then
         return b;
      else
         return c;
      end if;
   end btFsel;



end impact.d3.Scalar;







--  #define BT_DECLARE_HANDLE(name) typedef struct name##__ { int unused; } *name
--
--  #ifndef btFsel
--  SIMD_FORCE_INLINE impact.d3.Scalar btFsel(impact.d3.Scalar a, impact.d3.Scalar b, impact.d3.Scalar c)
--  {
--          return a >= 0 ? b : c;
--  }
--  #endif
--  #define btFsels(a,b,c) (impact.d3.Scalar)btFsel(a,b,c)
--
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
--
--
--  ///rudimentary class to provide type info
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
--  #endif //BT_SCALAR_H
