with impact.d3.Object.rigid;
with ada.Unchecked_Conversion;
with impact.d3.Scalar;



package body impact.d3.Joint
is

   --- Forge
   --

   procedure define (Self : in out Item'Class;   of_type : in     impact.d3.Joint.Kind;
                                                 rbA     : access impact.d3.Object.rigid.Item'Class)
   is
      function to_Integer is new ada.Unchecked_Conversion (impact.d3.Joint.Kind, Integer);
   begin
      Self.m_objectType               := to_Integer (of_type);

      Self.m_userConstraintType       := -1;
      Self.union.m_userConstraintId   := -1;
      Self.m_breakingImpulseThreshold := impact.d3.Scalar.SIMD_INFINITY;
      Self.m_isEnabled                := True;
      Self.m_needsFeedback            := False;
      Self.m_rbA                      := rbA;
      Self.m_rbB                      := getFixedBody;
      Self.m_appliedImpulse           := 0.0;
   end define;






   procedure define (Self : in out Item'Class;   of_type : in     impact.d3.Joint.Kind;
                                                 rbA     : access impact.d3.Object.rigid.Item'Class;
                                                 rbB     : access impact.d3.Object.rigid.Item'Class)
   is
      function to_Integer is new ada.Unchecked_Conversion (impact.d3.Joint.Kind, Integer);
   begin
      Self.m_objectType               := to_Integer (of_type);

      Self.m_userConstraintType       := -1;
      Self.union.m_userConstraintId   := -1;
      Self.m_breakingImpulseThreshold := impact.d3.Scalar.SIMD_INFINITY;
      Self.m_isEnabled                := True;
      Self.m_needsFeedback            := False;
      Self.m_rbA                      := rbA;
      Self.m_rbB                      := rbB;
      Self.m_appliedImpulse           := 0.0;
   end define;






   --- Attributes
   --




   procedure internalSetAppliedImpulse (Self :    out Item;   appliedImpulse : in math.Real)
   is
   begin
      Self.m_appliedImpulse := appliedImpulse;
   end internalSetAppliedImpulse;




   function  internalGetAppliedImpulse (Self : in Item) return math.Real
   is
   begin
      return Self.m_appliedImpulse;
   end internalGetAppliedImpulse;






   function  getBreakingImpulseThreshold (Self : in Item) return math.Real
   is
   begin
      return Self.m_breakingImpulseThreshold;
   end getBreakingImpulseThreshold;




   procedure setBreakingImpulseThreshold (Self : out Item;   threshold : in math.Real)
   is
   begin
      Self.m_breakingImpulseThreshold := threshold;
   end setBreakingImpulseThreshold;




   function  isEnabled (Self : in Item) return Boolean
   is
   begin
      return Self.m_isEnabled;
   end isEnabled;



   procedure setEnabled (Self : out Item;   enabled : in Boolean)
   is
   begin
      Self.m_isEnabled := enabled;
   end setEnabled;








   function  getRigidBodyA (Self : in Item) return access impact.d3.Object.rigid.item'Class
   is
   begin
      return Self.m_rbA;
   end getRigidBodyA;




   function  getRigidBodyB (Self : in Item) return access impact.d3.Object.rigid.item'Class
   is
   begin
      return Self.m_rbB;
   end getRigidBodyB;







   function  getUserConstraintType (Self : in Item) return Integer
   is
   begin
      return Self.m_userConstraintType;
   end getUserConstraintType;





   procedure setUserConstraintType (Self : out Item;   userConstraintType : in Integer)
   is
   begin
      Self.m_userConstraintType := userConstraintType;
   end setUserConstraintType;





   procedure setUserConstraintId (Self : out Item;   uid : in Integer)
   is
   begin
      Self.union.m_userConstraintId := uid;
   end setUserConstraintId;



   function  getUserConstraintId (Self : in Item) return Integer
   is
   begin
      return Self.union.m_userConstraintId;
   end getUserConstraintId;





   procedure setUserConstraintPtr (Self : out Item;   ptr : access Any'Class)
   is
   begin
      Self.union.m_userConstraintPtr := ptr;
   end setUserConstraintPtr;




   function  getUserConstraintPtr (Self : in Item) return access Any'Class
   is
   begin
      return Self.union.m_userConstraintPtr;
   end getUserConstraintPtr;






   function  getUid (Self : in Item) return Integer
   is
   begin
      return Self.union.m_userConstraintId;
   end getUid;




   function  needsFeedback (Self : in Item) return Boolean
   is
   begin
      return Self.m_needsFeedback;
   end needsFeedback;





   procedure enableFeedback (Self : out Item;   needsFeedback : in Boolean)
   is
   begin
      Self.m_needsFeedback := needsFeedback;
   end enableFeedback;








   function  getAppliedImpulse (Self : in Item) return math.Real
   is
   begin
      pragma Assert (Self.m_needsFeedback);
      return Self.m_appliedImpulse;
   end getAppliedImpulse;





   function  getConstraintType (Self : in Item) return impact.d3.Joint.Kind
   is
      function to_joint_Kind is new ada.Unchecked_Conversion (Integer, impact.d3.Joint.Kind);
   begin
      return to_joint_Kind (Self.m_objectType);
   end getConstraintType;













   function  btAdjustAngleToLimits (angleInRadians,
                                    angleLowerLimitInRadians,
                                    angleUpperLimitInRadians : in math.Real) return math.Real
   is
      use impact.d3.Scalar;

      diffLo,
      diffHi : math.Real;

   begin

      if angleLowerLimitInRadians >= angleUpperLimitInRadians then
         return angleInRadians;

      elsif angleInRadians < angleLowerLimitInRadians then
         diffLo := abs (btNormalizeAngle (angleLowerLimitInRadians - angleInRadians));
         diffHi := abs (btNormalizeAngle (angleUpperLimitInRadians - angleInRadians));

         if diffLo < diffHi then
            return angleInRadians;
         else
            return angleInRadians + SIMD_2_PI;
         end if;

      elsif angleInRadians > angleUpperLimitInRadians then
         diffHi := abs (btNormalizeAngle (angleInRadians - angleUpperLimitInRadians));
         diffLo := abs (btNormalizeAngle (angleInRadians - angleLowerLimitInRadians));

         if diffLo < diffHi then
            return angleInRadians - SIMD_2_PI;
         else
            return angleInRadians;
         end if;

      else
         return angleInRadians;
      end if;

   end btAdjustAngleToLimits;











   --- btAngularLimit
   --


   procedure set (Self : out btAngularLimit;   low, high        : in math.Real;
                                               softness         : in math.Real := 0.9;
                                               biasFactor       : in math.Real := 0.3;
                                               relaxationFactor : in math.Real := 1.0)
   is
      use impact.d3.Scalar;
   begin
      Self.m_halfRange        := (high - low) / 2.0;
      Self.m_center           := btNormalizeAngle (low + Self.m_halfRange);
      Self.m_softness         := softness;
      Self.m_biasFactor       := biasFactor;
      Self.m_relaxationFactor := relaxationFactor;
   end set;







   procedure test (Self : in out btAngularLimit;   angle : in math.Real)
   is
      use impact.d3.Scalar;
      deviation : math.Real;
   begin
      Self.m_correction := 0.0;
      Self.m_sign       := 0.0;
      Self.m_solveLimit := False;

      if Self.m_halfRange >= 0.0 then
         deviation := btNormalizeAngle (angle - Self.m_center);

         if deviation < -Self.m_halfRange then
            Self.m_solveLimit := True;
            Self.m_correction := -(deviation + Self.m_halfRange);
            Self.m_sign       := +1.0;

         elsif deviation > Self.m_halfRange then
            Self.m_solveLimit := True;
            Self.m_correction := Self.m_halfRange - deviation;
            Self.m_sign       := -1.0;
         end if;

      end if;

   end test;






   function  getSoftness (Self : in btAngularLimit) return math.Real
   is
   begin
      return Self.m_softness;
   end getSoftness;




   function  getBiasFactor (Self : in btAngularLimit) return math.Real
   is
   begin
      return  Self.m_biasFactor;
   end getBiasFactor;







   function  getRelaxationFactor (Self : in btAngularLimit) return math.Real
   is
   begin
      return Self.m_relaxationFactor;
   end getRelaxationFactor;





   function  getCorrection (Self : in btAngularLimit) return math.Real
   is
   begin
      return Self.m_correction;
   end getCorrection;





   function  getSign (Self : in btAngularLimit) return math.Real
   is
   begin
      return Self.m_sign;
   end getSign;





   function  getHalfRange (Self : in btAngularLimit) return math.Real
   is
   begin
      return Self.m_halfRange;
   end getHalfRange;





   function  isLimit (Self : in btAngularLimit) return Boolean
   is
   begin
      return Self.m_solveLimit;
   end isLimit;





   procedure fit (Self : in btAngularLimit;   angle : in out math.Real)
   is
      use impact.d3.Scalar;
      relativeAngle : math.Real;
   begin
      if Self.m_halfRange > 0.0 then
         relativeAngle := btNormalizeAngle (angle - Self.m_center);

         if not btEqual (relativeAngle, Self.m_halfRange)
         then
            if relativeAngle > 0.0 then   angle := Self.getHigh;
            else   angle := Self.getLow;
            end if;
         end if;

      end if;
   end fit;






   function  getError (Self : in btAngularLimit) return math.Real
   is
   begin
      return Self.m_correction * Self.m_sign;
   end getError;



   function  getLow  (Self : in btAngularLimit) return math.Real
   is
      use impact.d3.Scalar;
   begin
      return btNormalizeAngle (Self.m_center - Self.m_halfRange);
   end getLow;




   function  getHigh (Self : in btAngularLimit) return math.Real
   is
      use impact.d3.Scalar;
   begin
      return btNormalizeAngle (Self.m_center + Self.m_halfRange);
   end getHigh;






   function getMotorFactor (Self : in Item;   pos,
                                              lowLim, uppLim,
                                              vel,
                                              timeFact      : in math.Real) return math.Real
   is
      pragma Unreferenced (Self);
      lim_fact,
      delta_max : math.Real;

   begin
      if    lowLim > uppLim then   return 1.0;
      elsif lowLim = uppLim then   return 0.0;
      end if;


      lim_fact  := 1.0;
      delta_max := vel / timeFact;


      if delta_max < 0.0 then

         if pos >= lowLim and then pos < lowLim - delta_max then
            lim_fact := (lowLim - pos) / delta_max;

         elsif pos  < lowLim then
            lim_fact := 0.0;

         else
            lim_fact := 1.0;
         end if;


      elsif delta_max > 0.0 then

         if pos <= uppLim and then pos > uppLim - delta_max then
            lim_fact := (uppLim - pos) / delta_max;

         elsif pos  > uppLim then
            lim_fact := 0.0;

         else
            lim_fact := 1.0;
         end if;


      else
         lim_fact := 0.0;
      end if;


      return lim_fact;
   end getMotorFactor;






--     s_fixed : aliased impact.d3.Object.rigid.Item := impact.d3.Object.rigid.to_impact.d3.Object.rigid (0.0, null, null);
   s_fixed : access impact.d3.Object.rigid.Item'Class;

   function getFixedBody return access impact.d3.Object.rigid.Item'Class
   is
   begin
      if s_fixed = null then
         s_fixed := new impact.d3.Object.rigid.Item'(impact.d3.Object.rigid.Forge.to_rigid_Object (0.0, null, null));
      end if;

      s_fixed.setMassProps (0.0,  (0.0, 0.0, 0.0));

      return s_fixed;
   end getFixedBody;




end impact.d3.Joint;
