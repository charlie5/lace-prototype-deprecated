with physics.Conversion,
     math.Algebra.linear.d3;
with physics.Vector_3;
with Ada.Text_IO; use Ada.Text_IO;




package body physics.Motor.spring.angular is


   -- nb: based on PAL physics abstraction layer
   --



   procedure update (Self : in out Item)
   is
      use math.Algebra.linear.d3, physics.Conversion;
   begin
      --nb: this only applies to global position and orientation.

      if self.is_Enabled then
         -- find cross products of actual and desired forward, up,  and right vectors; these represent the orientation error.

         declare
            use math.real_Arrays, math.Algebra.linear;

            transform     : math.Matrix_3x3 := +self.Rigid.Spin;

            actualForward : math.Vector_3   := forward_Direction (transform);
            actualUp      : math.Vector_3   := up_Direction      (transform);
            actualRight   : math.Vector_3   := right_Direction   (transform);
         begin
            if Norm_squared (actualForward) /= 0.0 then   actualForward := normalised (actualForward);   end if;
            if Norm_squared (actualUp)      /= 0.0 then   actualUp      := normalised (actualUp);        end if;
            if Norm_squared (actualRight)   /= 0.0 then   actualRight   := normalised (actualRight);     end if;

            declare
               forwardError : math.Vector_3 :=  self.desiredForward * actualForward;
               upError      : math.Vector_3 :=  self.desiredUp      * actualUp;
               rightError   : math.Vector_3 :=  self.desiredRight   * actualRight;
            begin
               if Norm_squared (forwardError) /= 0.0 then   forwardError := normalised (forwardError);   end if;
               if Norm_squared (upError)      /= 0.0 then   upError      := normalised (upError);        end if;
               if Norm_squared (rightError)   /= 0.0 then   rightError   := normalised (rightError);     end if;

               -- scale error vectors by the magnitude of the angles.
               declare
                  use Math;

                  function to_Degrees (Self : in math.Vector_3) return math.Vector_3
                  is
                  begin
                     return Self * (180.0 / Pi);
--                       return Self;
                  end;

                  f_angle : math.Real := math.Real (to_Degrees (angle_between_preNorm (self.desiredForward, actualForward)));
                  u_angle : math.Real := math.Real (to_Degrees (angle_between_preNorm (self.desiredUp,      actualUp)));
                  r_angle : math.Real := math.Real (to_Degrees (angle_between_preNorm (self.desiredRight,   actualRight)));
--                    f_angle : math.Real := math.Real ( -(angle_between_preNorm (self.desiredForward, actualForward)));
--                    u_angle : math.Real := math.Real ( -(angle_between_preNorm (self.desiredUp,      actualUp)));
--                    r_angle : math.Real := math.Real ( -(angle_between_preNorm (self.desiredRight,   actualRight)));
               begin
                  forwardError := forwardError * (-f_angle);
                  upError      := upError      * (-u_angle);
                  rightError   := rightError   * (-r_angle);

--                    put_Line (math.Image (+self.Rigid.InvInertiaTensorWorld));

                  declare  -- use the error vector to calculate torque.
                     one_Third  : constant      := 1.0 / 3.0;
                     error_Axis : math.Vector_3 := (forwardError + upError + rightError) * one_Third;   -- average the vectors into one.
                     error_Term : math.Vector_3 := self.angularKs * error_Axis;
                     vel_Term   : math.Vector_3 := self.angularKd * to_Degrees (+self.Rigid.Gyre);
--                       the_Torque : math.Vector_3 := self.Rigid.inertia_Tensor * (error_Term - vel_Term);   -- scale the torque vector by the Rigid's inertia tensor.

                     the_inv_Tensor : math.Matrix_3x3 := +self.Rigid.InvInertiaTensorWorld;
                     the_Torque     : math.Vector_3   :=  (Inverse (the_inv_Tensor)) * (error_Term - vel_Term);   -- scale the torque vector by the Rigid's inertia tensor.
--                       the_Torque     : math.Vector_3   := (error_Term - vel_Term) * Inverse (the_inv_Tensor);   -- scale the torque vector by the Rigid's inertia tensor.

                     raw_Torque : aliased physics.Vector_3.item := +(20.0 * 256.0 * the_Torque * 180.0 / math.Pi);
                  begin
--                       put_Line ("applying torque");
                     self.Rigid.apply_Torque (raw_Torque'unchecked_access);   -- tbd: check this 'scale' factor
                  end;
               end;
            end;
         end;

      end if;

   end;




--     procedure update (Self : in out Item)
--     is
--        use math.Algebra.linear.d3, physics.Conversion;
--     begin
--        --nb: this only applies to global position and orientation.
--
--        if self.is_Enabled then
--           -- find cross products of actual and desired forward, up,  and right vectors; these represent the orientation error.
--
--           declare
--              use math.real_Arrays, math.Algebra.linear;
--
--              transform     : math.Matrix_3x3 := +self.Rigid.Spin;
--
--              actualForward : math.Vector_3   := forward_Direction (transform);
--              actualUp      : math.Vector_3   := up_Direction      (transform);
--              actualRight   : math.Vector_3   := right_Direction   (transform);
--           begin
--              if Norm_squared (actualForward) /= 0.0 then   actualForward := normalised (actualForward);   end if;
--              if Norm_squared (actualUp)      /= 0.0 then   actualUp      := normalised (actualUp);        end if;
--              if Norm_squared (actualRight)   /= 0.0 then   actualRight   := normalised (actualRight);     end if;
--
--              declare
--                 forwardError : math.Vector_3 :=  self.desiredForward * actualForward;
--                 upError      : math.Vector_3 :=  self.desiredUp      * actualUp;
--                 rightError   : math.Vector_3 :=  self.desiredRight   * actualRight;
--              begin
--                 if Norm_squared (forwardError) /= 0.0 then   forwardError := normalised (forwardError);   end if;
--                 if Norm_squared (upError)      /= 0.0 then   upError      := normalised (upError);        end if;
--                 if Norm_squared (rightError)   /= 0.0 then   rightError   := normalised (rightError);     end if;
--
--                 -- scale error vectors by the magnitude of the angles.
--                 declare
--                    use Math;
--
--                    function to_Degrees (Self : in math.Vector_3) return math.Vector_3
--                    is
--                    begin
--                       return Self * (180.0 / Pi);
--  --                       return Self;
--                    end;
--
--                    f_angle : math.Real := math.Real (to_Degrees (angle_between_preNorm (self.desiredForward, actualForward)));
--                    u_angle : math.Real := math.Real (to_Degrees (angle_between_preNorm (self.desiredUp,      actualUp)));
--                    r_angle : math.Real := math.Real (to_Degrees (angle_between_preNorm (self.desiredRight,   actualRight)));
--                 begin
--                    forwardError := forwardError * (-f_angle);
--                    upError      := upError      * (-u_angle);
--                    rightError   := rightError   * (-r_angle);
--
--  --                    put_Line (Image (+self.Rigid.InvInertiaTensorWorld));
--
--                    declare  -- use the error vector to calculate torque.
--                       one_Third  : constant      := 1.0 / 3.0;
--                       error_Axis : math.Vector_3 := (forwardError + upError + rightError) * one_Third;   -- average the vectors into one.
--                       error_Term : math.Vector_3 := self.angularKs * error_Axis;
--                       vel_Term   : math.Vector_3 := self.angularKd * to_Degrees (+self.Rigid.Gyre);
--  --                       the_Torque : math.Vector_3 := self.Rigid.inertia_Tensor * (error_Term - vel_Term);   -- scale the torque vector by the Rigid's inertia tensor.
--
--                       the_inv_Tensor : math.Matrix_3x3 := +self.Rigid.InvInertiaTensorWorld;
--                       the_Torque     : math.Vector_3   := Inverse (the_inv_Tensor) * (error_Term - vel_Term);   -- scale the torque vector by the Rigid's inertia tensor.
--  --                       the_Torque     : math.Vector_3   := (error_Term - vel_Term) * Inverse (the_inv_Tensor);   -- scale the torque vector by the Rigid's inertia tensor.
--
--                       raw_Torque : aliased physics.Vector_3.item := +(the_Torque * 180.0 / math.Pi);
--                    begin
--  --                       put_Line ("applying torque");
--                       self.Rigid.apply_Torque (raw_Torque'unchecked_access);   -- tbd: check this 'scale' factor
--                    end;
--                 end;
--              end;
--           end;
--
--        end if;
--
--     end;






--
--
--  	void SpringMotor::setGlobalAttachPoint(const Point3r& p)
--  	{
--  		if (!mData.solid)
--  		{
--  			OPAL_LOGGER("warning") <<
--  				"opal::SpringMotor::setGlobalAttachPoint: Solid pointer is \
--  invalid.  Ignoring request." << std::endl;
--  			return;
--  		}
--
--  		// Convert the global point to a local point offset from the Solid's
--  		// transform.
--  		Matrix44r inv = mData.solid->getTransform();
--  		inv.invert();
--  		mData.attachOffset = inv * p;
--  	}
--
--  	Point3r SpringMotor::getGlobalAttachPoint()const
--  	{
--  		if (!mData.solid)
--  		{
--  			OPAL_LOGGER("warning") <<
--  				"opal::SpringMotor::getGlobalAttachPoint: Solid pointer is \
--  invalid.  Returning (0,0,0)." << std::endl;
--  			return Point3r();
--  		}
--
--  		// The global position is a combination of the Solid's global
--  		// transform and the spring's local offset from the Solid's
--  		// transform.
--  		Point3r localPos(mData.attachOffset[0], mData.attachOffset[1],
--  			mData.attachOffset[2]);
--  		Point3r globalPos = mData.solid->getTransform() * localPos;
--
--  		return globalPos;
--  	}
--
--  	void SpringMotor::setDesiredTransform(const Matrix44r& transform)
--  	{
--  		mData.desiredPos = transform.getPosition();
--
--  		mData.desiredForward = transform.getForward();
--  		if (0 != mData.desiredForward.lengthSquared())
--  		{
--  			mData.desiredForward.normalize();
--  		}
--
--  		mData.desiredUp = transform.getUp();
--  		if (0 != mData.desiredUp.lengthSquared())
--  		{
--  			mData.desiredUp.normalize();
--  		}
--
--  		mData.desiredRight = transform.getRight();
--  		if (0 != mData.desiredRight.lengthSquared())
--  		{
--  			mData.desiredRight.normalize();
--  		}
--  	}
--
--
--  	void SpringMotor::setDesiredOrientation(const Vec3r& forward,
--  		const Vec3r& up, const Vec3r& right)
--  	{
--  		mData.desiredForward = forward;
--  		if (0 != mData.desiredForward.lengthSquared())
--  		{
--  			mData.desiredForward.normalize();
--  		}
--
--  		mData.desiredUp = up;
--  		if (0 != mData.desiredUp.lengthSquared())
--  		{
--  			mData.desiredUp.normalize();
--  		}
--
--  		mData.desiredRight = right;
--  		if (0 != mData.desiredRight.lengthSquared())
--  		{
--  			mData.desiredRight.normalize();
--  		}
--  	}
--

end physics.Motor.spring.angular;
