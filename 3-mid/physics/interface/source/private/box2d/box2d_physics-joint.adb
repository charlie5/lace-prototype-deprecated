with
     box2d_c.Binding,
     box2d_physics.Object,

     c_math_c.Vector_3,
     c_math_c.Matrix_4x4,
     c_math_c.Conversion,
     Swig,
     interfaces.C,

     ada.unchecked_Deallocation,
     ada.unchecked_Conversion;

package body box2d_Physics.Joint
is
   use c_math_c.Conversion,
       box2d_c.Binding,
       Interfaces;


   type Any_limited_view is access all lace.Any.limited_item'Class;

   function to_Any_view    is new ada.unchecked_Conversion (Swig.void_ptr, Any_limited_view);
   function to_Object_view is new ada.unchecked_Conversion (swig.void_ptr, physics.Object.view);
   pragma Unreferenced (to_Object_view);


   --  procedure set_b2d_user_Data (Self : in View)
   --  is
   --     function to_void_ptr is new ada.Unchecked_Conversion (Any_limited_view, Swig.void_ptr);
   --     Self_as_any : constant Any_limited_view := Any_limited_view (Self);
   --  begin
   --     b2d_Joint_user_Data_is (Self.C, to_void_ptr (Self_as_any));
   --  end set_b2d_user_Data;


   overriding
   function reaction_Force (Self : in Item) return Vector_3
   is
   begin
      return +b2d_Joint_reaction_Force (Self.C);
   end reaction_Force;


   overriding
   function reaction_Torque (Self : in Item) return Real
   is
   begin
      return +b2d_Joint_reaction_Torque (Self.C);
   end reaction_Torque;


   overriding
   procedure user_Data_is (Self : in out Item;   Now : access lace.Any.limited_item'Class)
   is
   begin
      Self.user_Data := Now;
   end user_Data_is;


   overriding
   function user_Data (Self : in Item) return access lace.Any.limited_item'Class
   is
   begin
      return Self.user_Data;
   end user_Data;


   --------
   --  DoF6
   --

   function new_Dof6_Joint (Object_A, Object_B : in physics.Object.view;
                            Frame_A,  Frame_B  : in Matrix_4x4) return physics.Joint.DoF6.view
   is
      Self       : constant access DoF6 := new DoF6;
      pragma Unreferenced (Self);

      c_Object_A : box2d_C.Pointers.Object_Pointer := box2d_physics.Object.view (Object_A).C;
      c_Object_B : box2d_C.Pointers.Object_Pointer := box2d_physics.Object.view (Object_B).C;

      c_Frame_A  : aliased c_math_c.Matrix_4x4.item := +Frame_A;
      c_Frame_B  : aliased c_math_c.Matrix_4x4.item := +Frame_B;
   begin
      return null;
   end new_Dof6_Joint;


   overriding
   procedure destruct (Self : in out DoF6)
   is
   begin
      raise Program_Error with "TBD";
   end destruct;


   overriding
   function Object_A (Self : in DoF6) return physics.Object.view
   is
      c_Object_A : constant box2d_c.Pointers.Object_Pointer := b2d_Joint_Object_A (Self.C);
   begin
      return physics.Object.view (to_Any_view (b2d_Object_user_Data (c_Object_A)));
   end Object_A;


   overriding
   function Object_B (Self : in DoF6) return physics.Object.view
   is
      c_Object_B : constant box2d_c.Pointers.Object_Pointer := b2d_Joint_Object_B (Self.C);
   begin
      return physics.Object.view (to_Any_view (b2d_Object_user_Data (c_Object_B)));
   end Object_B;


   overriding
   function Frame_A (Self : in DoF6) return Matrix_4x4
   is
   begin
      return +b2d_Joint_Frame_A (Self.C);
   end Frame_A;


   overriding
   function Frame_B (Self : in DoF6) return Matrix_4x4
   is
   begin
      return +b2d_Joint_Frame_B (Self.C);
   end Frame_B;


   overriding
   procedure Frame_A_is (Self : in out DoF6;   Now : in Matrix_4x4)
   is
      c_Now : aliased c_math_c.Matrix_4x4.item := +Now;
   begin
      b2d_Joint_Frame_A_is (Self.C, c_Now'unchecked_Access);
   end Frame_A_is;


   overriding
   procedure Frame_B_is (Self : in out DoF6;   Now : in Matrix_4x4)
   is
      c_Now : aliased c_math_c.Matrix_4x4.item := +Now;
   begin
      b2d_Joint_Frame_B_is (Self.C, c_Now'unchecked_Access);
   end Frame_B_is;


   overriding
   function is_Limited (Self : in DoF6;   DoF : Degree_of_freedom) return Boolean
   is
      use type Swig.bool;
   begin
      return b2d_Joint_is_Limited (Self.C, Degree_of_freedom'Pos (DoF)) /= 0;
   end is_Limited;


   overriding
   procedure Velocity_is (Self : in out DoF6;   Now : in Real;
                                                DoF : in Degree_of_freedom)
   is
   begin
      if DoF < 4 then
         raise Error with "Illegal degree of freedom:" & Degree_of_freedom'Image (DoF);
      end if;

      b2d_Joint_Velocity_is (Self.C, C.int (Now),
                                     c_math_c.Real (DoF));
   end Velocity_is;


   overriding
   function Extent (Self : in DoF6;   DoF : Degree_of_freedom) return Real
   is
   begin
      if DoF < 4 then
         raise Error with "Illegal degree of freedom:" & Degree_of_freedom'Image (DoF);
      end if;

      return Real (b2d_Joint_Extent (Self.C, C.int (DoF)));
   end Extent;


   overriding
   procedure desired_Extent_is (Self : in out DoF6;   Now : in Real;
                                                      DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end desired_Extent_is;


   overriding
   function lower_Limit (Self : in DoF6;   DoF : in Degree_of_freedom) return Real
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end lower_Limit;


   overriding
   function upper_Limit (Self : in DoF6;   DoF : in Degree_of_freedom) return Real
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end upper_Limit;


   overriding
   procedure lower_Limit_is (Self : in out DoF6;   Now : in Real;
                                                   DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end lower_Limit_is;


   overriding
   procedure upper_Limit_is (Self : in out DoF6;   Now : in Real;
                                                   DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end upper_Limit_is;


   --------
   --  Ball
   --

   function new_Ball_Joint (Object_A,   Object_B   : in physics.Object.view;
                            Pivot_in_A, Pivot_in_B : in Vector_3) return physics.Joint.ball.view
   is
      Self         : constant access Ball := new Ball;

      c_Object_A   : constant box2d_C.Pointers.Object_Pointer := box2d_physics.Object.view (Object_A).C;
      c_Object_B   : constant box2d_C.Pointers.Object_Pointer := box2d_physics.Object.view (Object_B).C;

      c_Pivot_in_A : aliased c_math_c.Vector_3.item := +Pivot_in_A;
      c_Pivot_in_B : aliased c_math_c.Vector_3.item := +Pivot_in_B;
   begin
      Self.C := b2d_new_ball_Joint (c_Object_A,
                                    c_Object_B,
                                    c_Pivot_in_A'unchecked_Access,
                                    c_Pivot_in_B'unchecked_Access);
      return Self;
   end new_Ball_Joint;


   overriding
   procedure destruct (Self : in out Ball)
   is
   begin
      raise Error with "TODO";
   end destruct;


   overriding
   function Object_A (Self : in Ball) return physics.Object.view
   is
      c_Object_A : constant box2d_c.Pointers.Object_Pointer := b2d_Joint_Object_A (Self.C);
   begin
      return physics.Object.view (to_Any_view (b2d_Object_user_Data (c_Object_A)));
   end Object_A;


   overriding
   function Object_B (Self : in Ball) return physics.Object.view
   is
      c_Object_B : constant box2d_c.Pointers.Object_Pointer := b2d_Joint_Object_B (Self.C);
   begin
      return physics.Object.view (to_Any_view (b2d_Object_user_Data (c_Object_B)));
   end Object_B;


   overriding
   function Frame_A (Self : in Ball) return Matrix_4x4
   is
   begin
      return +b2d_Joint_Frame_A (Self.C);
   end Frame_A;


   overriding
   function Frame_B (Self : in Ball) return Matrix_4x4
   is
   begin
      return +b2d_Joint_Frame_B (Self.C);
   end Frame_B;


   overriding
   procedure Frame_A_is (Self : in out Ball;   Now : in Matrix_4x4)
   is
      c_Now : aliased c_math_c.Matrix_4x4.item := +Now;
   begin
      b2d_Joint_Frame_A_is (Self.C, c_Now'unchecked_Access);
   end Frame_A_is;


   overriding
   procedure Frame_B_is (Self : in out Ball;   Now : in Matrix_4x4)
   is
      c_Now : aliased c_math_c.Matrix_4x4.item := +Now;
   begin
      b2d_Joint_Frame_B_is (Self.C, c_Now'unchecked_Access);
   end Frame_B_is;


   overriding
   function is_Limited (Self : in Ball;   DoF : Degree_of_freedom) return Boolean
   is
      use type Swig.bool;
   begin
      return b2d_Joint_is_Limited (Self.C, Degree_of_freedom'Pos (DoF))  /=  0;
   end is_Limited;


   overriding
   procedure Velocity_is (Self : in out Ball;   Now : in Real;
                                                DoF : in Degree_of_freedom)
   is
   begin
      if DoF < 4 then
         raise Error with "Illegal degree of freedom:" & Degree_of_freedom'Image (DoF);
      end if;

      b2d_Joint_Velocity_is (Self.C, C.int (Now),
                                     c_math_c.Real (DoF));
   end Velocity_is;


   overriding
   function Extent (Self : in Ball;   DoF : Degree_of_freedom) return Real
   is
   begin
      if DoF < 4 then
         raise Error with "Illegal degree of freedom:" & Degree_of_freedom'Image (DoF);
      end if;

      return Real (b2d_Joint_Extent (Self.C, C.int (DoF)));
   end Extent;


   overriding
   procedure desired_Extent_is (Self : in out Ball;   Now : in Real;
                                                      DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end desired_Extent_is;


   overriding
   function lower_Limit (Self : in Ball;   DoF : in Degree_of_freedom) return Real
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end lower_Limit;


   overriding
   function upper_Limit (Self : in Ball;   DoF : in Degree_of_freedom) return Real
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end upper_Limit;


   overriding
   procedure lower_Limit_is (Self : in out Ball;   Now : in Real;
                                                   DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end lower_Limit_is;


   overriding
   procedure upper_Limit_is (Self : in out Ball;   Now : in Real;
                                                   DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end upper_Limit_is;


   ----------
   --  Slider
   --

   function new_Slider_Joint (Object_A, Object_B : in physics.Object.view;
                              Frame_A,  Frame_B  : in Matrix_4x4) return physics.Joint.slider.view
   is
      Self       : constant access Slider := new Slider;

      c_Object_A : constant box2d_C.Pointers.Object_Pointer := box2d_physics.Object.view (Object_A).C;
      c_Object_B : constant box2d_C.Pointers.Object_Pointer := box2d_physics.Object.view (Object_B).C;

      c_Frame_A  : aliased  c_math_c.Matrix_4x4.item := +Frame_A;
      c_Frame_B  : aliased  c_math_c.Matrix_4x4.item := +Frame_B;
   begin
      Self.C := b2d_new_slider_Joint (c_Object_A,
                                      c_Object_B,
                                      c_Frame_A'unchecked_Access,
                                      c_Frame_B'unchecked_Access);
      return Self;
   end new_Slider_Joint;


   overriding
   procedure destruct (Self : in out Slider)
   is
   begin
      raise Error with "TODO";
   end destruct;


   overriding
   function Object_A (Self : in Slider) return physics.Object.view
   is
      c_Object_A : constant box2d_c.Pointers.Object_Pointer := b2d_Joint_Object_A (Self.C);
   begin
      return physics.Object.view (to_Any_view (b2d_Object_user_Data (c_Object_A)));
   end Object_A;


   overriding
   function Object_B (Self : in Slider) return physics.Object.view
   is
      c_Object_B : constant box2d_c.Pointers.Object_Pointer := b2d_Joint_Object_B (Self.C);
   begin
      return physics.Object.view (to_Any_view (b2d_Object_user_Data (c_Object_B)));
   end Object_B;


   overriding
   function Frame_A (Self : in Slider) return Matrix_4x4
   is
   begin
      return +b2d_Joint_Frame_A (Self.C);
   end Frame_A;


   overriding
   function Frame_B (Self : in Slider) return Matrix_4x4
   is
   begin
      return +b2d_Joint_Frame_B (Self.C);
   end Frame_B;


   overriding
   procedure Frame_A_is (Self : in out Slider;   Now : in Matrix_4x4)
   is
      c_Now : aliased c_math_c.Matrix_4x4.item := +Now;
   begin
      b2d_Joint_Frame_A_is (Self.C, c_Now'unchecked_Access);
   end Frame_A_is;


   overriding
   procedure Frame_B_is (Self : in out Slider;   Now : in Matrix_4x4)
   is
      c_Now : aliased c_math_c.Matrix_4x4.item := +Now;
   begin
      b2d_Joint_Frame_B_is (Self.C, c_Now'unchecked_Access);
   end Frame_B_is;


   overriding
   function is_Limited (Self : in Slider;   DoF : Degree_of_freedom) return Boolean
   is
      use type Swig.bool;
   begin
      return b2d_Joint_is_Limited (Self.C, Degree_of_freedom'Pos (DoF)) /= 0;
   end is_Limited;


   overriding
   procedure Velocity_is (Self : in out Slider;   Now : in Real;
                                                  DoF : in Degree_of_freedom)
   is
   begin
      if DoF < 4 then
         raise Error with "Illegal degree of freedom:" & Degree_of_freedom'Image (DoF);
      end if;

      b2d_Joint_Velocity_is (Self.C, C.int (Now),
                                     c_math_c.Real (DoF));
   end Velocity_is;


   overriding
   function Extent (Self : in Slider;   DoF : Degree_of_freedom) return Real
   is
   begin
      if DoF < 4 then
         raise Error with "Illegal degree of freedom:" & Degree_of_freedom'Image (DoF);
      end if;

      return Real (b2d_Joint_Extent (Self.C, C.int (DoF)));
   end Extent;


   overriding
   procedure desired_Extent_is (Self : in out Slider;   Now : in Real;
                                                        DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end desired_Extent_is;


   overriding
   function lower_Limit (Self : in Slider;   DoF : in Degree_of_freedom) return Real
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end lower_Limit;


   overriding
   function upper_Limit (Self : in Slider;   DoF : in Degree_of_freedom) return Real
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end upper_Limit;


   overriding
   procedure lower_Limit_is (Self : in out Slider;   Now : in Real;
                                                     DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end lower_Limit_is;


   overriding
   procedure upper_Limit_is (Self : in out Slider;   Now : in Real;
                                                     DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end upper_Limit_is;


   --------------
   --  cone_Twist
   --

   function new_cone_Twist_Joint (Object_A, Object_B : in physics.Object.view;
                                  Frame_A,  Frame_B  : in Matrix_4x4) return physics.Joint.cone_twist.view
   is
      Self       : constant access cone_Twist := new cone_Twist;

      c_Object_A : constant box2d_C.Pointers.Object_Pointer := box2d_physics.Object.view (Object_A).C;
      c_Object_B : constant box2d_C.Pointers.Object_Pointer := box2d_physics.Object.view (Object_B).C;

      c_Frame_A  : aliased c_math_c.Matrix_4x4.item := +Frame_A;
      c_Frame_B  : aliased c_math_c.Matrix_4x4.item := +Frame_B;
   begin
      Self.C := b2d_new_DoF6_Joint (c_Object_A,
                                    c_Object_B,
                                    c_Frame_A'unchecked_Access,
                                    c_Frame_B'unchecked_Access);
      return Self;
   end new_cone_Twist_Joint;


   overriding
   procedure destruct (Self : in out cone_Twist)
   is
   begin
      raise Error with "TODO";
   end destruct;


   overriding
   function Object_A (Self : in cone_Twist) return physics.Object.view
   is
      c_Object_A : constant box2d_c.Pointers.Object_Pointer := b2d_Joint_Object_A (Self.C);
   begin
      return physics.Object.view (to_Any_view (b2d_Object_user_Data (c_Object_A)));
   end Object_A;


   overriding
   function Object_B (Self : in cone_Twist) return physics.Object.view
   is
      c_Object_B : constant box2d_c.Pointers.Object_Pointer := b2d_Joint_Object_B (Self.C);
   begin
      return physics.Object.view (to_Any_view (b2d_Object_user_Data (c_Object_B)));
   end Object_B;


   overriding
   function Frame_A (Self : in cone_Twist) return Matrix_4x4
   is
   begin
      return +b2d_Joint_Frame_A (Self.C);
   end Frame_A;


   overriding
   function Frame_B (Self : in cone_Twist) return Matrix_4x4
   is
   begin
      return +b2d_Joint_Frame_B (Self.C);
   end Frame_B;


   overriding
   procedure Frame_A_is (Self : in out cone_Twist;   Now : in Matrix_4x4)
   is
      c_Now : aliased c_math_c.Matrix_4x4.item := +Now;
   begin
      b2d_Joint_Frame_A_is (Self.C, c_Now'unchecked_Access);
   end Frame_A_is;


   overriding
   procedure Frame_B_is (Self : in out cone_Twist;   Now : in Matrix_4x4)
   is
      c_Now : aliased c_math_c.Matrix_4x4.item := +Now;
   begin
      b2d_Joint_Frame_B_is (Self.C, c_Now'unchecked_Access);
   end Frame_B_is;


   overriding
   function is_Limited (Self : in cone_Twist;   DoF : Degree_of_freedom) return Boolean
   is
      use type Swig.bool;
   begin
      return b2d_Joint_is_Limited (Self.C, Degree_of_freedom'Pos (DoF))  /=  0;
   end is_Limited;


   overriding
   procedure Velocity_is (Self : in out cone_Twist;   Now : in Real;
                                                      DoF : in Degree_of_freedom)
   is
   begin
      if DoF < 4 then
         raise Error with "Illegal degree of freedom:" & Degree_of_freedom'Image (DoF);
      end if;

      b2d_Joint_Velocity_is (Self.C, C.int (Now),
                                     c_math_c.Real (DoF));
   end Velocity_is;


   overriding
   function Extent (Self : in cone_Twist;   DoF : Degree_of_freedom) return Real
   is
   begin
      if DoF < 4 then
         raise Error with "Illegal degree of freedom:" & Degree_of_freedom'Image (DoF);
      end if;

      return Real (b2d_Joint_Extent (Self.C, C.int (DoF)));
   end Extent;


   overriding
   procedure desired_Extent_is (Self : in out cone_Twist;   Now : in Real;
                                                            DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end desired_Extent_is;


   overriding
   function lower_Limit (Self : in cone_Twist;   DoF : in Degree_of_freedom) return Real
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end lower_Limit;


   overriding
   function upper_Limit (Self : in cone_Twist;   DoF : in Degree_of_freedom) return Real
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end upper_Limit;


   overriding
   procedure lower_Limit_is (Self : in out cone_Twist;   Now : in Real;
                                                         DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end lower_Limit_is;


   overriding
   procedure upper_Limit_is (Self : in out cone_Twist;   Now : in Real;
                                                         DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end upper_Limit_is;


   ---------
   --  Hinge
   --

   function new_hinge_Joint (in_Space                 : in box2d_c.Pointers.Space_Pointer;
                             Object_A,    Object_B    : in physics.Object.view;
                             Anchor_in_A, Anchor_in_B : in Vector_3;
                             low_Limit,   high_Limit  : in math.Real;
                             collide_Conected         : in Boolean) return physics.Joint.hinge.view
   is
      use type box2d_physics.Object.view,
               physics.Object.view;

      Self           : constant access Hinge := new Hinge;

      c_Object_A     : box2d_C.Pointers.Object_Pointer;
      c_Object_B     : box2d_C.Pointers.Object_Pointer;

      c_Anchor_in_A  : aliased c_math_c.Vector_3.item := +Anchor_in_A;
      c_Anchor_in_B  : aliased c_math_c.Vector_3.item := +Anchor_in_B;

   begin
      if   Object_A = null
        or Object_B = null
      then
         raise Error with "Null object detected.";
      end if;

      if box2d_physics.Object.view (Object_A) /= null
      then
         c_Object_A := box2d_physics.Object.view (Object_A).C;
      end if;

      if box2d_physics.Object.view (Object_B) /= null
      then
         c_Object_B := box2d_physics.Object.view (Object_B).C;
      end if;

      Self.C := b2d_new_hinge_Joint_with_local_anchors (in_Space,
                                                        c_Object_A,
                                                        c_Object_B,
                                                        c_Anchor_in_A'unchecked_Access,
                                                        c_Anchor_in_B'unchecked_Access,
                                                        c_math_c.Real (low_Limit),
                                                        c_math_c.Real (high_Limit),
                                                        Boolean'Pos (collide_Conected));
      return Self;
   end new_hinge_Joint;


   function new_hinge_Joint (Object_A : in physics.Object.view;
                             Frame_A  : in Matrix_4x4) return physics.Joint.hinge.view
   is
      use type box2d_physics.Object.view;

      Self : constant access Hinge := new Hinge;

      c_Object_A : constant box2d_C.Pointers.Object_Pointer := box2d_physics.Object.view (Object_A).C;
      c_Frame_A  : aliased  c_math_c.Matrix_4x4.item        := +Frame_A;
   begin
      Self.C := b2d_new_space_hinge_Joint (c_Object_A,
                                           c_Frame_A'unchecked_Access);
      return Self;
   end new_hinge_Joint;


   function new_hinge_Joint (in_Space              : in box2d_c.Pointers.Space_Pointer;
                             Object_A,  Object_B   : in physics.Object.view;
                             Frame_A,   Frame_B    : in Matrix_4x4;
                             low_Limit, high_Limit : in math.Real;
                             collide_Conected      : in Boolean) return physics.Joint.hinge.view
   is
      use type box2d_physics.Object.view,
               physics.Object.view;

      Self : constant access Hinge := new Hinge;

      c_Object_A : box2d_C.Pointers.Object_Pointer;
      c_Object_B : box2d_C.Pointers.Object_Pointer;

      c_Frame_A  : aliased c_math_c.Matrix_4x4.item := +Frame_A;
      c_Frame_B  : aliased c_math_c.Matrix_4x4.item := +Frame_B;
   begin
      if   Object_A = null
        or Object_B = null
      then
         raise Error with "Null object detected.";
      end if;

      if box2d_physics.Object.view (Object_A) /= null
      then
         c_Object_A := box2d_physics.Object.view (Object_A).C;
      end if;

      if box2d_physics.Object.view (Object_B) /= null
      then
         c_Object_B := box2d_physics.Object.view (Object_B).C;
      end if;

      Self.C := b2d_new_hinge_Joint (in_Space,
                                     c_Object_A,
                                     c_Object_B,
                                     c_Frame_A'unchecked_Access,
                                     c_Frame_B'unchecked_Access,
                                     c_math_c.Real (low_Limit),
                                     c_math_c.Real (high_Limit),
                                     Boolean'Pos (collide_Conected));
      return Self;
   end new_hinge_Joint;


   overriding
   procedure destruct (Self : in out Hinge)
   is
   begin
      b2d_free_hinge_Joint (Self.C);
      Self.C := null;
   end destruct;


   overriding
   procedure Limits_are (Self : in out Hinge;   Low, High        : in Real;
                                                Softness         : in Real := 0.9;
                                                biasFactor       : in Real := 0.3;
                                                relaxationFactor : in Real := 1.0)
   is
   begin
      b2d_Joint_hinge_Limits_are (Self.C, c_math_c.Real (Low),
                                          c_math_c.Real (High));
   end Limits_are;


   overriding
   function lower_Limit (Self : in Hinge) return Real
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end lower_Limit;


   overriding
   function upper_Limit (Self : in Hinge) return Real
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end upper_Limit;


   overriding
   function Angle (Self : in Hinge) return Real
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end Angle;


   overriding
   function Object_A (Self : in Hinge) return physics.Object.view
   is
   begin
      raise Error with "TODO";
      return null;
   end Object_A;


   overriding
   function Object_B (Self : in Hinge) return physics.Object.view
   is
   begin
      raise Error with "TODO";
      return null;
   end Object_B;


   overriding
   function Frame_A (Self : in Hinge) return Matrix_4x4
   is
      c_Frame : aliased c_math_c.Matrix_4x4.item;
   begin
      raise Error with "TODO";
      return +c_Frame;
   end Frame_A;


   overriding
   function Frame_B (Self : in Hinge) return Matrix_4x4
   is
      c_Frame : aliased c_math_c.Matrix_4x4.item;
   begin
      raise Error with "TODO";
      return +c_Frame;
   end Frame_B;


   overriding
   procedure Frame_A_is (Self : in out Hinge;   Now : in Matrix_4x4)
   is
      c_Frame : aliased constant c_math_c.Matrix_4x4.item := +Now;
      pragma Unreferenced (c_Frame);
   begin
      raise Error with "TODO";
   end Frame_A_is;


   overriding
   procedure Frame_B_is (Self : in out Hinge;   Now : in Matrix_4x4)
   is
      c_Frame : aliased constant c_math_c.Matrix_4x4.item := +Now;
      pragma Unreferenced (c_Frame);
   begin
      raise Error with "TODO";
   end Frame_B_is;


   overriding
   function is_Limited (Self : in Hinge;   DoF : Degree_of_freedom) return Boolean
   is
      pragma unreferenced (Self);
   begin
      return DoF = 1;
   end is_Limited;


   overriding
   procedure Velocity_is (Self : in out Hinge;   Now : in Real;
                                                 DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";

      if DoF /= 1 then
         raise Error with "Illegal degree of freedom:" & Degree_of_freedom'Image (DoF);
      end if;
   end Velocity_is;


   overriding
   function Extent (Self : in Hinge;   DoF : Degree_of_freedom) return Real
   is
   begin
      raise Error with "TODO";

      if DoF /= 1 then
         raise Error with "Illegal degree of freedom:" & Degree_of_freedom'Image (DoF);
      end if;

      return 0.0;
   end Extent;


   overriding
   procedure desired_Extent_is (Self : in out Hinge;   Now : in Real;
                                                       DoF : in Degree_of_freedom)
   is
   begin
      raise Error with "TODO";
   end desired_Extent_is;


   --------
   --- Free
   --

   procedure free (the_Joint : in out physics.Joint.view)
   is
      procedure deallocate is new ada.unchecked_Deallocation (physics.Joint.item'Class,
                                                              physics.Joint.view);
   begin
      deallocate (the_Joint);
   end free;


end box2d_Physics.Joint;
