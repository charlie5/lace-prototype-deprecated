with
     physics.Object,
     ada.Unchecked_Deallocation;


package body mmi.hinge_Joint
is
   use mmi.Joint,
       Math;


   package std_Physics renames Standard.Physics;



   procedure define (Self : access Item;  in_Space           : in     Standard.physics.Space.view;
                                          Sprite_A, Sprite_B : access mmi.Sprite.item'class;
                                          pivot_Axis         : in     math.Vector_3;
                                          pivot_Anchor       : in     math.Vector_3)
   is
      pivot_in_A : aliased constant Vector_3 := (pivot_Anchor - Sprite_A.Site);
      pivot_in_B : aliased constant Vector_3 := (pivot_Anchor - Sprite_B.Site);

      the_Axis   : aliased constant Vector_3 := pivot_Axis;

   begin
      Self.define (in_Space,
                   Sprite_A,   Sprite_B,
                   the_Axis,
                   pivot_in_A, pivot_in_B,
                   low_Limit               => to_Radians (-180.0),
                   high_Limit              => to_Radians ( 180.0),
                   collide_Conected        => False);
   end define;



   procedure define (Self : access Item;   in_Space           : in     Standard.physics.Space.view;
                                           Sprite_A, Sprite_B : access mmi.Sprite.item'class;
                                           pivot_Axis         : in     math.Vector_3)
   is
      Midpoint : constant math.Vector_3 := (Sprite_A.Site + Sprite_B.Site) / 2.0;
   begin
      define (Self,  in_Space,  Sprite_A, Sprite_B,  pivot_Axis,  pivot_anchor => Midpoint);
   end define;



   procedure define (Self : access Item;   in_Space              : in     Standard.physics.Space.view;
                                           Sprite_A,  Sprite_B   : access mmi.Sprite.item'class;
                                           Frame_A,   Frame_B    : in     math.Matrix_4x4;
                                           low_Limit             : in     math.Real := math.to_Radians (-180.0);
                                           high_Limit            : in     math.Real := math.to_Radians ( 180.0);
                                           collide_Conected      : in     Boolean)
   is
      the_Frame_A : aliased constant Matrix_4x4 := Frame_A;
      the_Frame_B : aliased constant Matrix_4x4 := Frame_B;

      type joint_Cast is access all mmi.Joint.Item;

      sprite_A_Solid,
      sprite_B_Solid : standard.physics.Object.view;

   begin
      if   Sprite_A = null
        or Sprite_B = null
      then
         raise Program_Error;
      end if;

      sprite_A_Solid := standard.physics.Object.view (Sprite_A.Solid);
      sprite_B_Solid := standard.physics.Object.view (Sprite_B.Solid);

      joint.define (joint_Cast (Self),   Sprite_A, Sprite_B);   -- Define base class.

      Self.Physics := in_Space.new_hinge_Joint (sprite_A_Solid,  sprite_B_Solid,
                                                the_Frame_A,     the_Frame_B,
                                                low_Limit,       high_Limit,
                                                collide_Conected);
   end define;




   procedure define (Self : access Item;   in_Space  : in     Standard.physics.Space.view;
                                           Sprite_A  : access mmi.Sprite.item'class;
                                           Frame_A   : in     math.Matrix_4x4)
   is
      type joint_Cast is access all mmi.Joint.Item;

      the_Frame_A    : aliased constant Matrix_4x4 := Frame_A;
      sprite_A_Solid : standard.physics.Object.view;

   begin
      joint.define (joint_Cast (Self),   Sprite_A, null);     -- Define base class.

      sprite_A_Solid := standard.physics.Object.view (Sprite_A.Solid);
      Self.Physics   := in_Space.new_hinge_Joint     (sprite_A_Solid,
                                                      the_Frame_A);
   end define;




   procedure define (Self : access Item;   in_Space         : in     Standard.physics.Space.view;
                                           Sprite_A,
                                           Sprite_B         : access mmi.Sprite.item'class;
                                           pivot_Axis       : in     math.Vector_3;
                                           Anchor_in_A      : in     math.Vector_3;
                                           Anchor_in_B      : in     math.Vector_3;
                                           low_Limit,
                                           high_Limit       : in     Real;
                                           collide_Conected : in     Boolean)
   is
      type joint_Cast is access all mmi.Joint.Item;

      sprite_A_Solid,
      sprite_B_Solid : standard.physics.Object.view;

   begin
      if   Sprite_A = null
        or Sprite_B = null
      then
         raise Program_Error;
      end if;

      sprite_A_Solid := standard.physics.Object.view (Sprite_A.Solid);
      sprite_B_Solid := standard.physics.Object.view (Sprite_B.Solid);

      joint.define (joint_Cast (Self),   Sprite_A, Sprite_B);     -- Define base class.

      Self.Physics := in_Space.new_hinge_Joint  (sprite_A_Solid,  sprite_B_Solid,
                                                 Anchor_in_A,     Anchor_in_B,
                                                 pivot_Axis,
                                                 low_Limit,       high_Limit,
                                                 collide_Conected);
   end define;



   overriding
   procedure destroy (Self : in out Item)
   is
      my_Physics : std_physics.Joint.view := std_physics.Joint.view (Self.Physics);

      procedure deallocate is new ada.Unchecked_Deallocation (std_physics.Joint.item'Class,
                                                              std_physics.Joint.view);
   begin
      my_Physics.destruct;
      deallocate (my_Physics);

      Self.Physics := null;
   end destroy;





   --- Attributes
   --

   overriding
   function Degrees_of_freedom (Self : in Item) return joint.degree_of_Freedom
   is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Degrees_of_freedom;




   function  Angle (Self : in     Item'Class) return math.Real
   is
   begin
      raise Constraint_Error with "TBD";
      return 0.0;
   end Angle;



   overriding
   function  Frame_A    (Self : in     Item) return math.Matrix_4x4
   is
   begin
      return Self.Physics.Frame_A;
   end Frame_A;



   overriding
   function  Frame_B    (Self : in     Item) return math.Matrix_4x4
   is
   begin
      return Self.Physics.Frame_B;
   end Frame_B;



   overriding
   procedure Frame_A_is (Self : in out Item;   Now  : in math.Matrix_4x4)
   is
   begin
      Self.Physics.Frame_A_is (Now);
   end Frame_A_is;


   overriding
   procedure Frame_B_is (Self : in out Item;   Now  : in math.Matrix_4x4)
   is
   begin
      Self.Physics.Frame_B_is (Now);
   end Frame_B_is;



   overriding
   function Physics (Self : in     Item) return Joint.Physics_view
   is
   begin
      return Joint.Physics_view (Self.Physics);
   end Physics;




   --- Joint Limits
   --

   procedure Limits_are (Self : in out Item'Class;   Low, High         : in math.Real;
                                                     Softness          : in math.Real := 0.9;
                                                     bias_Factor       : in math.Real := 0.3;
                                                     relaxation_Factor : in math.Real := 1.0)
   is
   begin
      Self.low_Bound         := Low;
      Self.high_Bound        := High;
      Self.Softness          := Softness;
      Self.bias_Factor       := bias_Factor;
      Self.relaxation_Factor := relaxation_Factor;
   end Limits_are;


   procedure apply_Limits (Self : in out Item)
   is
   begin
      Self.Physics.Limits_are (Self.low_Bound,
                               Self.high_Bound,
                               Self.Softness,
                               Self.bias_Factor,
                               Self.relaxation_Factor);
   end apply_Limits;




   ---  Bounds - limits the range of motion for a Degree of freedom.
   --

   overriding
   function  low_Bound    (Self       : access Item;   for_Degree : in     joint.Degree_of_freedom) return math.Real
   is
      use type mmi.Joint.Degree_of_freedom;
   begin
      if for_Degree /= Revolve then
         raise constraint_Error with "invalid Degree of freedom: " & joint.Degree_of_freedom'Image (for_Degree);
      end if;

      return self.low_Bound;
   end low_Bound;



   overriding
   procedure low_Bound_is (Self       : access Item;   for_Degree : in     joint.Degree_of_freedom;
                                                       Now        : in     math.Real)
   is
      use type mmi.Joint.Degree_of_freedom;
   begin
      if for_Degree /= Revolve then
         raise constraint_Error with "invalid Degree of freedom: " & joint.Degree_of_freedom'Image (for_Degree);
      end if;

      self.low_Bound := Now;
      self.apply_Limits;
   end low_Bound_is;



   overriding
   function  high_Bound    (Self       : access Item;  for_Degree : in     joint.Degree_of_freedom) return math.Real
   is
      use type mmi.Joint.Degree_of_freedom;
   begin
      if for_Degree /= Revolve then
         raise constraint_Error with "invalid Degree of freedom: " & joint.Degree_of_freedom'Image (for_Degree);
      end if;

      return self.high_Bound;
   end high_Bound;



   overriding
   procedure high_Bound_is (Self       : access Item;  for_Degree : in     joint.Degree_of_freedom;
                                                       Now        : in     math.Real)
   is
      use type mmi.Joint.Degree_of_freedom;

      Span : math.Real := abs (Now) * 2.0;
   begin
      if for_Degree /= Revolve then
         raise constraint_Error with "invalid Degree of freedom: " & joint.Degree_of_freedom'Image (for_Degree);
      end if;

      self.high_Bound := Now;
      self.apply_Limits;
   end high_Bound_is;



   overriding
   function Extent (Self : in     Item;   for_Degree : in     Degree_of_freedom) return math.Real
   is
      use type mmi.Joint.Degree_of_freedom;
   begin
      if for_Degree /= Revolve then
         raise constraint_Error with "invalid Degree of freedom: " & joint.Degree_of_freedom'Image (for_Degree);
      end if;

      return self.Angle;
   end Extent;



   overriding
   function is_Bound (Self : in     Item;   for_Degree : in joint.Degree_of_freedom) return Boolean
   is
   begin
      return self.Physics.is_Limited (for_Degree);
   end is_Bound;



   overriding
   procedure Velocity_is   (Self : in     Item;   for_Degree : in     joint.Degree_of_freedom;
                                                  Now        : in     math.Real)
   is
   begin
      self.Physics.Velocity_is (Now, for_Degree);
   end Velocity_is;


end mmi.hinge_Joint;
