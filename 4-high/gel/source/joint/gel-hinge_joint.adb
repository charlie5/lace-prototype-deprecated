with
     physics.Object,
     ada.unchecked_Deallocation;

package body gel.hinge_Joint
is
   use gel.Joint;


   procedure define (Self : access Item;  in_Space           : in     std_physics.Space.view;
                                          Sprite_A, Sprite_B : access gel.Sprite.item'Class;
                                          pivot_Axis         : in     Vector_3;
                                          pivot_Anchor       : in     Vector_3)
   is
      pivot_in_A : constant Vector_3 := (pivot_Anchor - Sprite_A.Site);
      pivot_in_B : constant Vector_3 := (pivot_Anchor - Sprite_B.Site);

      the_Axis   : constant Vector_3 := pivot_Axis;

   begin
      Self.define (in_Space,
                   Sprite_A,   Sprite_B,
                   the_Axis,
                   pivot_in_A, pivot_in_B,
                   low_Limit        => to_Radians (-180.0),
                   high_Limit       => to_Radians ( 180.0),
                   collide_Conected => False);
   end define;



   procedure define (Self : access Item;   in_Space           : in     std_physics.Space.view;
                                           Sprite_A, Sprite_B : access gel.Sprite.item'Class;
                                           pivot_Axis         : in     Vector_3)
   is
      Midpoint : constant Vector_3 := (Sprite_A.Site + Sprite_B.Site) / 2.0;
   begin
      Self.define (in_Space,
                   Sprite_A,
                   Sprite_B,
                   pivot_Axis,
                   pivot_anchor => Midpoint);
   end define;



   procedure define (Self : access Item;   in_Space              : in     std_physics.Space.view;
                                           Sprite_A,  Sprite_B   : access gel.Sprite.item'Class;
                                           Frame_A,   Frame_B    : in     Matrix_4x4;
                                           low_Limit             : in     Real := to_Radians (-180.0);
                                           high_Limit            : in     Real := to_Radians ( 180.0);
                                           collide_Conected      : in     Boolean)
   is
      A_Frame : constant Matrix_4x4 := Frame_A;
      B_Frame : constant Matrix_4x4 := Frame_B;

      type Joint_cast is access all gel.Joint.item;

      sprite_A_Solid,
      sprite_B_Solid : std_physics.Object.view;

   begin
      if   Sprite_A = null
        or Sprite_B = null
      then
         raise Error with "Sprite is null.";
      end if;

      sprite_A_Solid := std_physics.Object.view (Sprite_A.Solid);
      sprite_B_Solid := std_physics.Object.view (Sprite_B.Solid);

      joint.define (Joint_cast (Self),  Sprite_A, Sprite_B);   -- Define base class.

      Self.Physics := in_Space.new_hinge_Joint (sprite_A_Solid,  sprite_B_Solid,
                                                A_Frame,         B_Frame,
                                                low_Limit,       high_Limit,
                                                collide_Conected);
   end define;



   procedure define (Self : access Item;   in_Space  : in     std_physics.Space.view;
                                           Sprite_A  : access gel.Sprite.item'Class;
                                           Frame_A   : in     Matrix_4x4)
   is
      type Joint_cast is access all gel.Joint.item;

      A_Frame        : constant Matrix_4x4 := Frame_A;
      sprite_A_Solid : std_physics.Object.view;

   begin
      joint.define (Joint_cast (Self), Sprite_A, null);     -- Define base class.

      sprite_A_Solid := std_physics.Object.view  (Sprite_A.Solid);
      Self.Physics   := in_Space.new_hinge_Joint (sprite_A_Solid,
                                                  A_Frame);
   end define;



   procedure define (Self : access Item;   in_Space         : in     std_physics.Space.view;
                                           Sprite_A,
                                           Sprite_B         : access gel.Sprite.item'Class;
                                           pivot_Axis       : in     Vector_3;
                                           Anchor_in_A,
                                           Anchor_in_B      : in     Vector_3;
                                           low_Limit,
                                           high_Limit       : in     Real;
                                           collide_Conected : in     Boolean)
   is
      type Joint_cast is access all gel.Joint.item;

      sprite_A_Solid,
      sprite_B_Solid : std_physics.Object.view;

   begin
      if   Sprite_A = null
        or Sprite_B = null
      then
         raise Error with "Attempt to join a null sprite.";
      end if;

      sprite_A_Solid := std_physics.Object.view (Sprite_A.Solid);
      sprite_B_Solid := std_physics.Object.view (Sprite_B.Solid);

      Joint.define (Joint_cast (Self), Sprite_A, Sprite_B);     -- Define base class.

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

      procedure deallocate is new ada.unchecked_Deallocation (std_physics.Joint.item'Class,
                                                              std_physics.Joint.view);
   begin
      my_Physics.destruct;
      deallocate (my_Physics);

      Self.Physics := null;
   end destroy;


   --------------
   --- Attributes
   --

   overriding
   function Degrees_of_freedom (Self : in Item) return joint.degree_of_Freedom
   is
      pragma unreferenced (Self);
   begin
      return 1;
   end Degrees_of_freedom;



   function Angle (Self : in Item'Class) return Real
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end Angle;


   overriding
   function Frame_A (Self : in Item) return Matrix_4x4
   is
   begin
      return Self.Physics.Frame_A;
   end Frame_A;


   overriding
   function Frame_B (Self : in Item) return Matrix_4x4
   is
   begin
      return Self.Physics.Frame_B;
   end Frame_B;


   overriding
   procedure Frame_A_is (Self : in out Item;   Now  : in Matrix_4x4)
   is
   begin
      Self.Physics.Frame_A_is (Now);
   end Frame_A_is;


   overriding
   procedure Frame_B_is (Self : in out Item;   Now  : in Matrix_4x4)
   is
   begin
      Self.Physics.Frame_B_is (Now);
   end Frame_B_is;


   overriding
   function Physics (Self : in Item) return joint.Physics_view
   is
   begin
      return Joint.Physics_view (Self.Physics);
   end Physics;


   ----------------
   --- Joint Limits
   --

   procedure Limits_are (Self : in out Item'Class;   Low, High         : in Real;
                                                     Softness          : in Real := 0.9;
                                                     bias_Factor       : in Real := 0.3;
                                                     relaxation_Factor : in Real := 1.0)
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


   --  Bounds - limits the range of motion for a Degree of freedom.
   --

   overriding
   function low_Bound (Self : access Item;   for_Degree : in joint.Degree_of_freedom) return Real
   is
      use type joint.Degree_of_freedom;
   begin
      if for_Degree /= Revolve then
         raise Error with "Invalid degree of freedom:" & for_Degree'Image;
      end if;

      return Self.low_Bound;
   end low_Bound;


   overriding
   procedure low_Bound_is (Self : access Item;   for_Degree : in joint.Degree_of_freedom;
                                                 Now        : in Real)
   is
      use type joint.Degree_of_freedom;
   begin
      if for_Degree /= Revolve then
         raise Error with "Invalid degree of freedom:" & for_Degree'Image;
      end if;

      Self.low_Bound := Now;
      Self.apply_Limits;
   end low_Bound_is;


   overriding
   function high_Bound (Self : access Item;  for_Degree : in joint.Degree_of_freedom) return Real
   is
      use type joint.Degree_of_freedom;
   begin
      if for_Degree /= Revolve then
         raise Error with "Invalid degree of freedom:" & for_Degree'Image;
      end if;

      return Self.high_Bound;
   end high_Bound;


   overriding
   procedure high_Bound_is (Self : access Item;  for_Degree : in joint.Degree_of_freedom;
                                                 Now        : in Real)
   is
      use type joint.Degree_of_freedom;

      Span : Real := abs (Now) * 2.0;
   begin
      if for_Degree /= Revolve then
         raise Error with "Invalid degree of freedom:" & for_Degree'Image;
      end if;

      Self.high_Bound := Now;
      Self.apply_Limits;
   end high_Bound_is;


   overriding
   function Extent (Self : in Item;   for_Degree : in Degree_of_freedom) return Real
   is
      use type joint.Degree_of_freedom;
   begin
      if for_Degree /= Revolve then
         raise Error with "Invalid degree of freedom:" & for_Degree'Image;
      end if;

      return Self.Angle;
   end Extent;


   overriding
   function is_Bound (Self : in Item;   for_Degree : in joint.Degree_of_freedom) return Boolean
   is
   begin
      return Self.Physics.is_Limited (for_Degree);
   end is_Bound;


   overriding
   procedure Velocity_is (Self : in Item;   for_Degree : in joint.Degree_of_freedom;
                                            Now        : in Real)
   is
   begin
      self.Physics.Velocity_is (Now, for_Degree);
   end Velocity_is;


end gel.hinge_Joint;
