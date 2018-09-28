package physics.Shape
--
-- Models a physical shape.
--
is
   type Item is limited interface;
   type View is access all Item'Class;

   procedure define (Self : in out Item) is abstract;

   procedure free     (Self : in out View);

   procedure Scale_is (Self : in out Item;   Now : in math.Vector_3) is abstract;
   procedure evolve   (Self : in out Item;   By  : in Duration)      is null;
   procedure destruct (Self : in out Item)                           is abstract;

end physics.Shape;
