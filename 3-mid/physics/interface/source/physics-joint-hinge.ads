package physics.Joint.hinge
--
-- An interface to a hinge joint.
--
is
   type Item is  limited interface
             and Joint.item;

   type View is access all Item'Class;



   procedure Limits_are  (Self : in out Item;   Low, High        : in Real;
                                                Softness         : in Real := 0.9;
                                                biasFactor       : in Real := 0.3;
                                                relaxationFactor : in Real := 1.0)   is abstract;

   function  lower_Limit (Self : in     Item)   return Real                          is abstract;
   function  upper_Limit (Self : in     Item)   return Real                          is abstract;

   function  Angle       (Self : in     Item)   return Real                          is abstract;

end physics.Joint.hinge;
