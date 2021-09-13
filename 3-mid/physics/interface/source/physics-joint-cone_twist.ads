package physics.Joint.cone_Twist
--
-- An interface to a cone-twist joint.
--
is
   type Item is  limited interface
             and Joint.item;

   type View is access all Item'Class;


   function  lower_Limit    (Self : in     Item;   DoF : in Degree_of_freedom) return Real    is abstract;
   function  upper_Limit    (Self : in     Item;   DoF : in Degree_of_freedom) return Real    is abstract;


   procedure lower_Limit_is (Self : in out Item;   Now : in Real;
                                                   DoF : in Degree_of_freedom)   is abstract;

   procedure upper_Limit_is (Self : in out Item;   Now : in Real;
                                                   DoF : in Degree_of_freedom)   is abstract;

end physics.Joint.cone_Twist;
