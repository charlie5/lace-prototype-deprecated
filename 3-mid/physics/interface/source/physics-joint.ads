with
     physics.Object,
     lace.Any;

package physics.Joint
--
-- Provides an interface for physics joints.
--
is
   type Item is limited interface
                and lace.Any.limited_item;

   type View is access all Item'Class;


   type Degree_of_freedom is range 1 .. 6;

   procedure destruct    (Self : in out Item)                            is abstract;

   function  Object_A    (Self : in     Item) return physics.Object.view is abstract;
   function  Object_B    (Self : in     Item) return physics.Object.view is abstract;


   function  Frame_A     (Self : in     Item) return Matrix_4x4          is abstract;
   function  Frame_B     (Self : in     Item) return Matrix_4x4          is abstract;

   procedure Frame_A_is  (Self : in out Item;   Now : in Matrix_4x4)     is abstract;
   procedure Frame_B_is  (Self : in out Item;   Now : in Matrix_4x4)     is abstract;


   function  is_Limited  (Self : in     Item;   DoF : Degree_of_freedom) return Boolean is abstract;


   procedure Velocity_is (Self : in out Item;   Now : in Real;
                                                DoF : in Degree_of_freedom)             is abstract;
   --
   --  Sets the spatial or angular velocity for the specified DoF.


   function  Extent      (Self : in     Item;   DoF : Degree_of_freedom) return Real    is abstract;
   --
   --  Returns the current distance or angle (for a spatial or angular DoF, respectively).


   procedure desired_Extent_is (Self : in out Item;   Now : in Real;
                                                      DoF : in Degree_of_freedom)       is abstract;
   --
   --  Sets the desired spacial or angular extent for a given degree of freedom (DoF).


   function  reaction_Force  (Self : in     Item) return Vector_3 is abstract;
   function  reaction_Torque (Self : in     Item) return Real     is abstract;


   procedure user_Data_is    (Self : in out Item;   Now : access lace.Any.limited_item'Class) is abstract;
   function  user_Data       (Self : in     Item)  return access lace.Any.limited_item'Class  is abstract;


end physics.Joint;
