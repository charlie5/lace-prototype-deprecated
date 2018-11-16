with impact.d3.Joint;
with impact.d3.Manifold;
with impact.d3.Object.rigid;
with impact.d3.Object;
with impact.d3.contact_solver_Info;


--  #include "LinearMath/impact.d3.Vector.h"
--  #include "impact.d3.jacobian_Entry.h"
--  #include "impact.d3.Joint.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.Manifold.h"


package impact.d3.Joint.contact
--
--  impact.d3.Joint.contact can be automatically created to solve contact constraints using the unified impact.d3.Joint interface.
--
is

   type Item is new impact.d3.Joint.item with private;


   --- Forge
   --

   function to_contact_Joint (contactManifold : access impact.d3.Manifold.item;
                              rbA, rbB        : access impact.d3.Object.rigid         .item'Class) return Item;

   overriding procedure destruct (Self : in out Item);




   --- Attributes
   --

   overriding procedure setParam (Self :    out Item;   num   : in impact.d3.Joint.btConstraintParams;
                                             value : in math.Real;
                                             axis  : in Integer := -1);

   overriding function  getParam (Self : in     Item;   num   : in impact.d3.Joint.btConstraintParams;
                                             axis  : in Integer := -1) return math.Real;


   overriding procedure getInfo1 (Self : in out Item;   info : out impact.d3.Joint.btConstraintInfo1);
   overriding procedure getInfo2 (Self : in out Item;   info : out impact.d3.Joint.btConstraintInfo2);




   procedure setContactManifold (Self : in out Item;   contactManifold : access impact.d3.Manifold.item);


   function getContactManifold (Self : access Item) return access impact.d3.Manifold.Item'Class;




   --- Utility
   --

   function resolveSingleCollision (body1                : access impact.d3.Object.rigid.item'Class;
                                    colObj2              : access impact.d3.Object.item'Class;
                                    contactPositionWorld : in     math.Vector_3;
                                    contactNormalOnB     : in     math.Vector_3;
                                    solverInfo           : in     impact.d3.contact_solver_Info.item'Class;
                                    distance             : in     math.Real                   ) return math.Real;
   --
   --  Very basic collision resolution without friction.






private


   type Item is new impact.d3.Joint.item with
      record
         m_contactManifold : aliased impact.d3.Manifold.item;
      end record;


end impact.d3.Joint.contact;







