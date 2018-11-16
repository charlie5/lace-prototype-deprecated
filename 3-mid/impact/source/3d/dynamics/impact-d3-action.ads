with
     impact.d3.Space;



package impact.d3.Action is

   type Item is abstract tagged limited
      record
         null;
      end record;


   procedure updateAction (Self           : in Item;
                           collisionWorld : access impact.d3.Space.item'Class;
                           deltaTimeStep  : in     math.Real                  ) is abstract;

   --  procedure destruct (Self : in out Item) is null;

   --  procedure DebugDraw (Self        : in Item;
   --                       debugDrawer : access btDebugDraw.item'Class) is abstract;

end impact.d3.Action;

--  #ifndef _BT_ACTION_INTERFACE_H
--  #define _BT_ACTION_INTERFACE_H

--  class btIDebugDraw;
--  class impact.d3.Space;

--  #include "LinearMath/impact.d3.Scalar.h"
--  #include "impact.d3.Object.rigid.h"

--  ///Basic interface to allow actions such as vehicles and characters to be updated inside a impact.d3.Space.dynamic
--  class impact.d3.Action
--  {
--  protected:

--          static impact.d3.Object.rigid& getFixedBody();


--  public:

--          virtual ~impact.d3.Action()
--          {
--          }

--          virtual void updateAction( impact.d3.Space* collisionWorld, impact.d3.Scalar deltaTimeStep)=0;

--          virtual void debugDraw(btIDebugDraw* debugDrawer) = 0;

--  };

--  #endif //_BT_ACTION_INTERFACE_H
