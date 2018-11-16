with impact.d3.Object,
     impact.d3.Manifold,
     impact.d3.Joint,
     impact.d3.contact_solver_Info,
     impact.d3.Dispatcher;


package impact.d3.constraint_Solver
--
--  impact.d3.constraint_Solver provides solver interface.
--
is


   type Item is abstract tagged private;
   type View is access all Item'Class;


   procedure destruct (Self : in out Item)   is null;



   procedure prepareSolve (Self : in out Item;   numBodies    : in Integer;
                                                 numManifolds : in Integer)
   is null;



   function  solveGroup (Self : access Item;   bodies      : access impact.d3.Object.Vector;
                                               manifold    : access impact.d3.Manifold.Vector;
                                               constraints : access impact.d3.Joint.Vector;
                                               info        : in impact.d3.contact_solver_Info.Item'Class;
                                               dispatcher  : in impact.d3.Dispatcher.item'Class) return math.Real
   is abstract;
   --
   --  Solve a group of constraints.



   procedure allSolved (Self : in out Item;   info : in impact.d3.contact_solver_Info.Item'Class)
   is null;



   procedure reset (Self : in out Item)
   is abstract;
   --
   --  Clear internal cached data and reset random seed.






private

   type Item is abstract tagged null record;


end impact.d3.constraint_Solver;
