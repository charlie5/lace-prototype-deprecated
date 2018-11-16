

package body impact.d3.Space.dynamic
is




   --- Forge
   --


   package body Forge
   is

      procedure define (Self : out Item;    dispatcher             : access impact.d3.Dispatcher.item'Class;
                                            broadphase             : access impact.d3.collision.Broadphase.item'Class;
                        collisionConfiguration : access impact.d3.collision.Configuration.item'Class)
      is
      begin
         impact.d3.Space.Forge.define (impact.d3.Space.item (Self),  dispatcher, broadphase, collisionConfiguration);
         Self.m_solverInfo := impact.d3.contact_solver_Info.to_solver_Info;
      end define;

   end Forge;







   --- Attributes
   --


   procedure addConstraint (Self                                 : in out Item;
                            constraint                           : access impact.d3.Joint.Item'Class;
                            disableCollisionsBetweenLinkedBodies : Boolean                           )
   is
   begin
      null;
   end addConstraint;



   procedure removeConstraint (Self       : in out Item;
                               constraint : access impact.d3.Joint.Item'Class)
   is
   begin
      null;
   end removeConstraint;



   function getNumConstraints (Self : Item) return Integer
   is
      pragma Unreferenced (Self);
   begin
      return 0;
   end getNumConstraints;



   function getConstraint (Self  : Item;
                           index : Integer) return access impact.d3.Joint.Item'Class
   is
      pragma Unreferenced (Self, index);
   begin
      return null;
   end getConstraint;



   procedure getWorldUserInfo (Self : in out Item)
   is
   begin
      null;
   end getWorldUserInfo;



   procedure setInternalTickCallback (Self          : in out Item;
                                      cb            : in     btInternalTickCallback;
                                      worldUserInfo : access lace.Any.item'Class   := null;
                                      isPreTick     : in     Boolean               := False)
   is
   begin
      if isPreTick then
         Self.m_internalPreTickCallback := cb;
      else
         Self.m_internalTickCallback := cb;
      end if;

      Self.m_worldUserInfo := worldUserInfo;
   end setInternalTickCallback;



   procedure setWorldUserInfo (Self          : in out Item;
                               worldUserInfo : access lace.Any.item'Class)
   is
   begin
      Self.m_worldUserInfo := worldUserInfo;
   end setWorldUserInfo;



   function getSolverInfo (Self : access Item) return access impact.d3.contact_solver_Info.item
   is
   begin
      return Self.m_solverInfo'Unchecked_Access;
   end getSolverInfo;




   --- 'Protected'
   --

   function m_internalPreTickCallback (Self : in Item) return btInternalTickCallback
   is
   begin
      return Self.m_internalPreTickCallback;
   end m_internalPreTickCallback;


   function m_internalTickCallback (Self : in Item) return btInternalTickCallback
   is
   begin
      return Self.m_internalTickCallback;
   end m_internalTickCallback;






end impact.d3.Space.dynamic;
