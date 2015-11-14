
--  with i.physics.Object;
--  with i.physics.Joint;

with ada.strings.unbounded;



package physics.Motor is


   type Item is abstract tagged
      record
         Name       : ada.strings.unbounded.unbounded_String;
         is_Enabled : Boolean                               := False;
      end record;



   procedure update (Self : in out Item) is abstract;


--  	class Motor
--  	{
--  	public:
--
--  		/// Returns true if this Motor depends on the given Solid.
--  		virtual bool internal_dependsOnSolid(Solid* s);
--
--  		/// Returns true if this Motor depends on the given Joint.
--  		virtual bool internal_dependsOnJoint(Joint* j);
--  }
--
--  #endif

   procedure dummy;


end physics.Motor;


