with
     physics.Space,
     physics.Joint,
     physics.Object,

     lace.Observer,
     lace.Any,
     ada.Tags;


package physics.Engine
--
-- Provides a task which evolves a physical space.
--
is
   type Item is tagged limited private;
   type View is access all Item'Class;

--     procedure start (Self : access Item;   space_Kind : in physics.space_Kind);
   procedure start (Self : access Item;   the_Space : in Space.view);
   procedure stop  (Self : access Item);

   procedure add   (Self : access Item;   the_Object : in Object.view);
   procedure rid   (Self : in out Item;   the_Object : in Object.view);

   procedure add   (Self : in out Item;   the_Joint  : in Joint.view);
   procedure rid   (Self : in out Item;   the_Joint  : in Joint.view);

   procedure update_Scale (Self : in out Item;   of_Object : in Object.view;
                                                 To        : in math.Vector_3);

   procedure apply_Force   (Self : in out Item;   to_Object : in Object.view;
                                                  Force     : in math.Vector_3);

   procedure update_Site   (Self : in out Item;   of_Object : in Object.view;
                                                  To        : in math.Vector_3);

   procedure set_Speed     (Self : in out Item;   of_Object : in Object.view;
                                                  To        : in math.Vector_3);

   procedure set_Gravity   (Self : in out Item;   To        : in math.Vector_3);

   procedure set_xy_Spin   (Self : in out Item;   of_Object : in Object.view;
                                                  To        : in math.Radians);

   procedure update_Bounds (Self : in out Item;   of_Object : in Object.view);

   procedure set_local_Anchor (Self : in out Item;   for_Joint   : in Joint.view;
                                                     To          : in math.Vector_3;
                                                     is_Anchor_A : in Boolean);


private

   task
   type Evolver (Self : access Engine.item'Class)
   is
--        entry start (space_Kind : in physics.space_Kind);
      entry start (the_Space : in Space.view);
      entry stop;

      entry reset_Age;

      pragma Storage_Size (20_000_000);
   end Evolver;


   --  Engine Commands
   --
   type Any_limited_view is access all lace.Any.limited_item'Class;

   type command_Kind is (add_Object,             rid_Object,
                         scale_Object,           destroy_Object,
                         update_Bounds,          update_Site,
                         set_Speed,              apply_Force,
                         set_xy_Spin,
                         add_Joint,              rid_Joint,
                         set_Joint_local_Anchor,
                         free_Joint,
                         cast_Ray,
--                           new_impact_Response,
                         set_Gravity);

   type Command (Kind : command_Kind := command_Kind'First) is
      record
         Object : physics.Object.view;

         case Kind
         is
            when add_Object =>
               add_Children : Boolean;
--                 Model        : physics.Model.view;

            when rid_Object =>
               rid_Children : Boolean;

            when update_Site =>
               Site    : math.Vector_3;

            when scale_Object =>
               Scale   : math.Vector_3;

            when apply_Force =>
               Force   : math.Vector_3;

            when set_Speed =>
               Speed   : math.Vector_3;

            when set_Gravity =>
               Gravity : math.Vector_3;

            when set_xy_Spin =>
               xy_Spin : math.Radians;

            when add_Joint | rid_Joint | free_Joint =>
               Joint   : physics.Joint.view;

            when set_Joint_local_Anchor =>
               anchor_Joint : physics.Joint.view;
               is_Anchor_A  : Boolean;         -- When false, is anchor B.
               local_Anchor : math.Vector_3;

            when cast_Ray =>
               From, To   : math.Vector_3;
               Observer   : lace.Observer.view;
               Context    : Any_limited_view;
               event_Kind : ada.Tags.Tag;

--              when new_impact_Response =>
--                 Filter   : impact_Filter;
--                 Response : impact_Response;

            when others =>
               null;
         end case;
      end record;

   type Commands is array (Positive range 1 .. 200_000) of Command;


   protected
   type safe_command_Set
   is
      function  is_Empty return Boolean;

      procedure add   (the_Command : in     Command);
      procedure Fetch (To          :    out Commands;
                       Count       :    out Natural);
   private
      Set       : Commands;
      the_Count : Natural := 0;
   end safe_command_Set;

   type safe_command_Set_view is access all safe_command_Set;


   type Item is tagged limited
      record
         Age      : Duration := 0.0;

         Space    : physics.Space.view;
         Commands : safe_command_Set_view := new safe_command_Set;
         Evolver  : engine.Evolver (Item'Access);
      end record;


end physics.Engine;
