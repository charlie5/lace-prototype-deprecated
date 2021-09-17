with
     physics.remote.Model,
     openGL .remote_Model,

     lace.Observer,
     lace.Subject,
     lace.Event,

     ada.unchecked_Conversion,
     ada.Containers.indefinite_hashed_Maps,
     ada.Containers.indefinite_Vectors,
     ada.Streams;


package gel.remote.World
--
--  Provides a remote interface of an gel world.
--
--  Supports world mirroring, in which a mirror world mimics the objects and dynamics of a master world.
--
is
   pragma Remote_Types;


   type Item is  limited interface
             and lace.Subject .item
             and lace.Observer.item;

   type View is access all Item'Class;

   pragma Asynchronous (View);



   -----------
   --  Mirrors
   --

   --  Registration
   --

   procedure   register (Self : access Item;   the_Mirror         : in World.view;
                                               Mirror_as_observer : in lace.Observer.view)   is abstract;
   procedure deregister (Self : access Item;   the_Mirror         : in World.view)           is abstract;



   ----------
   --  Models
   --

   --  Graphics
   --
   use type openGL.remote_Model.item;
   package  model_Vectors is new ada.containers.indefinite_Vectors (Positive,  openGL.remote_Model.item'Class);

   function Hash is new ada.unchecked_Conversion (gel.graphics_model_Id,  ada.containers.Hash_type);
   use type gel.graphics_model_Id;

   package  Id_Maps_of_Model_Plan is new ada.containers.indefinite_Hashed_Maps (gel.graphics_model_Id,
                                                                                openGL.remote_Model.item'Class,
                                                                                Hash,
                                                                                "=");
   subtype  graphics_Model_Set is Id_Maps_of_Model_Plan.Map;

   function graphics_Models (Self : in Item) return graphics_Model_Set   is abstract;


   type new_model_Event is new lace.Event.item with
      record
         Model : access openGL.remote_Model.item'Class;
      end record;


   procedure Write (Stream : not null access ada.Streams.Root_Stream_Type'Class;   the_Event : in  new_model_Event);
   procedure Read  (Stream : not null access ada.Streams.Root_Stream_Type'Class;   the_Event : out new_model_Event);

   for new_model_Event'Write use Write;
   for new_model_Event'Read  use Read;


   --  Physics
   --
   use physics.remote.Model;
   package  physics_model_Vectors is new ada.containers.indefinite_Vectors (Positive,  physics.remote.Model.item'Class);

   use type physics.model_Id;
   function Hash is new ada.unchecked_Conversion (physics.model_Id, ada.containers.Hash_type);
   package  Id_Maps_of_physics_Model_Plan is new ada.containers.indefinite_Hashed_Maps (physics.model_Id,
                                                                                        physics.remote.Model.item'Class,
                                                                                        Hash,
                                                                                        "=");
   subtype  physics_Model_Set is Id_Maps_of_physics_Model_Plan.Map;

   function physics_Models (Self : in Item) return physics_Model_Set is abstract;


   type new_physics_model_Event is new lace.Event.item with
      record
         Model : access physics.remote.Model.item'Class;
      end record;


   procedure Write (Stream : not null access ada.Streams.Root_Stream_Type'Class;   the_Event : in  new_physics_model_Event);
   procedure Read  (Stream : not null access ada.Streams.Root_Stream_Type'Class;   the_Event : out new_physics_model_Event);

   for new_physics_model_Event'Write use Write;
   for new_physics_model_Event'Read  use Read;



   -----------
   --  Sprites
   --

   type sprite_model_Pair is
      record
         sprite_Id         : gel.sprite_Id;
         graphics_model_Id : openGL.model_Id;
         physics_model_Id  : physics.model_Id;

         Mass              : math.Real;
         Transform         : math.Matrix_4x4;
         is_Visible        : Boolean;
      end record;

   type sprite_model_Pairs is array (math.Index range <>) of sprite_model_Pair;

   function Sprites (Self : in Item) return sprite_model_Pairs   is abstract;



   -------------------------
   --  Sprite Motion Updates
   --

   --  Coarse types to help minimise network use - (tbd: currently disabled til better quaternion 'coarsen' is ready)
   --
   type coarse_Real is new math.Real;   -- not ccoarse atm (see above 'tbd')

   type coarse_Vector_3 is array (1 .. 3) of coarse_Real;

   function refined (Self : in coarse_Vector_3) return   math.Vector_3;
   function coarsen (Self : in   math.Vector_3) return coarse_Vector_3;


   type coarse_Real2 is new math.Real;   -- not coarse atm


   type coarse_Quaternion is array (1 .. 4) of coarse_Real2;

   function refined (Self : in coarse_Quaternion) return   math.Quaternion;
   function coarsen (Self : in   math.Quaternion) return coarse_Quaternion;



   type motion_Update is
      record
         Id   : gel.sprite_Id;
         Site : coarse_Vector_3;
         Spin : coarse_Quaternion;
      end record;
   pragma Pack (motion_Update);


   type motion_Updates is array (Positive range <>) of motion_Update;
   pragma Pack (motion_Updates);


   procedure motion_Updates_write (Stream : access Ada.Streams.Root_Stream_Type'Class;   Item : in  motion_Updates);
   procedure motion_Updates_read  (Stream : access Ada.Streams.Root_Stream_Type'Class;   Item : out motion_Updates);

   for motion_Updates'Write use motion_Updates_write;
   for motion_Updates'Read  use motion_Updates_read;

   procedure motion_Updates_are (Self : in Item;   Now : in motion_Updates)   is abstract;



   --------------
   --  Test/Debug
   --

   procedure kick_Sprite (Self : in out Item;   sprite_Id : in gel.Sprite_Id) is abstract;

end gel.remote.World;
