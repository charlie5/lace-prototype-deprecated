with
     openGL.Impostor,
     openGL.Visual,
     openGL.Renderer.lean;

limited
with
     openGL.Camera;

private
with
     ada.containers.hashed_Maps,
     ada.unchecked_Conversion;


package openGL.Impostorer
--
-- Provides an impostoring engine.
--
is

   type Item is tagged limited private;
   type View is access all Item'Class;


   ---------
   --- Forge
   --

   procedure define   (Self : in out Item);
   procedure destruct (Self : in out Item);


   --------------
   --- Attributes
   --

   function  impostor_Count       (Self : in     Item) return Natural;


   function  impostor_size_Min    (Self : in     Item'Class)     return Real;
   procedure impostor_size_Min_is (Self : in out Item'Class;   Now : in Real);
   --
   -- Visuals whose projected size falls below this minimum will be substituted with impostors.


   function  Camera               (Self : in     Item'Class)  return access openGL.Camera.item'Class;
   procedure Camera_is            (Self : in out Item'Class;   Now : access openGL.Camera.item'Class);

   function  Renderer             (Self : in     Item'Class)     return openGL.Renderer.lean.view;
   procedure Renderer_is          (Self : in out Item'Class;   Now : in openGL.Renderer.lean.view);


   --------------
   --  Operations
   --

   procedure substitute (Self : in out Item;   the_Visuals : in out openGL.Visual.views;
                                               Camera      : access openGL.Camera.item'Class);




private

   -- visual_Maps_of_impostor
   --

   use type Visual  .view,
            Impostor.view;

   function Hash is new ada.unchecked_Conversion (Visual.view, ada.containers.Hash_type);

   package visual_Maps_of_impostor is new ada.containers.hashed_Maps (Visual.view,
                                                                      openGL.Impostor.view,
                                                                      hash            => Hash,
                                                                      equivalent_keys => "=");
   use visual_Maps_of_impostor;



   -- impostor_load_Balancer
   --

   package impostor_load_Balancer
   is

      type Slot is
         record
            max_Faces     : Positive;
            max_Updates   : Positive;

            Impostors       : Impostor.views (1 .. 20_000);
            impostors_Count : Natural                     := 0;
         end record;

      type Slots      is array (Positive range <>) of Slot;
      type Slots_view is access all Slots;

   end impostor_load_Balancer;


   default_Slots : aliased impostor_load_Balancer.Slots := (1 => (max_Faces =>  100,           max_Updates =>  1*20,  others => <>),
                                                            2 => (max_Faces => 1000,           max_Updates =>  15,  others => <>),
                                                            3 => (max_Faces => Positive'Last,  max_Updates =>  12,  others => <>));
   -- Impostorer
   --

   type Item is tagged limited
      record
         impostor_size_Min      :         safe_Real;

         visual_Map_of_imposter :         visual_Maps_of_impostor.Map;
         impostor_load_Slots    :         impostor_load_Balancer.Slots_view := new impostor_load_Balancer.Slots' (default_Slots);

         Camera                 : access  openGL.Camera.item'Class;
         Renderer               :         openGL.Renderer.lean.view;
      end record;


end openGL.Impostorer;
