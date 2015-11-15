with
     ada.Unchecked_Deallocation;


package body mmi.physics_Model
is

   ----------
   --- Forge
   --

   package body Forge
   is
      function new_physics_Model (Id          : in mmi.physics_model_Id := mmi.null_physics_model_Id;
                                  shape_Info  : in a_Shape;
                                  Scale       : in math.Vector_3        := (1.0, 1.0, 1.0);
                                  Mass        : in math.Real            := 0.0;
                                  Friction    : in math.Real            := 0.1;
                                  Restitution : in math.Real            := 0.1;
                                  is_Tangible : in Boolean              := True) return mmi.physics_Model.view
      is
      begin
         return new Item' (id          => Id,
                           scale       => Scale,
                           shape_Info  => shape_Info,
                           Shape       => null,
                           Mass        => Mass,
                           Friction    => Friction,
                           Restitution => Restitution,
                           is_Tangible => is_Tangible);
      end new_physics_Model;
   end Forge;



   procedure define (Self : in out Item;   Scale : in math.Vector_3)
   is
   begin
      Self.Scale := Scale;
   end define;



   procedure destroy (Self : in out Item)
   is
   begin
      null;
   end destroy;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (physics_Model.item'Class, physics_Model.view);
   begin
      Self.destroy;
      deallocate (Self);
   end free;



   ---------------
   --- Attributes
   --

   function  Id (Self : in Item'Class) return mmi.physics_model_Id
   is
   begin
      return self.Id;
   end Id;


   procedure Id_is (Self : in out Item'Class;   Now : in mmi.physics_model_Id)
   is
   begin
      self.Id := Now;
   end Id_is;



   procedure Scale_is (Self : in out Item'Class;   Now : in math.Vector_3)
   is
   begin
      Self.Scale := Now;
   end Scale_is;


end mmi.physics_Model;
