with
     ada.unchecked_Deallocation;

package body physics.Model
is
   ----------
   --- Forge
   --

   package body Forge
   is
      function new_physics_Model (Id          : in model_Id := null_model_Id;
                                  shape_Info  : in a_Shape;
                                  Scale       : in Vector_3 := (1.0, 1.0, 1.0);
                                  Mass        : in Real     := 0.0;
                                  Friction    : in Real     := 0.1;
                                  Restitution : in Real     := 0.1;
                                  --  Site        : in Vector_3 := Origin_3D;
                                  is_Tangible : in Boolean  := True) return View
      is
      begin
         return new Item' (Id          => Id,
                           Scale       => Scale,
                           shape_Info  => shape_Info,
                           Shape       => null,
                           Mass        => Mass,
                           Friction    => Friction,
                           Restitution => Restitution,
                           --  Site        => Site,
                           is_Tangible => is_Tangible);
      end new_physics_Model;
   end Forge;


   procedure define (Self : in out Item;   Scale : in Vector_3)
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
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class,
                                                              View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;


   ---------------
   --- Attributes
   --

   function Id (Self : in Item'Class) return model_Id
   is
   begin
      return Self.Id;
   end Id;


   procedure Id_is (Self : in out Item'Class;   Now : in model_Id)
   is
   begin
      Self.Id := Now;
   end Id_is;


   procedure Scale_is (Self : in out Item'Class;   Now : in Vector_3)
   is
   begin
      Self.Scale := Now;
   end Scale_is;


end physics.Model;
