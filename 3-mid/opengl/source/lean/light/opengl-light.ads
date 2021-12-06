--  with
--       ada.Containers.indefinite_hashed_Maps;


package openGL.Light
--
--  Models a light.
--
is
   type Item is abstract tagged private;

   --  type Id is range 1 .. 10;


   --------------
   --- Attributes
   --
   function  is_On   (Self : in     Item)     return Boolean;
   procedure is_On   (Self : in out Item;   Now : in Boolean := True);

   function  Site    (Self : in     Item)     return openGL.Site;
   procedure Site_is (Self : in out Item;   Now : in openGL.Site);


   --------------
   --- Containers
   --

   --  function Hash (Id : in Light.Id) return ada.Containers.Hash_type;
   --  package  id_Maps_of_light is new ada.Containers.indefinite_hashed_Maps (Key_Type        => Id,
   --                                                                          Element_Type    => Item'Class);
                                                                          --  Hash            => ,
                                                                          --  Equivalent_Keys => ,
                                                                          --  "="             => )

private

   type Item is abstract tagged
      record
         On   : Boolean     := True;
         Site : openGL.Site := (0.0, 0.0, 1.0);     -- The GL default.
      end record;

end openGL.Light;
