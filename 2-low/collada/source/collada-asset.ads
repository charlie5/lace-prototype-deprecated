with
     ada.calendar;


package collada.Asset
--
-- Models a collada Asset.
--
is

   type Contributor is
      record
         Author         : Text;
         authoring_Tool : Text;
      end record;


   type Unit is
      record
         Name  : Text;
         Meter : Float;
      end record;


   type up_Direction is (X_up, Y_up, Z_up);



   type Item is
      record
         Contributor : asset.Contributor;
         Created     : ada.calendar.Time;
         Modified    : ada.calendar.Time;
         Unit        : asset.Unit;
         up_Axis     : up_Direction;
      end record;

end collada.Asset;
