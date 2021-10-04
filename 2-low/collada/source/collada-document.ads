with
     collada.Asset,
     collada.Libraries;

package collada.Document
--
-- Models a colada document.
--
is
   type Item is tagged private;

   function to_Document (Filename : in String) return Item;


   function Asset     (Self : in Item) return collada.Asset    .item;
   function Libraries (Self : in Item) return collada.Libraries.item;



private

   type Item is tagged
      record
         Asset     : collada.Asset    .item;
         Libraries : collada.Libraries.item;
      end record;

end collada.Document;
