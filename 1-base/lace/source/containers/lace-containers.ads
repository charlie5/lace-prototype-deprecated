with
     ada.Containers;


package lace.Containers
is
   pragma Pure;

   subtype Hash_Type  is ada.Containers.Hash_type;
   subtype Count_Type is ada.Containers.Count_type;

end lace.Containers;
