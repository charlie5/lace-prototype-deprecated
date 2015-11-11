with
     ada.Strings.unbounded;


package XML.Reader
is
   use ada.Strings.unbounded;


   type Parser is private;

   function Create_Parser return Parser;



   type Start_Element_Handler is access procedure (Name : in     unbounded_String;
                                                   Atts : in XML.Attributes_view);
   type End_Element_Handler   is access procedure (Name : in     unbounded_String);


   procedure Set_Element_Handler (The_Parser    : in Parser;
                                  Start_Handler : in Start_Element_Handler;
                                  End_Handler   : in End_Element_Handler);



   type Character_Data_Handler is access procedure (Data:  in unbounded_String);

   procedure Set_Character_Data_Handler (The_Parser :  in Parser;
                                         CD_Handler :  in Character_Data_Handler);



   procedure Parse (The_Parser : in Parser;
                    XML        : in String;
                    Is_Final   : in Boolean);

   XML_Parse_Error : exception;





private

   type XML_Parser_Ptr is access all Character;  -- Essentially, C's "void *".

   type Parser_Rec is
      record
         XML_Parser    : XML_Parser_Ptr;
         Start_Handler : Start_Element_Handler;
         End_Handler   : End_Element_Handler;
         CD_Handler    : Character_Data_Handler;
   end record;

   type Parser is access Parser_Rec;

--   pragma Linker_Options ("-lexpat");

end XML.Reader;
