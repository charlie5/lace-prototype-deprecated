with
     ada.unchecked_Conversion,
     ada.unchecked_Deallocation,
     interfaces.C.Strings,
     system.Storage_Elements;


package body XML.Reader
is

   package C renames Interfaces.C;
   package S renames Interfaces.C.Strings;


   type XML_Char     is new C.unsigned_short;
   type XML_Char_Ptr is access all XML_Char;
   type Char_Ptr_Ptr is access all S.chars_ptr;




   procedure XML_SetUserData (XML_Parser : in XML_Parser_Ptr;
                              Parser_Ptr : in Parser);

   pragma Import (C, XML_SetUserData, "XML_SetUserData");



   procedure Internal_Start_Handler (My_Parser : in Parser;
                                     Name      : in S.chars_ptr;
                                     AttAdd    : in System.Address);

   pragma Convention (C, Internal_Start_Handler);

   procedure Internal_Start_Handler (My_Parser : in Parser;
                                     Name      : in S.chars_ptr;
                                     AttAdd    : in System.Address)
   is

      use S, System, System.Storage_Elements;

      procedure Free is new ada.Unchecked_Deallocation (Attributes_t,   Attributes_view);
      function To_CP is new ada.unchecked_Conversion   (System.Address, Char_Ptr_Ptr);

      AA_Size             : Storage_Offset;

      the_Attribute_Array : Attributes_view;
      N_Atts              : Natural;
      Atts                : System.Address;

   begin
      -- Calculate the size of a single attribute (name or value) pointer.
      --
      AA_Size := S.Chars_Ptr'Size / System.Storage_Unit;

      -- Count the number of attributes by scanning for a null pointer.
      --
      N_Atts := 0;
      Atts   := AttAdd;

      while To_CP (Atts).all /= S.Null_Ptr
      loop
         N_Atts := N_Atts + 1;
         Atts   := Atts   + (AA_Size * 2);
      end loop;

      -- Allocate a new attribute array of the correct size.
      --
      the_Attribute_Array := new Attributes_t (1 .. N_Atts);

      -- Convert the attribute strings to unbounded_String.
      --
      Atts := AttAdd;

      for Att in 1 .. N_Atts
      loop
         the_Attribute_Array (Att).Name  := to_unbounded_String (S.Value (To_CP (Atts).all));
         Atts                            := Atts + AA_Size;
         the_Attribute_Array (Att).Value := to_unbounded_String (S.Value (To_CP (Atts).all));
         Atts                            := Atts + AA_Size;
      end loop;

      -- Call the user's handler.
      --
      My_Parser.Start_Handler (to_unbounded_String (S.Value (Name)),
                               the_Attribute_Array);

      -- Give back the attribute array.
      --
      Free (the_Attribute_Array);
   end Internal_Start_Handler;




   procedure Internal_End_Handler (My_Parser : in Parser;
                                   Name      : in S.chars_ptr);

   pragma Convention (C, Internal_End_Handler);

   procedure Internal_End_Handler (My_Parser : in Parser;
                                   Name      : in S.chars_ptr)
   is
   begin
      My_Parser.End_Handler (to_unbounded_String (S.Value (Name)));
   end Internal_End_Handler;




   procedure Internal_CD_Handler (My_Parser : in Parser;
                                  Data      : in S.chars_ptr;
                                  Len       : in C.int);

   pragma Convention (C, Internal_CD_Handler);

   procedure Internal_CD_Handler (My_Parser : in Parser;
                                  Data      : in S.chars_ptr;
                                  Len       : in C.int)
   is
      the_Data : constant unbounded_String := to_unbounded_String (S.Value (Data,  c.size_t (Len)));

   begin
      if the_Data /= ""
      then
         My_Parser.CD_Handler (the_Data);
      end if;
   end Internal_CD_Handler;




   function Create_Parser return Parser
   is
      function XML_ParserCreate (Encoding:  in XML_Char_Ptr) return XML_Parser_Ptr;
      pragma Import (C, XML_ParserCreate, "XML_ParserCreate");

   begin
      return new Parser_Rec' (XML_ParserCreate (null),
                              null,
                              null,
                              null);
   end Create_Parser;




   procedure Set_Element_Handler (The_Parser    : in Parser;
                                  Start_Handler : in Start_Element_Handler;
                                  End_Handler   : in End_Element_Handler)
   is
      type Internal_Start_Element_Handler is access procedure (My_Parser : in Parser;
                                                               Name      : in S.chars_ptr;
                                                               AttAdd    : in System.Address);
      pragma Convention (C, Internal_Start_Element_Handler);


      type Internal_End_Element_Handler   is access procedure (My_Parser : in Parser;
                                                               Name      : in S.chars_ptr);
      pragma Convention (C, Internal_End_Element_Handler);


      procedure XML_SetElementHandler (XML_Parser    : in XML_Parser_Ptr;
                                       Start_Handler : in Internal_Start_Element_Handler;
                                       End_Handler   : in Internal_End_Element_Handler);
      pragma Import (C, XML_SetElementHandler, "XML_SetElementHandler");

   begin
      XML_SetUserData (The_Parser.XML_Parser,
                       The_Parser);

      The_Parser.Start_Handler := Start_Handler;
      The_Parser.End_Handler   := End_Handler;

      XML_SetElementHandler (The_Parser.XML_Parser, Internal_Start_Handler'Access,
                                                    Internal_End_Handler  'Access);
   end Set_Element_Handler;




   procedure Set_Character_Data_Handler (The_Parser : in Parser;
                                         CD_Handler : in Character_Data_Handler)
   is

      type Internal_Character_Data_Handler is access procedure  (My_Parser : in Parser;
                                                                 Data      : in S.chars_ptr;
                                                                 Len       : in C.int);
      pragma Convention (C, Internal_Character_Data_Handler);

      procedure XML_SetCharacterDataHandler (XML_Parser : in XML_Parser_Ptr;
                                             CD_Handler : in Internal_Character_Data_Handler);
      pragma Import (C, XML_SetCharacterDataHandler, "XML_SetCharacterDataHandler");

   begin
      XML_SetUserData             (The_Parser.XML_Parser, The_Parser);
      The_Parser.CD_Handler := CD_Handler;
      XML_SetCharacterDataHandler (The_Parser.XML_Parser, Internal_CD_Handler'Access);
   end Set_Character_Data_Handler;




   procedure Parse (The_Parser : in Parser;
                    XML        : in String;
                    Is_Final   : in Boolean)
   is
      function XML_Parse (XML_Parser : in XML_Parser_Ptr;
                          XML        : in S.chars_ptr;
                          Len        : in C.int;
                          Is_Final   : in C.int) return C.int;
      pragma Import (C, XML_Parse, "XML_Parse");

      use C;

      XML_STATUS_ERROR : constant C.int := 0;
      pragma Unreferenced (XML_STATUS_ERROR);
      XML_STATUS_OK    : constant C.int := 1;

      Final_Flag       :          C.int;
      Status           :          C.int;
      XML_Data         :          S.chars_ptr;

   begin
      if Is_Final
      then   Final_Flag := 1;
      else   Final_Flag := 0;
      end if;

      XML_Data := S.New_Char_Array (C.To_C (XML));
      Status   := XML_Parse (The_Parser.XML_Parser,
                             XML_Data,
                             C.int (XML'Length),
                             Final_Flag);
      S.Free (XML_Data);

      if Status /= XML_STATUS_OK
      then
         raise XML_Parse_Error;
      end if;
   end Parse;


end XML.Reader;
