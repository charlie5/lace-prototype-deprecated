with
     xml.Reader,
     ada.Text_IO;


package body XML
is

   ------------------
   --- Attribute type
   --

   function Name (Self : in Attribute_t) return String
   is
   begin
      return to_String (Self.Name);
   end Name;



   function Value (Self : in Attribute_t) return String
   is
   begin
      return to_String (Self.Value);
   end Value;




   ----------------
   --- Element type
   --

   function to_XML (Filename : in String) return Element
   is
      use xml.Reader,   xml.element_Vectors,
          ada.Text_IO;

      the_Root      : aliased  Element;

      Line_Max      : constant                  := 800_000;

      Depth         :          Natural          := 0;
      the_XML_File  :          File_Type;
      the_Parser    :          xml.reader.Parser;
      Done          :          Boolean;
      Buffer        :          String (1 .. Line_Max);
      Buffer_Length :          Natural;


      element_Stack : element_Vector;

      function current_Element return Element_view
      is
      begin
         return element_Stack.last_Element;
      end current_Element;


      procedure Starter (Name:  in unbounded_String;
                         Atts:  in Attributes_view)
      is
         new_Element : constant Element_view := new Element' (name       => Name,
                                                              attributes => new Attributes_t' (Atts.all),
                                                              data       => <>,
                                                              parent     => current_Element,
                                                              children   => <>);
      begin
         current_Element.add_Child (new_Element);
         element_Stack  .append    (new_Element);
      end Starter;


      procedure Ender (Name:  in unbounded_String)
      is
         pragma Unreferenced (Name);
      begin
         element_Stack.delete_Last;
      end Ender;


      procedure data_Handler (Data:  in unbounded_String)
      is
      begin
         append (current_Element.Data,  "" & Data);
      end data_Handler;


   begin
      append (element_Stack,  the_Root'unchecked_Access);

      open (the_XML_File, In_File, Filename);

      the_Parser := Create_Parser;
      set_Element_Handler        (the_Parser, Starter     'unrestricted_Access,
                                              Ender       'unrestricted_Access);
      set_Character_Data_Handler (the_Parser, data_Handler'unrestricted_Access);

      loop
         Get_Line (the_XML_File, Buffer, Buffer_Length);

         Done := End_Of_File (the_XML_File);

         Parse (the_Parser,  Buffer (1 .. Buffer_Length),  Done);
         exit when Done;
      end loop;

      close (the_XML_File);

      return the_Root;
   end to_XML;



   function Name (Self : in Element) return String
   is
   begin
      return to_String (Self.Name);
   end Name;




   function Data (Self : in Element) return String
   is
   begin
      return to_String (Self.Data);
   end Data;



   function Children (Self : in Element) return Elements
   is
      the_Children : Elements (1 .. Integer (Self.children.Length));

   begin
      for Each in the_Children'Range
      loop
         the_Children (Each) := Self.Children.Element (Each);
      end loop;

      return the_Children;
   end Children;



   function Children (Self : in Element;   Named : in String) return Elements
   is
      the_Children : Elements (1 .. Integer (Self.children.Length));
      Count        : Natural := 0;

   begin
      for Each in the_Children'Range
      loop
         if Self.Children.Element (Each).Name = Named
         then
            Count                := Count + 1;
            the_Children (Count) := Self.Children.Element (Each);
         end if;
      end loop;

      return the_Children (1 .. Count);
   end Children;



   procedure add_Child (Self : in out Element;   the_Child : access Element)
   is
   begin
      Self.Children.append (the_Child.all'Access);
   end add_Child;



   function Child (Self : in Element;   Named : in String) return access Element
   is
      use element_Vectors;
      Cursor : element_Vectors.Cursor := Self.children.First;

   begin
      while has_Element (Cursor)
      loop
         if element_Vectors.Element (Cursor).Name = Named
         then
            return element_Vectors.Element (Cursor);
         end if;

         next (Cursor);
      end loop;

      return null;
   end Child;



   function  Attributes (Self : in Element) return Attributes_t
   is
   begin
      return Self.Attributes.all;
   end Attributes;



   function  Attribute (Self : in Element;   Named : in String) return access Attribute_t'Class
   is
   begin
      for Each in Self.Attributes'Range
      loop
         if Self.Attributes (Each).Name = Named
         then
            return Self.Attributes (Each)'Access;
         end if;
      end loop;

      return null;
   end Attribute;



   function  Parent (Self : in Element) return access Element
   is
   begin
      return Self.Parent;
   end Parent;


end XML;
