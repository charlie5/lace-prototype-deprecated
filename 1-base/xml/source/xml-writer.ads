with
     ada.Strings.unbounded,
     ada.Text_IO;


package XML.Writer
is
   use ada.Strings.unbounded;


   procedure Start_Document (F :  in ada.Text_IO.File_Type);
   procedure End_Document   (F :  in ada.Text_IO.File_Type);

   procedure Start (F     : in ada.Text_IO.File_Type;
                    Name  : in String;
                    Atts  : in Attributes_view);

   procedure Start (F     : in ada.Text_IO.File_Type;
                    Name  : in unbounded_String;
                    Atts  : in Attributes_view);

   procedure Finish (F    : in ada.Text_IO.File_Type;
                     Name : in String);

   procedure Finish (F    : in ada.Text_IO.File_Type;
                     Name : in unbounded_String);

   procedure Empty (F     : in ada.Text_IO.File_Type;
                    Name  : in String;
                    Atts  : in Attributes_view);

   procedure Empty (F     : in ada.Text_IO.File_Type;
                    Name  : in unbounded_String;
                    Atts  : in Attributes_view);

   function "+"    (K, V : in String)                                  return Attribute_t;
   function "+"    (K, V : in String)                                  return Attributes_view;
   function "+"    (K    : in unbounded_String;
                    V    : in String)                                  return Attribute_t;
   function "+"    (K    : in unbounded_String;
                    V    : in String)                                  return Attributes_view;
   function "+"    (K    : in String;
                    V    : in unbounded_String)                        return Attribute_t;
   function "+"    (K    : in String;
                    V    : in unbounded_String)                        return Attributes_view;

   function MkAtt  (L, R : in Attribute_t)                             return Attributes_view;
   function "&"    (L, R : in Attribute_t)                             return Attributes_view;
   function "&"    (L    : in Attributes_view;   R:  in Attribute_t)   return Attributes_view;

end XML.Writer;
