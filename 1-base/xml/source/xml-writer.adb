with
     ada.unchecked_Deallocation;


package body XML.Writer
is


   Depth:  Natural;

   procedure Free is new ada.Unchecked_Deallocation (Attributes_t,
                                                     Attributes_view);



   procedure Start_Document (F:  in ada.Text_IO.File_Type)
   is
   begin
      ada.Text_IO.Put_Line (F, "<?xml version=""1.0"" standalone=""yes""?>");
      Depth := 0;
   end Start_Document;



   procedure End_Document   (F:  in ada.Text_IO.File_Type)
   is
   begin
      null;
   end End_Document;



   procedure Start (F:     in ada.Text_IO.File_Type;
                    Name:  in String;
                    Atts:  in Attributes_view)
   is
   begin
      for Pad in 1 .. Depth
      loop
         ada.Text_IO.Put (F, "   ");
      end loop;

      Depth := Depth + 1;
      ada.Text_IO.Put (F, "<" & Name);

      for Att in Atts'Range
      loop
         ada.Text_IO.Put (F, " " & to_String (Atts (Att).Name) & "=""" &
                                   to_String (Atts (Att).Value) & """");
      end loop;

      ada.Text_IO.Put_Line (F, ">");
   end Start;



   procedure Start (F:     in ada.Text_IO.File_Type;
                    Name:  in unbounded_String;
                    Atts:  in Attributes_view)
   is
   begin
      Start (F, to_String (Name), Atts);
   end Start;



   procedure Finish (F:     in ada.Text_IO.File_Type;
                     Name:  in String)
   is
   begin
      Depth := Depth - 1;

      for Pad in 1 .. Depth
      loop
         ada.Text_IO.Put (F, "   ");
      end loop;

      ada.Text_IO.Put_Line (F, "</" & Name & ">");
   end Finish;



   procedure Finish (F:     in ada.Text_IO.File_Type;
                     Name:  in unbounded_String)
   is
   begin
      Finish (F, to_String (Name));
   end Finish;



   procedure Empty (F:     in ada.Text_IO.File_Type;
                    Name:  in String;
                    Atts:  in Attributes_view)
   is
   begin
      for Pad in 1 .. Depth
      loop
         ada.Text_IO.Put (F, "   ");
      end loop;

      ada.Text_IO.Put (F, "<" & Name);

      for Att in Atts'Range
      loop
         ada.Text_IO.Put (F, " " & to_String (Atts (Att).Name) & "=""" &
                                   to_String (Atts (Att).Value) & """");
      end loop;

      ada.Text_IO.Put_Line (F, "/>");
   end Empty;



   procedure Empty (F:     in ada.Text_IO.File_Type;
                    Name:  in unbounded_String;
                    Atts:  in Attributes_view)
   is
   begin
      Empty (F, to_String (Name), Atts);
   end Empty;



   function "+" (K, V:  in String) return Attribute_t
   is
   begin
      return Attribute_t'(to_unbounded_String (K),
                          to_unbounded_String (V));
   end "+";



   function "+" (K, V:  in String) return Attributes_view
   is
   begin
      return new Attributes_t'(1 => Attribute_t'(to_unbounded_String (K),
                                                 to_unbounded_String (V)));
   end "+";



   function "+" (K:  in unbounded_String;
                 V:  in String) return Attribute_t
   is
   begin
      return Attribute_t'(K, to_unbounded_String (V));
   end "+";



   function "+" (K:  in unbounded_String;
                 V:  in String) return Attributes_view
   is
   begin
      return new Attributes_t'(1 => Attribute_t' (K, to_unbounded_String (V)));
   end "+";



   function "+" (K:  in String;
                 V:  in unbounded_String) return Attribute_t
   is
   begin
      return Attribute_t'(to_unbounded_String (K), V);
   end "+";



   function "+" (K:  in String;
                 V:  in unbounded_String) return Attributes_view
   is
   begin
      return new Attributes_t'(1 => Attribute_t'(to_unbounded_String (K), V));
   end "+";



   function MkAtt (L, R:  in Attribute_t) return Attributes_view
   is
   begin
      return new Attributes_t'(L, R);
   end MkAtt;



   function "&" (L, R:  in Attribute_t) return Attributes_view
   is
   begin
      return new Attributes_t'(L, R);
   end "&";



   function "&" (L:  in Attributes_view;   R:  in Attribute_t) return Attributes_view
   is

      Result:  Attributes_view;
      ByeBye:  Attributes_view;

   begin
      Result                 := new Attributes_t (1 .. L'Length + 1);
      Result (1 .. L'Length) := L.all;
      Result (L'Length + 1)  := R;
      ByeBye                 := L;

      Free (ByeBye);
      return Result;
   end "&";


end XML.Writer;
