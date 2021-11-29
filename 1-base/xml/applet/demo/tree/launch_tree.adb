with
     XML,
     ada.Text_IO;


procedure launch_Tree
--
-- Loads an xml file, parses it into a tree and displays the tree.
--
is
   the_Tree : constant xml.Element := xml.to_XML ("./box.dae");
   Depth    :          Natural     := 0;


   procedure show_Element (the_Element : in xml.Element)
   is
      use ada.Text_IO;

      the_Children : constant xml.Elements := the_Element.Children;

   begin
      Depth := Depth + 1;

      for Each in 1 .. Depth-1
      loop
         put ("  ");
      end loop;

      put_Line (the_Element.Name);

      for Each in the_Children'range
      loop
         show_Element (the_Children (Each).all);
      end loop;

      Depth := Depth - 1;
   end show_Element;


begin
   show_Element (the_Tree);
end launch_Tree;
