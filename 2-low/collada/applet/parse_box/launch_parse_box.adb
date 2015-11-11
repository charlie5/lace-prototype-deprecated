with
     collada.Document,

     ada.Strings.unbounded,
     ada.Text_IO;


procedure launch_parse_Box
--
-- Loads an xml file, parses it into a collada Document.
--
is
   the_Asset : collada.Document.item := collada.document.to_Document ("./box.dae");
begin
   null;
end;
