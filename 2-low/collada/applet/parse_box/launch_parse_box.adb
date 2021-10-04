with
     collada.Document;

procedure launch_parse_Box
--
-- Loads an xml file, parses it into a collada document.
--
is
   the_Asset : collada.Document.item := collada.document.to_Document ("./box.dae")
     with unreferenced;
begin
   null;
end launch_parse_Box;
