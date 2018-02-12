with
     openGL.Model;


package openGL.Model.line
--
--  Provides an abstract class for line models.
--
is

   type Item is abstract new openGL.Model.item with private;



private

   type Item is abstract new openGL.Model.item with
      record
         null;
      end record;

end openGL.Model.line;
