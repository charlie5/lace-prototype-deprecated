private
with
     openGL.Buffer.indices;

package openGL.Primitive.indexed
--
--  Provides a class for indexed openGL primitives.
--
is
   type    Item  is limited new Primitive.item with private;
   subtype Class is Item'Class;

   type    View  is access all Item'class;
   type    Views is array (Index_t range <>) of View;


   ---------
   --  Forge
   --

   function  new_Primitive (Kind       : in facet_Kind;
                            Indices    : in openGL.Indices;
                            line_Width : in Real := unused_line_Width) return Primitive.indexed.view;

   function  new_Primitive (Kind       : in facet_Kind;
                            Indices    : in openGL.long_Indices;
                            line_Width : in Real := unused_line_Width) return Primitive.indexed.view;

   procedure define  (Self : in out Item;   Kind       : in facet_Kind;
                                            Indices    : in openGL.Indices;
                                            line_Width : in Real);

   procedure define  (Self : in out Item;   Kind       : in facet_Kind;
                                            Indices    : in openGL.long_Indices;
                                            line_Width : in Real);
   overriding
   procedure destroy (Self : in out Item);


   --------------
   --  Attributes
   --

   procedure Indices_are (Self : in out Item;   Now : in      Indices);
   procedure Indices_are (Self : in out Item;   Now : in long_Indices);


   --------------
   --  Operations
   --

   overriding
   procedure render (Self : in out Item);



private

   type Item is limited new Primitive.item with
      record
         Indices : Buffer.indices.view;
      end record;

end openGL.Primitive.indexed;
