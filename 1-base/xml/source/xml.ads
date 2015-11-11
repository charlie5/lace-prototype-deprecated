private
with
     ada.Strings.unbounded,
     ada.Containers.vectors;


package XML
--
-- Provides simple XML reader/writer support.
--
-- Heavily based on Chip Richards Ada XML packages.
--
is

   --- Attribute type
   --

   type Attribute_t     is tagged private;
   type Attributes_t    is array (Positive range <>) of aliased Attribute_t;

   type Attributes_view is access all Attributes_t;


   function Name  (Self : in Attribute_t) return String;
   function Value (Self : in Attribute_t) return String;



   --- Element type
   --

   type Element  is tagged private;
   type Elements is array (Positive range <>) of access Element;



   -- Construction
   --

   function to_XML (Filename : in String) return Element;
   --
   -- Parses 'Filename' and returns the root node Element of the parsed XML tree.



   -- Attributes
   --

   function  Name       (Self : in     Element) return String;
   function  Attributes (Self : in     Element) return Attributes_t;
   function  Data       (Self : in     Element) return String;

   function  Attribute  (Self : in     Element;   Named : in String) return access Attribute_t'Class;
   --
   -- Returns null if the named attribute does not exist.


   -- Hierachy
   --

   function  Parent     (Self : in     Element) return access Element;
   function  Children   (Self : in     Element) return Elements;


   function  Child      (Self : in     Element;   Named : in String) return access Element;
   --
   -- Returns null if the named child does not exist.


   function  Children   (Self : in     Element;   Named : in String) return Elements;

   procedure add_Child  (Self : in out Element;   the_Child : access Element);





private

   use ada.Strings.unbounded;


   type Attribute_t is tagged
      record
         Name  : unbounded_String;
         Value : unbounded_String;
      end record;



   type Element_view    is access all Element;

   package element_Vectors is new ada.containers.Vectors (Positive, Element_view);
   subtype element_Vector  is element_vectors.Vector;


   type Element is tagged
      record
         Name       : unbounded_String;
         Attributes : Attributes_view;
         Data       : unbounded_String;

         Parent     : Element_view;
         Children   : element_Vector;
      end record;

end XML;
