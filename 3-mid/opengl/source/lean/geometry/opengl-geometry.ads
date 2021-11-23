with
     openGL.Primitive,
     openGL.Buffer,
     openGL.Program,
     openGL.Texture;

private
with
     ada.Strings.unbounded;

package openGL.Geometry
--
--  Provides a base class for openGL geometry.
--  A Geometry is composed of up to 5 primitives.
--  Each primitive has its own set of GL indices and a facet kind.
--  All primitives share a common set of vertices.
--  Subclasses may be created to provide for the various possible variants of an openGL vertex.
--
is
   type    Item  is abstract tagged limited private;
   subtype Class is Item'Class;

   type    View  is access all Item'class;
   type    Views is array (Index_t range <>) of View;


   ---------
   --- Forge
   --

   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);


   --------------
   --  Attributes
   --

   procedure Label_is        (Self : in out Item'Class;   Now : in String);
   function  Label           (Self : in     Item'Class)     return String;

   procedure Texture_is      (Self : in out Item'Class;   Now : in Texture.Object);
   function  Texture         (Self : in     Item'Class)     return Texture.Object;

   procedure Bounds_are      (Self : in out Item'Class;   Now : in Bounds);
   function  Bounds          (self : in     Item'Class)     return Bounds;   -- Returns the bounds in object space.

   procedure is_Transparent  (Self : in out Item;         Now : in Boolean := True);
   function  is_Transparent  (Self : in     Item)           return Boolean;

   procedure Program_is      (Self : in out Item;         Now : in Program.view);
   function  Program         (Self : in     Item)           return Program.view;

   procedure add             (Self : in out Item'Class;   the_Primitive : in Primitive.view);
   function  Primitives      (Self : in     Item'Class)     return Primitive.views;
   procedure free_Primitives (Self : in out Item);

   procedure Indices_are     (Self : in out Item;   Now       : in Indices;
                                                    for_Facia : in Positive);
   procedure Indices_are     (Self : in out Item;   Now       : in long_Indices;
                                                    for_Facia : in Positive);

   --------------
   --  Operations
   --

   procedure render         (Self : in out Item'Class);
   procedure enable_Texture (Self : in     Item) is null;


   -----------
   --  Normals
   --

   function Normals_of (face_Kind : in primitive.facet_Kind;
                        Indices   : in openGL.Indices;
                        Sites     : in openGL.Sites) return access Normals;

   function Normals_of (face_Kind : in primitive.facet_Kind;
                        Indices   : in openGL.long_Indices;
                        Sites     : in openGL.Sites) return access Normals;


private
   use ada.Strings.unbounded;

   type Item is abstract tagged limited
      record
         Label           : unbounded_String;
         Texture         : openGL.Texture.Object := openGL.Texture.null_Object;

         Program         : openGL.Program.view;
         Vertices        : Buffer.view;

         Primitives      : Primitive.views (1 .. 5);
         primitive_Count : Index_t := 0;

         is_Transparent  : Boolean := False;     -- Geometry contains lucid colors.
         Bounds          : openGL.Bounds;
      end record;


   generic
      type any_Index_t is range <>;
      with function get_Site (Index : in any_Index_t) return Vector_3;
   function get_Bounds (Count : in Natural) return openGL.Bounds;

   generic
      type any_Index_t is range <>;
      with function get_Color (Index : in any_Index_t) return rgba_Color;
   function get_Transparency (Count : in Natural) return Boolean;

end openGL.Geometry;
