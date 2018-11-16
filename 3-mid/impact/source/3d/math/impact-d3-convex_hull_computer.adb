with interfaces.c.Pointers,

     Swig.Pointers,

     ada.Unchecked_Deallocation,
     system.Address_to_Access_Conversions;
with Ada.Unchecked_Conversion;
with impact.d3.Vector;
--  with math.algebra.linear.d3;
with ada.containers.Generic_Array_Sort;
with impact.d3.Scalar;



package body impact.d3.convex_hull_Computer
is

   use Math,
       Interfaces;



   --- Edge
   --

   default_Terminator : constant Edge := (others => -1);

   package edge_Pointers is new interfaces.c.Pointers (Positive, Edge, Edges,  default_Terminator);
   subtype edge_Pointer  is     edge_Pointers.Pointer;





   function getSourceVertex (Self : access Edge) return Integer
   is
      use edge_Pointers, Interfaces;
      the_Edge : constant edge_Pointer := edge_Pointer (Self) + C.ptrdiff_t (Self.m_reverse);
   begin
      return the_Edge.targetVertex;
   end getSourceVertex;




   function getTargetVertex (Self : in     Edge) return Integer
   is
   begin
      return Self.targetVertex;
   end getTargetVertex;




   function getNextEdgeOfVertex (Self : access Edge) return Edge_view
   is
      use edge_Pointers, Interfaces;
      the_Edge : constant edge_Pointer := edge_Pointer (Self) + C.ptrdiff_t (Self.next);
   begin
      return Edge_view (the_Edge);
   end getNextEdgeOfVertex;




   function getNextEdgeOfFace (Self : access Edge) return Edge_view
   is
      use edge_Pointers, Interfaces;
      the_Edge : constant edge_Pointer := edge_Pointer (Self) + C.ptrdiff_t (Self.m_reverse);
   begin
      return the_Edge.getNextEdgeOfVertex;
   end getNextEdgeOfFace;




   function getReverseEdge (Self : access Edge) return Edge_view
   is
      use edge_Pointers, Interfaces;
   begin
      return Edge_view (edge_Pointer (Self) + C.ptrdiff_t (Self.m_reverse));
   end getReverseEdge;






   --- impact.d3.convex_hull_Computer
   --

   function compute (Self : access Item'Class;   coords       : access math.Real;
                                                 stride       : in     Integer;
                                                 count        : in     Integer;
                                                 shrink       : in     math.Real;
                                                 shrinkClamp  : in     math.Real) return math.Real
   is
   begin
      return Self.compute (coords.all'Address, False, stride, count, shrink, shrinkClamp);
   end compute;




   function compute (Self : access Item'Class;   coords       : access Long_Float;
                                                 stride       : in     Integer;
                                                 count        : in     Integer;
                                                 shrink       : in     math.Real;
                                                 shrinkClamp  : in     math.Real) return math.Real
   is
   begin
      return Self.compute (coords.all'Address, True, stride, count, shrink, shrinkClamp);
   end compute;


















   --- Point64
   --


   function isZero (Self : in Point64'Class) return Boolean
   is
   begin
      return Self.x = 0  and then  Self.y = 0  and then  Self.z = 0;
   end isZero;



   function dot (Self : in Point64'Class;   b : in Point64'Class) return int64_t
   is
   begin
      return Self.x * b.x  +  Self.y * b.y  +  Self.z * b.z;
   end dot;








   --- Point32
   --


   function to_Point32 (x, y, z : in int32_t) return Point32
   is
   begin
      return (x, y, z, -1);
   end to_Point32;


   overriding function "=" (Self : in Point32;   Other : in Point32) return Boolean
   is
   begin
      return Self.x = Other.x  and then  Self.y = Other.y  and then  Self.z = Other.z;
   end;



   function isZero (Self : in Point32) return Boolean
   is
   begin
      return Self.x = 0  and then  Self.y = 0  and then  Self.z = 0;
   end isZero;




   function cross (Self : in Point32'Class;   b : in Point32'Class) return Point64
   is
   begin
      return Point64'(int64_t (Self.y) * int64_t (b.z)  -  int64_t (Self.z) * int64_t (b.y),
                      int64_t (Self.z) * int64_t (b.x)  -  int64_t (Self.x) * int64_t (b.z),
                      int64_t (Self.x) * int64_t (b.y)  -  int64_t (Self.y) * int64_t (b.x));
   end cross;




   function cross (Self : in Point32'Class;   b : in Point64'Class) return Point64
   is
   begin
      return Point64'(int64_t (Self.y) * int64_t (b.z)  -  int64_t (Self.z) * int64_t (b.y),
                      int64_t (Self.z) * int64_t (b.x)  -  int64_t (Self.x) * int64_t (b.z),
                      int64_t (Self.x) * int64_t (b.y)  -  int64_t (Self.y) * int64_t (b.x));
   end cross;





   function dot (Self : in Point32;   b : in Point32) return int64_t
   is
   begin
      return   int64_t (Self.x) * int64_t (b.x)
             + int64_t (Self.y) * int64_t (b.y)
             + int64_t (Self.z) * int64_t (b.z);
   end dot;



   function dot (Self : in Point32;   b : in Point64'Class) return int64_t
   is
   begin
      return   int64_t (Self.x) * int64_t (b.x)
             + int64_t (Self.y) * int64_t (b.y)
             + int64_t (Self.z) * int64_t (b.z);
   end dot;




   function "+" (Self, b : in Point32) return Point32
   is
   begin
      return to_Point32 (Self.x + b.x,
                         Self.y + b.y,
                         Self.z + b.z);
   end;




   function "-" (Self, b : in Point32) return Point32
   is
   begin
      return to_Point32 (Self.x - b.x,
                         Self.y - b.y,
                         Self.z - b.z);
   end;






   --- Int128
   --

   type Int128 is tagged
      record
         low, high : uint64_t;
      end record;




   --- DMul
   --

--     generic
--        type UWord  is private;
--        type UHWord is private;

   package DMul
   is

      procedure mul (a, b : in uint64_t;   resLow, resHigh : out uint64_t);
      procedure mul (a, b : in Int128;     resLow, resHigh : out Int128);

   private


      function  high (value : in uint64_t) return uint32_t;
      function  low  (value : in uint64_t) return uint32_t;

      function  mul  (a, b  : in uint32_t) return uint64_t;

      procedure shlHalf (value : in out uint64_t);


      function  high (value : in Int128) return uint64_t;
      function  low  (value : in Int128) return uint64_t;

      function  mul  (a, b  : in uint64_t) return Int128;

      procedure shlHalf (value : in out Int128);

   end DMul;





   function to_Int128 (low : in uint64_t) return Int128
   is
   begin
      return (low, 0);
   end to_Int128;




   function to_Int128 (value : in int64_t) return Int128
   is
      Result : Int128;
   begin
      Result.low := uint64_t (value);

      if Value >= 0 then
         Result.high :=  0;
      else
         Result.high := -1;
      end if;


      return Result;
   end to_Int128;




   function "-" (Self : in Int128'Class) return Int128
   is
   begin
      if Self.low = 0 then
         return (low  => uint64_t (-int64_t (Self.low)),
                 high => not Self.high + 1);
      else
         return (low  => uint64_t (-int64_t (Self.low)),
                 high => not Self.high + 0);
      end if;
   end;






   function mul (a, b : in  int64_t) return Int128
   is
      negative : Boolean := a < 0;
      my_a     : int64_t := a;
      my_b     : int64_t := b;

      result   : Int128;
   begin
      if negative then
         my_a := -my_a;
      end if;

      if my_b < 0 then
         negative := not negative;
         my_b     := -my_b;
      end if;

      DMul.mul (uint64_t (my_a), uint64_t (my_b),  result.low, result.high);

      if negative then
         return -result;
      else
         return  result;
      end if;
   end mul;





   function mul (a, b : in uint64_t) return Int128
   is
      result : Int128;
   begin
      DMul.mul (a, b,  result.low, result.high);

      return result;
   end mul;






   function "+" (Self, b : in Int128) return Int128
   is
      lo : constant uint64_t := Self.low + b.low;
   begin
      if lo < Self.low then
         return (low  => lo,
                 high => Self.high + b.high + 1);
      else
         return (low  => lo,
                 high => Self.high + b.high + 0);
      end if;
   end;




   function "-" (Self, b : in Int128) return Int128
   is
   begin
      return Self + (-b);
   end;





   procedure add (Self : in out Int128'Class;    b : in Int128)
   is
      lo : constant uint64_t := Self.low + b.low;
   begin
      if lo < Self.low then
         Self.high := Self.high + 1;
      end if;

      Self.low  := lo;
      Self.high := Self.high + b.high;
   end add;




   procedure inc (Self : in out Int128'Class)
   is
   begin
      Self.low := Self.low + 1;

      if Self.low = 0 then
         Self.high := Self.high + 1;
      end if;
   end inc;




   function "*" (Self : in Int128;   b : in int64_t) return Int128
   is
      negative : Boolean := int64_t (Self.high) < 0;
      a        : Int128;
      my_b     : int64_t := b;

      Result   : Int128;
   begin
      if negative then
         a := -Self;
      else
         a :=  Self;
      end if;


      if my_b < 0 then
         negative := not negative;
         my_b        := -my_b;
      end if;

      result := mul (a.low, uint64_t (my_b));

      result.high := result.high +  a.high * uint64_t (my_b);

      if negative then
         return -result;
      else
         return  result;
      end if;
   end;






   function toScalar (Self : in Int128'Class) return math.Real
   is
   begin
      if int64_t (Self.high) >= 0 then
         return math.Real (Self.high) * (math.Real (16#1_0000_0000#) * math.Real (16#1_0000_0000#)) + math.Real (Self.low);
      else
         return -toScalar (-Self);
      end if;
   end toScalar;





   function getSign (Self : in Int128'Class) return Integer
   is
   begin
      if int64_t (Self.high) < 0 then
         return -1;
      else
         if Self.high /= 0 or else Self.low /= 0 then
            return 1;
         else
            return 0;
         end if;
      end if;
   end getSign;




   function "<" (Self : in Int128;   b : in Int128) return Boolean
   is
   begin
      return     Self.high < b.high
        or else (       Self.high = b.high
                 and then Self.low  < b.low);
   end;






   function ucmp (Self : in Int128;   b : in Int128) return Integer
   is
   begin
      if Self.high < b.high then
         return -1;
      end if;

      if Self.high > b.high then
         return 1;
      end if;

      if Self.low < b.low then
         return -1;
      end if;

      if Self.low > b.low then
         return 1;
      end if;

      return 0;
   end ucmp;










   --- Rational64
   --


   function to_Rational64 (numerator, denominator : int64_t) return Rational64
   is
      Self : Rational64;
   begin

      if numerator > 0 then
         Self.sign      := 1;
         Self.numerator := uint64_t (numerator);

      elsif numerator < 0 then
         Self.sign      := -1;
         Self.numerator := uint64_t (-numerator);

      else
         Self.sign      := 0;
         Self.numerator := 0;
      end if;


      if denominator > 0 then
         Self.denominator := uint64_t (denominator);

      elsif denominator < 0 then
         Self.sign        := -Self.sign;
         Self.denominator := uint64_t (-denominator);

      else
         Self.denominator := 0;
      end if;


      return Self;
   end to_Rational64;





   function isNegativeInfinity (Self : in Rational64) return Boolean
   is
   begin
      return Self.sign < 0  and then  Self.denominator = 0;
   end isNegativeInfinity;



   function isNaN (Self : in Rational64) return Boolean
   is
   begin
      return Self.sign = 0  and then  Self.denominator = 0;
   end isNaN;




   function compare (Self, b : in Rational64) return Integer
   is
   begin
      if Self.sign /= b.sign then
         return Self.sign - b.sign;

      elsif Self.sign = 0 then
         return 0;
      end if;


      --  // return (numerator * b.denominator > b.numerator * denominator) ? sign : (numerator * b.denominator < b.numerator * denominator) ? -sign : 0;

      return Self.sign  *  ucmp (mul (Self.numerator, b.denominator),  mul (Self.denominator, b.numerator));
   end compare;






   function toScalar (Self : in Rational64) return math.Real
   is
      Value : math.Real;
   begin
      if Self.denominator = 0 then
         Value := math.Infinity;
      else
         Value := math.Real (Self.numerator) / math.Real (Self.denominator);
      end if;


      return math.Real (Self.sign) * Value;
   end toScalar;












   --- Rational128
   --

   type Rational128 is tagged
      record
         numerator,
         denominator : Int128;

         sign        : Integer;
         isInt64     : Boolean;
      end record;



   function to_Rational128 (value : in int64_t) return Rational128
   is
      Self : Rational128;
   begin
      if value > 0 then
         Self.sign      := 1;
         Self.numerator := to_Int128 (value);

      elsif value < 0 then
         Self.sign      := -1;
         Self.numerator := to_Int128 (-value);

      else
         Self.sign      := 0;
         Self.numerator := to_Int128 (uint64_t'(0));
      end if;


      Self.denominator := to_Int128 (uint64_t'(1));
      Self.isInt64     := True;

      return Self;
   end to_Rational128;




   function to_Rational128 (numerator, denominator : in Int128) return Rational128
   is
      Self  : Rational128;
      dsign : Integer;
   begin
      Self.sign := numerator.getSign;

      if Self.sign >= 0 then
         Self.numerator := numerator;
      else
         Self.numerator := -numerator;
      end if;


      dsign := denominator.getSign;

      if dsign >= 0 then
         Self.denominator := denominator;
      else
         Self.sign        := -Self.sign;
         Self.denominator := -denominator;
      end if;

      Self.isInt64 := False;


      return Self;
   end to_Rational128;




   function compare (Self : in Rational128;   b : in int64_t) return Integer
   is
      a    : int64_t;
      my_b : int64_t := b;
   begin
      if Self.isInt64 then
         a := int64_t (Self.sign) * int64_t (Self.numerator.low);

         if a > my_b then
            return 1;
         elsif a < b then
            return -1;
         else
            return 0;
         end if;
      end if;


      if my_b > 0 then
         if Self.sign <= 0 then
            return -1;
         end if;

      elsif my_b < 0 then
         if Self.sign >= 0 then
            return 1;
         end if;

         my_b := -my_b;

      else
         return Self.sign;
      end if;


      return ucmp (Self.numerator,  Self.denominator * my_b)  *  Self.sign;
   end compare;






   function compare (Self : in Rational128;   b : in Rational128) return Integer
   is
      cmp             : Integer;

      nbdLow, nbdHigh,
      dbnLow, dbnHigh : Int128;
   begin
      if Self.sign /= b.sign then
         return Self.sign - b.sign;

      elsif Self.sign = 0 then
         return 0;
      end if;


      if Self.isInt64 then
         return -compare (b,  int64_t (Self.sign) * int64_t (Self.numerator.low));
      end if;

      DMul.mul (Self.numerator,   b.denominator, nbdLow, nbdHigh);
      DMul.mul (Self.denominator, b.numerator,   dbnLow, dbnHigh);

      cmp := ucmp (nbdHigh, dbnHigh);

      if cmp /= 0 then
         return cmp * Self.sign;
      end if;

      return ucmp (nbdLow, dbnLow)  *  Self.sign;
   end compare;








   function toScalar (Self : in Rational128;   b : in int64_t) return math.Real
   is
      pragma Unreferenced (b);
      Value : math.Real;
   begin
      if Self.denominator.getSign = 0 then
         Value := math.Infinity;
      else
         Value := Self.numerator.toScalar / Self.denominator.toScalar;
      end if;


      return math.Real (Self.sign) * Value;
   end toScalar;











   --- PointR128
   --

   type PointR128 is tagged
      record
         x, y, z     : Int128;
         denominator : Int128;
      end record;




   function xvalue (Self : in PointR128'Class) return math.Real
   is
   begin
      return Self.x.toScalar / Self.denominator.toScalar;
   end xvalue;



   function yvalue (Self : in PointR128'Class) return math.Real
   is
   begin
      return Self.y.toScalar / Self.denominator.toScalar;
   end yvalue;



   function zvalue (Self : in PointR128'Class) return math.Real
   is
   begin
      return Self.z.toScalar / Self.denominator.toScalar;
   end zvalue;




   type Vertex        is tagged;
   type internal_Edge is tagged;
   type Face          is tagged;


   type Vertex_view        is access all Vertex;
   type internal_Edge_view is access all internal_Edge;
   type Face_view          is access all Face;


   --- Vertex
   --

   type Vertex is tagged
      record
         next            : Vertex_view;
         prev            : Vertex_view;

         edges           : internal_Edge_view;

         firstNearbyFace : Face_view;
         lastNearbyFace  : Face_view;

         point128        : PointR128;
         point           : Point32;
         copy            : Integer := -1;
      end record;


   type Vertices is array (Positive range <>) of aliased Vertex;


   null_Vertex : constant Vertex := (others => <>);

   package vertex_Pointers is new interfaces.c.Pointers (Positive, Vertex, Vertices,  null_Vertex);
   subtype vertex_Pointer  is     vertex_Pointers.Pointer;




   type internal_Edge is tagged
      record
         next,
         prev      : internal_Edge_view;
         m_reverse : internal_Edge_view;

         target    : Vertex_view;
         face      : access convex_hull_Computer.Face;

         copy      : Integer;
      end record;



   type Face is tagged
      record
         next                     : Face_view;
         nearbyVertex             : Vertex_view;
         nextWithSameNearbyVertex : Face_view;

         origin,
         dir0,
         dir1                     : Point32;
      end record;







   procedure destruct (Self : in out Vertex)
   is
   begin
      null;
   end destruct;




   function next (Self : in Vertex) return Vertex_view
   is
   begin
      return Self.next;
   end next;




   procedure set_next (Self : in out Vertex;   To : in Vertex_view)
   is
   begin
      Self.next := To;
   end set_next;





   function "-" (Self, b : in Vertex) return Point32
   is
   begin
      return Self.point - b.point;
   end;




   function dot (Self : in Vertex;   b : in Point64) return Rational128;

   function dot (Self : in Vertex;   b : in Point64) return Rational128
   is
   begin
      if Self.point.index >= 0 then
         return to_Rational128 (Self.point.dot (b));
      else
         return to_Rational128 (Self.point128.x * b.x  +  Self.point128.y * b.y  +  Self.point128.z * b.z,
                                Self.point128.denominator);
      end if;
   end dot;




   function xvalue (Self : in Vertex) return math.Real
   is
   begin
      if Self.point.index >= 0 then
         return math.Real (Self.point.x);
      else
         return Self.point128.xvalue;
      end if;
   end xvalue;




   function yvalue (Self : in Vertex) return math.Real
   is
   begin
      if Self.point.index >= 0 then
         return math.Real (Self.point.y);
      else
         return Self.point128.yvalue;
      end if;
   end yvalue;




   function zvalue (Self : in Vertex) return math.Real
   is
   begin
      if Self.point.index >= 0 then
         return math.Real (Self.point.z);
      else
         return Self.point128.zvalue;
      end if;
   end zvalue;




   procedure receiveNearbyFaces (Self : access Vertex;   src : access Vertex)
   is
      f : access Face;
   begin
      if Self.lastNearbyFace /= null then
         Self.lastNearbyFace.nextWithSameNearbyVertex := src.firstNearbyFace;
      else
         Self.firstNearbyFace := src.firstNearbyFace;
      end if;

      if src.lastNearbyFace /= null then
         Self.lastNearbyFace := src.lastNearbyFace;
      end if;


      f := src.firstNearbyFace;

      while f /= null loop
         pragma Assert (f.nearbyVertex = src);

         f.nearbyVertex := Self.all'Access;
         f              := f.nextWithSameNearbyVertex;
      end loop;


      src.firstNearbyFace := null;
      src.lastNearbyFace  := null;
   end receiveNearbyFaces;





   --- internal_Edge
   --


   procedure destruct (Self : in out internal_Edge)
   is
   begin
      null;
   end destruct;



   procedure link (Self : access internal_Edge'Class;   n : in internal_Edge_view)
   is
      pragma Assert (Self.m_reverse.target = n.m_reverse.target);
   begin
      Self.next := n;
      n.prev    := internal_Edge_view (Self); --.all'access;
   end link;



   function next (Self : in internal_Edge) return internal_Edge_view
   is
   begin
      return Self.next;
   end next;




   procedure set_next (Self : in out internal_Edge;   To : in internal_Edge_view)
   is
   begin
      Self.next := To;
   end set_next;






   --- Face
   --


   procedure destruct (Self : in out Face)
   is
   begin
      null;
   end destruct;



   procedure init (Self : access Face;   a, b, c : in Vertex_view)
   is
   begin
      Self.nearbyVertex := a;
      Self.origin       := a.point;
      Self.dir0         := b.all - a.all;
      Self.dir1         := c.all - a.all;

      if a.lastNearbyFace /= null then
         a.lastNearbyFace.nextWithSameNearbyVertex := Self.all'Access;
      else
         a.firstNearbyFace := Self.all'Access;
      end if;

      a.lastNearbyFace := Self.all'Access;
   end init;





   function getNormal (Self : in Face) return Point64
   is
   begin
      return Point64 (cross (Self.dir0,  Self.dir1));
   end getNormal;




   function next (Self : in Face) return Face_view
   is
   begin
      return Self.next;
   end next;




   procedure set_next (Self : in out Face;   To : in Face_view)
   is
   begin
      Self.next := To;
   end set_next;





   --- DMul
   --


   package body DMul
   is


      function high (value : in uint64_t) return uint32_t
      is
      begin
         return uint32_t (value / 2**32);
      end high;



      function low (value : in uint64_t) return uint32_t
      is
      begin
         return uint32_t (value);
      end low;



      function mul  (a, b  : in uint32_t) return uint64_t
      is
      begin
         return uint64_t (a) * uint64_t (b);
      end mul;



      procedure shlHalf (value : in out uint64_t)
      is
      begin
         Value := Value * 2**32;
      end shlHalf;


      function  high (value : in Int128) return uint64_t
      is
      begin
         return value.high;
      end high;


      function  low  (value : in Int128) return uint64_t
      is
      begin
         return value.low;
      end low;


      function  mul  (a, b  : in uint64_t) return Int128
      is
      begin
         return impact.d3.convex_Hull_Computer.mul (a, b);
      end mul;


      procedure shlHalf (value : in out Int128)
      is
      begin
         value.high := value.low;
         value.low  := 0;
      end shlHalf;



      procedure mul (a, b : in uint64_t;   resLow, resHigh : out uint64_t)
      is
         p00   : uint64_t := mul (low  (a),  low  (b));
         p01   : constant uint64_t := mul (low  (a),  high (b));
         p10   : constant uint64_t := mul (high (a),  low  (b));
         p11   : uint64_t := mul (high (a),  high (b));
         p0110 : uint64_t := uint64_t (low (p01))  +  uint64_t (low (p10));
      begin
         p11 := p11 + uint64_t (high (p01));
         p11 := p11 + uint64_t (high (p10));
         p11 := p11 + uint64_t (high (p0110));

         shlHalf (p0110);

         p00 := p00 + p0110;

         if p00 < p0110 then
            p11 := p11 + 1;
         end if;

         resLow  := p00;
         resHigh := p11;
      end mul;




      procedure mul (a, b : in Int128;     resLow, resHigh : out Int128)
      is
         p00   : Int128 := mul (low  (a),  low  (b));
         p01   : constant Int128 := mul (low  (a),  high (b));
         p10   : constant Int128 := mul (high (a),  low  (b));
         p11   : Int128 := mul (high (a),  high (b));
         p0110 : Int128 := to_Int128 (low (p01))  +  to_Int128 (low (p10));
      begin
         p11.add (to_Int128 (high (p01)));
         p11.add (to_Int128 (high (p10)));
         p11.add (to_Int128 (high (p0110)));

         shlHalf (p0110);

         p00 := p00 + p0110;

         if p00 < p0110 then
            p11.inc;
         end if;

         resLow  := p00;
         resHigh := p11;
      end mul;



--        procedure mul (a, b : in UWord;   resLow, resHigh : out UWord)
--        is
--           p00   : UWord := mul (low  (a),  low  (b));
--           p01   : UWord := mul (low  (a),  high (b));
--           p10   : UWord := mul (high (a),  low  (b));
--           p11   : UWord := mul (high (a),  high (b));
--           p0110 : UWord := UWord (low (p01))  +  UWord (low (p10));
--        begin
--           p11 := p11 + high (p01);
--           p11 := p11 + high (p10);
--           p11 := p11 + high (p0110);
--
--           shlHalf (p0110);
--
--           p00 := p00 + p0110;
--
--           if p00 < p0110 then
--              p11 := p11 + 1;
--           end if;
--
--           resLow  := p00;
--           resHigh := p11;
--        end;


   end DMul;







   --- IntermediateHull
   --

   type IntermediateHull is
      record
          minXy, maxXy,
          minYx, maxYx : Vertex_view;
      end record;





   --- Orientation
   --

   type Orientation is (NONE, CLOCKWISE, COUNTER_CLOCKWISE);






   generic
      type T      is private;
      type T_view is access all T;

      with procedure set_next (Self : in out T;   Now : in T_view);

   package PoolArray
   is

      type Item is tagged private;


      function  to_PoolArray (size : in Integer) return Item;

      procedure destruct (Self : in out Item);


      function  Next    (Self : in     Item)  return access Item;
      procedure Next_is (Self : in out Item;   Now : access Item);


      function init (Self : in Item) return T_view;

--                                  T* init()

   private

      type T_array      is array (Positive range <>) of aliased T;
      type T_array_view is access all T_array;


      type Item is tagged
         record
            m_array : T_array_view;
            size    : Integer;

            next    : access Item;
         end record;



   end PoolArray;






   package body PoolArray
   is

      function  Next    (Self : in     Item)  return access Item
      is
      begin
         return Self.next;
      end Next;



      procedure Next_is (Self : in out Item;   Now : access Item)
      is
      begin
         Self.next := Now;
      end Next_is;



      function to_PoolArray (size : in Integer) return Item
      is
         Self : Item;
      begin
         Self.size    := size;
         Self.m_array := new T_array (1 .. size);

         return Self;
      end to_PoolArray;



      procedure destruct (Self : in out Item)
      is
         procedure free is new ada.Unchecked_Deallocation (T_array, T_array_view);
      begin
         free (Self.m_array);
      end destruct;




      function init (Self : in Item) return T_view
      is
      begin
         for i in 1 .. Self.size loop

            if i + 1 <= Self.size then
               set_next (Self.m_array (i),  Self.m_array (i + 1)'Access);
            else
               set_next (Self.m_array (i),  null);
            end if;

         end loop;


         return Self.m_array (1)'Access;
      end init;


   end PoolArray;




   --- Pool
   --


   generic
      type T      is private;
      type T_view is access all T;

      with procedure destruct (Self : in out T);

      with procedure set_next (Self : in out T;   Now : in T_view);
      with function      next (Self : in     T)     return T_view;

   package Pool
   is
--        type T_view is access all T;



      type Item is tagged private;


      procedure destruct (Self : in out Item);
      procedure reset    (Self : in out Item);

      procedure setArraySize (Self : in out Item;   arraySize : in Integer);

      function  newObject  (Self : access Item) return access T;
      procedure freeObject (Self : in out Item;   object : in T_view);


   private


      package my_PoolArray is new PoolArray (T, T_view, set_Next);

      type my_PoolArray_view is access all my_PoolArray.Item;




      type Item is tagged
         record
            arrays      : my_PoolArray_view;
            nextArray   : my_PoolArray_view;

            freeObjects : T_view;
            arraySize   : Integer := 256;
         end record;



   end Pool;




   package body Pool
   is

      procedure destruct (Self : in out Item)
      is
         p : my_PoolArray_view;

         procedure free is new ada.Unchecked_Deallocation (my_PoolArray.item, my_PoolArray_view);
      begin
         while self.arrays /= null loop
            p           := Self.arrays;
            Self.arrays := my_PoolArray_view (p.next);

            p.destruct;
            free (p);
         end loop;

      end destruct;



      procedure reset (Self : in out Item)
      is
      begin
         Self.nextArray   := Self.arrays;
         Self.freeObjects := null;
      end reset;



      procedure setArraySize (Self : in out Item;   arraySize : in Integer)
      is
      begin
         Self.arraySize := arraySize;
      end setArraySize;





      function newObject (Self : access Item) return access T
      is
         o : T_view           := Self.freeObjects;
         p : my_PoolArray_view;
      begin

         if o = null then
            p := Self.nextArray;

            if p /= null then
               Self.nextArray := my_PoolArray_view (p.next);
            else
               p := new my_PoolArray.Item'(my_PoolArray.to_PoolArray (Self.arraySize));
               p.next_is (Self.arrays);

               Self.arrays := p;
            end if;

            o := T_view (p.init);
         end if;

         Self.freeObjects := T_view (next (o.all));


         return o;
      end newObject;





      procedure freeObject (Self : in out Item;   object : in T_view)
      is
         use my_PoolArray;
      begin
         destruct (object.all);

         set_next (object.all, now => Self.freeObjects);
         Self.freeObjects := object;
      end freeObject;



   end Pool;







   --- Containers
   --

   package vertex_Pool is new Pool (Vertex,        Vertex_view,         destruct, set_next, next);
   package   edge_Pool is new Pool (internal_Edge, internal_Edge_view,  destruct, set_next, next);
   package   face_Pool is new Pool (Face,          Face_view,           destruct, set_next, next);



   package vertex_Vectors is new ada.containers.Vectors (Positive, Vertex_view);
   subtype vertex_Vector  is     vertex_Vectors.Vector;

   package face_Vectors is new ada.containers.Vectors (Positive, Face_view);
   subtype face_Vector  is     face_Vectors.Vector;





   --- Internal
   --

   type Internal is tagged
      record
         scaling,
         center           : math.Vector_3;

         vertexPool       : aliased vertex_Pool.item;
         edgePool         : aliased edge_Pool.item;
         facePool         : aliased face_Pool.item;

         originalVertices : vertex_Vector;

         mergeStamp,
         minAxis,
         medAxis,
         maxAxis,
         usedEdgePairs,
         maxUsedEdgePairs : Integer;

         vertexList       : Vertex_view;
      end record;




   function  mergeProjection (Self : access Internal'Class;   h0, h1 : access IntermediateHull;
                                                              c0, c1 : access Vertex_view   ) return Boolean;




   function getOrientation (prev, next : access internal_Edge;   s, t : in Point32) return Orientation
   is
   begin
      pragma Assert (prev.m_reverse.target = next.m_reverse.target);

      if prev.next = next then

         if prev.prev = next then
            declare
               n : constant Point64 := t.cross (s);
               m : constant Point64 := cross (prev.target.Point - next.m_reverse.target.Point,  next.target.Point - next.m_reverse.target.Point);
               pragma Assert (not m.isZero);

               dot : constant int64_t := n.dot (m);
               pragma Assert (dot /= 0);
            begin
               if dot > 0 then
                  return COUNTER_CLOCKWISE;
               else
                  return CLOCKWISE;
               end if;
            end;
         end if;

         return COUNTER_CLOCKWISE;

      elsif prev.prev = next then
         return CLOCKWISE;

      else
         return NONE;
      end if;
   end getOrientation;








   function  findMaxAngle (Self : in Internal'Class;   ccw    : in     Boolean;
                                                             start  : in     Vertex;
                                                             s      : in     Point32;
                                                             rxs    : in     Point64;
                                                             sxrxs  : in     Point64;
                                                             minCot : access Rational64) return access internal_Edge
   is
      minEdge : internal_Edge_view;
      e       : internal_Edge_view := start.edges;
   begin
      if e /= null then
         loop
            if e.copy > Self.mergeStamp then
               declare
                  t   : constant Point32    := e.target.Point - start.Point;
                  cot : constant Rational64 := to_Rational64 (t.dot (sxrxs),
                                                     t.dot (rxs));
               begin
                  if cot.isNaN then
                     if ccw then
                        null;   pragma Assert (t.dot (s) < 0);
                     else
                        null;   pragma Assert (t.dot (s) > 0);
                     end if;
                  else
                     declare
                        cmp : constant Integer := cot.compare (minCot.all);
                     begin
                        if minEdge = null then
                           minCot.all := cot;
                           minEdge    := e;
                        elsif cmp < 0 then
                           minCot.all  := cot;
                           minEdge     := e;
                        elsif      cmp = 0
                          and then ccw = (getOrientation (minEdge, e, s, t) = COUNTER_CLOCKWISE)
                        then
                           minEdge := e;
                        end if;
                     end;
                  end if;
               end;
            end if;

            e := e.next;
            exit when e = start.edges;
         end loop;
      end if;


      return minEdge;
   end findMaxAngle;








   procedure findEdgeForCoplanarFaces (Self : in out Internal'Class;   c0, c1       : access Vertex;
                                                                             e0, e1       :    out internal_Edge_view;
                                                                             stop0, stop1 : access Vertex)
   is
      start0 : constant internal_Edge_view := e0;
      start1 : constant internal_Edge_view := e1;

      function get_et0 return Point32
      is
      begin
         if start0 /= null then   return start0.target.point;
         else                     return c0.point;
         end if;
      end get_et0;

      function get_et1 return Point32
      is
      begin
         if start1 /= null then   return start1.target.point;
         else                     return c1.point;
         end if;
      end get_et1;

      et0 : Point32 := get_et0;
      et1 : Point32 := get_et1;

      s   : constant Point32 := c1.point - c0.point;


      function get_Start return internal_Edge_view
      is
      begin
         if start0 /= null then   return start0;
         else                     return start1;
         end if;
      end get_Start;

      normal  : constant Point64 := cross (get_Start.target.point - c0.point,  s);
      dist    : constant int64_t := c0.point.dot (normal);                            pragma Assert (start1 = null or else (start1.target.point.dot (normal) = dist));
      perp    : constant Point64 := s.cross (normal);                                 pragma Assert (not perp.isZero);

      maxDot0 : int64_t := et0.dot (perp);
      maxDot1 : int64_t;
      dx, dy  : int64_t;
      dot     : int64_t;

      e       : internal_Edge_view;

   begin
      if e0 /= null then
         while e0.target /= stop0
         loop
            e := e0.m_reverse.prev;

            if e.target.point.dot (normal) < dist then
               exit;
            end if;

            pragma Assert (e.target.point.dot (normal) = dist);

            if e.copy = Self.mergeStamp then
               exit;
            end if;

            dot := e.target.point.dot (perp);

            if dot <= maxDot0 then
               exit;
            end if;

            maxDot0 := dot;
            e0      := e;
            et0     := e.target.point;
         end loop;
      end if;

      maxDot1 := et1.dot (perp);
      if e1 /= null then

         while e1.target /= stop1
         loop
            e := e1.m_reverse.next;

            if e.target.point.dot (normal) < dist then
               exit;
            end if;

            pragma Assert (e.target.point.dot (normal) = dist);

            if e.copy = Self.mergeStamp then
               exit;
            end if;

            dot := e.target.point.dot (perp);

            if dot <= maxDot1 then
               exit;
            end if;

            maxDot1 := dot;
            e1      := e;
            et1     := e.target.point;
         end loop;
      end if;


      dx := maxDot1 - maxDot0;

      if dx > 0 then
         while True
         loop
            declare
               f0,  f1  : internal_Edge_view;
               dx0, dy0 : int64_t;
               dx1, dy1 : int64_t;
               dxn      : int64_t;
               d1       : Point32;

               Continue : Boolean := True;

               function dot_is_negative (dxN, dyN : in int64_t) return Boolean     -- tbd: better name
               is
               begin
                  if dxN = 0 then
                     return dyN < 0;
                  else
                     return     dxN < 0
                       and then to_Rational64 (dyN, dxN).compare (to_Rational64 (dy, dx)) >= 0;
                  end if;
               end dot_is_negative;

            begin
               dy := impact.d3.convex_hull_Computer.dot (et1 - et0, s);

               if e0 /= null and then (e0.target /= stop0) then
                  f0 := e0.next.m_reverse;

                  if f0.copy > Self.mergeStamp then
                     dx0 := impact.d3.convex_hull_Computer.dot (f0.target.point - et0,  perp);
                     dy0 := impact.d3.convex_hull_Computer.dot (f0.target.point - et0,  s);
                     --                                          if (dx0 = 0) ? (dy0 < 0) : ((dx0 < 0) and then (Rational64(dy0, dx0).compare(Rational64(dy, dx)) >= 0)) then
                     if Dot_is_negative (dx0, dy0) then
                        et0 := f0.target.point;
                        dx  := impact.d3.convex_hull_Computer.dot (et1 - et0, perp);
                        if e0 = start0 then
                           e0 := null;
                        else
                           e0 := f0;
                        end if;

                        continue := False;
                     end if;
                  end if;
               end if;

               if Continue then
                  if e1 /= null and then (e1.target /= stop1) then
                     f1 := e1.m_reverse.next;

                     if f1.copy > Self.mergeStamp then
                        d1 := f1.target.point - et1;

                        if d1.dot (normal) = 0 then
                           dx1 := d1.dot (perp);
                           dy1 := d1.dot (s);
                           dxn := impact.d3.convex_hull_Computer.dot (f1.target.point - et0,  perp);
                           --                                                  if dxn > 0 and then ((dx1 = 0) ? (dy1 < 0) : ((dx1 < 0) && (Rational64(dy1, dx1).compare(Rational64(dy, dx)) > 0))) then
                           if dxn > 0 and then Dot_is_negative (dx1, dy1) then
                              e1  := f1;
                              et1 := e1.target.point;
                              dx  := dxn;
                              continue := False;
                           end if;
                        else
                           null;   pragma Assert ((e1 = start1) and then (d1.dot (normal) < 0));
                        end if;

                     end if;
                  end if;
               end if;

               if continue then
                  exit;
               end if;
            end;
         end loop;

      elsif dx < 0 then
         while True
         loop
            declare
               dy       : int64_t;

               f0,  f1  : internal_Edge_view;
               dx0, dy0 : int64_t;
               dx1, dy1 : int64_t;
               dxn      : int64_t;
               d0       : Point32;

               Continue : Boolean := True;

               function dot_is_negative (dxN, dyN : in int64_t) return Boolean   -- tbd: better name
               is
               begin
                  if dxN = 0 then
                     return dyN > 0;
                  else
                     return     dxN < 0
                       and then to_Rational64 (dyN, dxN).compare (to_Rational64 (dy, dx)) <= 0;
                  end if;
               end dot_is_negative;
            begin
               dy := impact.d3.convex_hull_Computer.dot (et1 - et0,  s);

               if e1 /= null and then (e1.target /= stop1) then
                  f1 := e1.prev.m_reverse;

                  if f1.copy > Self.mergeStamp then
                     dx1 := impact.d3.convex_hull_Computer.dot (f1.target.point - et1,  perp);
                     dy1 := impact.d3.convex_hull_Computer.dot (f1.target.point - et1,  s);
                     --                                          if (dx1 = 0) ? (dy1 > 0) : ((dx1 < 0) and then (Rational64(dy1, dx1).compare(Rational64(dy, dx)) <= 0)) then
                     if dot_is_negative (dx1, dy1) then
                        et1 := f1.target.point;
                        dx  := impact.d3.convex_hull_Computer.dot (et1 - et0,  perp);

                        if e1 = start1 then
                           e1 := null;
                        else
                           e1 := f1;
                        end if;

                        continue := False;
                     end if;
                  end if;
               end if;

               if Continue then
                  if e0 /= null and then (e0.target /= stop0) then
                     f0 := e0.m_reverse.prev;

                     if f0.copy > Self.mergeStamp then
                        d0 := f0.target.point - et0;

                        if d0.dot (normal) = 0 then
                           dx0 := d0.dot (perp);
                           dy0 := d0.dot (s);
                           dxn := impact.d3.convex_hull_Computer.dot (et1 - f0.target.point,  perp);
                           --                                                  if (dxn < 0) and then ((dx0 = 0) ? (dy0 > 0) : ((dx0 < 0) and then (Rational64(dy0, dx0).compare(Rational64(dy, dx)) < 0))) then
                           if dxn < 0 and then dot_is_negative (dx0, dy0) then
                              e0  := f0;
                              et0 := e0.target.point;
                              dx  := dxn;
                              continue := True;
                           end if;
                        else
                           null;   pragma Assert ((e0 = start0) and then (d0.dot (normal) < 0));
                        end if;
                     end if;
                  end if;
               end if;

               if Continue then
                  exit;
               end if;
            end;
         end loop;
      end if;
   end findEdgeForCoplanarFaces;






   function  newEdgePair (Self : access Internal'Class;   from, to : in Vertex_view) return internal_Edge_view
   is
      pragma Assert (from /= null and then to /= null);

      e : constant internal_Edge_view := internal_Edge_view (Self.edgePool.newObject);
      r : constant internal_Edge_view := internal_Edge_view (Self.edgePool.newObject);
   begin
      e.m_reverse := r;
      r.m_reverse := e;

      e.copy   := Self.mergeStamp;
      r.copy   := Self.mergeStamp;

      e.target := to;
      r.target := from;

      e.face   := null;
      r.face   := null;

      Self.usedEdgePairs := Self.usedEdgePairs + 1;

      if Self.usedEdgePairs > Self.maxUsedEdgePairs then
         Self.maxUsedEdgePairs := Self.usedEdgePairs;
      end if;

      return e;
   end newEdgePair;








   procedure removeEdgePair (Self : in out Internal'Class;   edge : in internal_Edge_view)
   is
      use edge_Pool;

      n : internal_Edge_view := internal_Edge_view (edge.next);
      r : constant internal_Edge_view := internal_Edge_view (edge.m_reverse);

      pragma Assert (edge.target /= null  and then  r.target /= null);
   begin
      if n /= edge then
         n.prev         := edge.prev;
         edge.prev.next := n;
         r.target.edges := n;
      else
         r.target.edges := null;
      end if;


      n := internal_Edge_view (r.next);

      if n /= r then
         n.prev            := r.prev;
         r.prev.next       := n;
         edge.target.edges := n;
      else
         edge.target.edges := null;
      end if;


      Self.edgePool.freeObject (edge);
      Self.edgePool.freeObject (r);

      Self.usedEdgePairs := Self.usedEdgePairs - 1;
   end removeEdgePair;








   procedure merge (Self : in out Internal'Class;   h0, h1 : access IntermediateHull);


   procedure merge (Self : in out Internal'Class;   h0, h1 : access IntermediateHull)
   is
      c0, c1       : aliased Vertex_view;

      toPrev0,
      firstNew0,

      toPrev1,
      firstNew1         :  internal_Edge_view;

      pendingHead0,
      pendingHead1,
      pendingTail0,
      pendingTail1 : internal_Edge_view;

      prevPoint    : Point32;
   begin
      null;

      if not (h1.maxXy /= null) then
         return;
      end if;

      if not (h0.maxXy /= null) then
         h0.all := h1.all;
         return;
      end if;


      Self.mergeStamp := Self.mergeStamp - 1;

      if mergeProjection (Self'Access,  h0, h1,  c0'Access, c1'Access) then
         declare
            s      : constant Point32 := c1.all - c0.all;
            normal : constant Point64 := to_Point32 (0, 0, -1).cross (s);
            t      : constant Point64 := s.cross (normal);                   pragma Assert (not t.isZero);

            e       : internal_Edge_view := c0.edges;
            start0,
            start1  : internal_Edge_view;

            the_dot     : int64_t;
         begin

            if e /= null then
               loop
                  the_dot := dot ((e.target.all - c0.all), normal);        pragma Assert (the_dot <= 0);

                  if          the_dot = 0
                    and then dot (e.target.all - c0.all, t) > 0
                  then
                     if         start0 = null
                       or else getOrientation (start0, e, s, to_Point32 (0, 0, -1)) = CLOCKWISE
                     then
                        start0 := e;
                     end if;
                  end if;
                  e := e.next;
                  exit when e = c0.edges;
               end loop;
            end if;

            e      := c1.edges;
            start1 := null;

            if e /= null then

               loop
                  the_dot := dot (e.target.all - c1.all, normal);   pragma Assert (the_dot <= 0);

                  if          the_dot = 0
                    and then dot (e.target.all - c1.all,  t)  >  0
                  then
                     if        start1 = null
                       or else getOrientation (start1, e, s, to_Point32 (0, 0, -1)) = COUNTER_CLOCKWISE
                     then
                        start1 := e;
                     end if;
                  end if;

                  e := e.next;
                  exit when e = c1.edges;
               end loop;
            end if;

            if        start0 /= null
              or else start1 /= null
            then
               findEdgeForCoplanarFaces (Self,  c0, c1, start0, start1, null, null);

               if start0 /= null then
                  c0 := start0.target;
               end if;

               if start1 /= null then
                  c1 := start1.target;
               end if;
            end if;

            prevPoint   := c1.point;
            prevPoint.z := prevPoint.z + 1;
         end;
      else
         prevPoint   := c1.point;
         prevPoint.x := prevPoint.x + 1;
      end if;


      declare
         first0   : constant access Vertex := c0;
         first1   : constant access Vertex := c1;
         firstRun : Boolean       := True;

         s       : Point32;
         r       : Point32;
         rxs     : Point64;
         sxrxs   : Point64;

         minCot0 : aliased Rational64;
         min0    : internal_Edge_view;
         minCot1 : aliased Rational64;
         min1    :  internal_Edge_view;

         e,
         e0, e1  :  internal_Edge_view;

         cmp     : Integer;

         n       : internal_Edge_view;
      begin
         loop
            s     := c1.all - c0.all;
            r     := prevPoint - c0.point;
            rxs   := r.cross (s);
            sxrxs := s.cross (rxs);

            minCot0 := to_Rational64 (0, 0);
            min0    := findMaxAngle (Self,  False, c0.all, s, rxs, sxrxs, minCot0'Access).all'Access;
            minCot1 := to_Rational64 (0, 0);
            min1    := findMaxAngle (Self,  True,  c1.all, s, rxs, sxrxs, minCot1'Access).all'Access;

            if          not (min0 = null)
              and then      min1 = null
            then
               e := Self.newEdgePair (c0, c1);
               e.link (e);
               c0.edges := e;

               e := e.m_reverse;
               e.link (e);
               c1.edges := e;
               return;

            else


               if    min0 = null then   cmp := +1;
               elsif min1 = null then   cmp := -1;
               else                     cmp := minCot0.compare (minCot1);
               end if;

               --  cmp := !min0 ? 1 : !min1 ? -1 : minCot0.compare(minCot1);


               if        firstRun
                 or else (      (cmp >= 0 and then not isNegativeInfinity (minCot1))
                          or else (cmp <  0 and then not isNegativeInfinity (minCot0)))
               then

                  e := Self.newEdgePair (c0, c1);

                  if pendingTail0 /= null then
                     pendingTail0.prev := e;
                  else
                     pendingHead0 := e;
                  end if;

                  e.next       := pendingTail0;
                  pendingTail0 := e;

                  e := e.m_reverse;

                  if pendingTail1 /= null then
                     pendingTail1.next := e;
                  else
                     pendingHead1 := e;
                  end if;

                  e.prev       := pendingTail1;
                  pendingTail1 := e;
               end if;

               e0 := min0;
               e1 := min1;


               if cmp = 0 then
                  Self.findEdgeForCoplanarFaces (c0, c1, e0, e1, null, null);
               end if;

               if         cmp >= 0
                 and then e1 /= null
               then
                  if toPrev1 /= null then
                     e := toPrev1.next;
                     n := null;

                     while e /= min1
                     loop
                        n := e.next;
                        removeEdgePair (Self, e);
                        e := n;
                     end loop;

                     --  for (Edge* e = toPrev1.next, *n = NULL; e != min1; e = n)
                     --  {
                     --        n := e.next;
                     --        removeEdgePair (e);
                     --  }
                  end if;

                  if pendingTail1 /= null then
                     if toPrev1 /= null then
                        toPrev1.link (pendingHead1);
                     else
                        min1.prev.link (pendingHead1);
                        firstNew1 := pendingHead1;
                     end if;

                     pendingTail1.link (min1);
                     pendingHead1 := null;
                     pendingTail1 := null;

                  elsif toPrev1 = null then
                     firstNew1 := min1;
                  end if;

                  prevPoint := c1.point;
                  c1        := e1.target;
                  toPrev1   := e1.m_reverse;
               end if;

               if         cmp <= 0
                 and then e0  /= null
               then
                  if toPrev0 /= null then
                     e := toPrev0.prev;
                     n := null;

                     while e /= min0
                     loop
                        n := e.prev;
                        Self.removeEdgePair (e);
                        e := n;
                     end loop;

                     --  for (Edge* e = toPrev0.prev, *n = NULL; e != min0; e = n)
                     --  {
                     --        n := e.prev;
                     --        removeEdgePair(e);
                     --  }
                  end if;

                  if pendingTail0 /= null then
                     if toPrev0 /= null then
                        pendingHead0.link (toPrev0);
                     else
                        pendingHead0.link (min0.next);
                        firstNew0 := pendingHead0;
                     end if;

                     min0.link (pendingTail0);
                     pendingHead0 := null;
                     pendingTail0 := null;

                  elsif toPrev0 = null then
                     firstNew0 := min0;
                  end if;

                  prevPoint := c0.point;
                  c0        := e0.target;
                  toPrev0   := e0.m_reverse;
               end if;
            end if;


            if         c0 = first0
              and then c1 = first1
            then
               if toPrev0 = null then
                  pendingHead0.link (pendingTail0);
                  c0.edges := pendingTail0;
               else
                  e := toPrev0.prev;
                  n := null;

                  while e /= firstNew0
                  loop
                     n := e.prev;
                     Self.removeEdgePair (e);
                     e := n;
                  end loop;

                  --  for (Edge* e = toPrev0.prev, *n = NULL; e != firstNew0; e = n)
                  --  {
                  --        n := e.prev;
                  --        removeEdgePair (e);
                  --  }

                  if pendingTail0 /= null  then
                     pendingHead0.link (toPrev0);
                     firstNew0   .link (pendingTail0);
                  end if;
               end if;

               if toPrev1 = null then
                  pendingTail1.link (pendingHead1);
                  c1.edges := pendingTail1;
               else
                  e := toPrev1.next;
                  n := null;

                  while e /= firstNew1
                  loop
                     n := e.next;
                     Self.removeEdgePair (e);
                     e := n;
                  end loop;

                  --  for (Edge* e = toPrev1.next, *n = NULL; e != firstNew1; e = n)
                  --  {
                  --        n := e.next;
                  --        removeEdgePair (e);
                  --  }

                  if pendingTail1 /= null then
                     toPrev1     .link (pendingHead1);
                     pendingTail1.link (firstNew1);
                  end if;
               end if;

               return;
            end if;

            firstRun := False;
         end loop;
      end;

   end merge;








   procedure computeInternal (Self : in out Internal'Class;   start, finish : in     Integer;
                                                                          result        :    access IntermediateHull)
   is
      n : constant Integer := finish - start;
   begin
      if n = 0 then
         result.minXy := null;
         result.maxXy := null;
         result.minYx := null;
         result.maxYx := null;

         return;
      end if;


      if n = 2 then
         declare
            use vertex_Pointers;

            v : Vertex_view := Self.originalVertices.Element (start);
            w : Vertex_view := Vertex_view (vertex_Pointer (v) + 1);
            t : Vertex_view;
            e : internal_Edge_view;
            dx, dy : int32_t;
         begin
            if v.point /= w.point then
               dx := v.point.x - w.point.x;
               dy := v.point.y - w.point.y;

               if dx = 0 and then dy = 0 then

                  if v.point.z > w.point.z then
                     t := w;
                     w := v;
                     v := t;
                  end if;

                  pragma Assert (v.point.z < w.point.z);

                  v.next := v;
                  v.prev := v;
                  result.minXy := v;
                  result.maxXy := v;
                  result.minYx := v;
                  result.maxYx := v;

               else
                  v.next := w;
                  v.prev := w;
                  w.next := v;
                  w.prev := v;

                  if dx < 0 or else (dx = 0 and then dy < 0) then
                     result.minXy := v;
                     result.maxXy := w;
                  else
                     result.minXy := w;
                     result.maxXy := v;
                  end if;

                  if dy < 0 or else (dy = 0 and then dx < 0) then
                     result.minYx := v;
                     result.maxYx := w;
                  else
                     result.minYx := w;
                     result.maxYx := v;
                  end if;
               end if;

               e := Self.newEdgePair (v, w);
               e.link (e);
               v.edges := e;

               e := e.m_reverse;
               e.link (e);
               w.edges := e;

               return;
            end if;
         end;
      end if;


      if n = 1 or else n = 2 then      -- lint -fallthrough
         declare
            v : constant Vertex_view := Self.originalVertices.Element (start);
         begin
            v.edges := null;
            v.next := v;
            v.prev := v;

            result.minXy := v;
            result.maxXy := v;
            result.minYx := v;
            result.maxYx := v;

            return;
         end;
      end if;


      declare
         split0 : constant Integer := start + n / 2;
         p      : constant Point32 := Self.originalVertices.Element (split0 - 1).point;
         split1 : Integer := split0;

         hull1  : aliased IntermediateHull;
      begin
         while split1 < finish and then (Self.originalVertices.Element (split1).point = p)
         loop
            split1 := split1 + 1;
         end loop;

         Self.computeInternal (start,  split0, result);
         Self.computeInternal (split1, finish, hull1'Access);

         merge (Self,  result, hull1'Access);
      end;

   end computeInternal;











   function  mergeProjection (Self : access Internal'Class;   h0, h1 : access IntermediateHull;
                                                                    c0, c1 : access Vertex_view   ) return Boolean
   is
      pragma Unreferenced (Self);
      v0   : Vertex_view := h0.maxYx;
      v1   : Vertex_view := h1.minYx;

      v00,
      v10  : Vertex_view;

      sign : int32_t := 1;
   begin

      if         v0.point.x = v1.point.x
        and then v0.point.y = v1.point.y
      then
         declare
            pragma Assert (v0.point.z < v1.point.z);

            v1p : constant Vertex_view := v1.prev;
            v1n : Vertex_view;
         begin
            if v1p = v1 then
               c0.all := v0;

               if v1.edges /= null then
                  pragma Assert (v1.edges.next = v1.edges);
                  v1 := v1.edges.target;
                  pragma Assert (v1.edges.next = v1.edges);
               end if;

               c1.all := v1;
               return False;
            end if;


            v1n      := v1.next;
            v1p.next := v1n;
            v1n.prev := v1p;

            if v1 = h1.minXy then
               if (v1n.point.x < v1p.point.x) or else ((v1n.point.x = v1p.point.x) and then (v1n.point.y < v1p.point.y)) then
                  h1.minXy := v1n;
               else
                  h1.minXy := v1p;
               end if;
            end if;

            if v1 = h1.maxXy then
               if (v1n.point.x > v1p.point.x) or else ((v1n.point.x = v1p.point.x) and then (v1n.point.y > v1p.point.y)) then
                  h1.maxXy := v1n;
               else
                  h1.maxXy := v1p;
               end if;
            end if;
         end;
      end if;


      v0 := h0.maxXy;
      v1 := h1.maxXy;

      for side in 1 .. 2 loop
         declare
            dx        : int32_t := (v1.point.x - v0.point.x) * sign;
            dy        : int32_t;

            w0, w1    : Vertex_view;

            dx0, dy0,
            dx1, dy1,
            dxn       : int32_t;

            continue  : Boolean := False;   -- for simulated C 'continue' statement.
         begin

            if dx > 0 then
               loop
                  dy := v1.point.y - v0.point.y;

                  if side = 2 then
                     w0 := v0.next;
                  else
                     w0 := v0.prev;
                  end if;


                  if w0 /= v0 then
                     dx0 := (w0.point.x - v0.point.x) * sign;
                     dy0 :=  w0.point.y - v0.point.y;

                     if          dy0 <= 0
                       and then (       dx0 = 0
                                 or else (       dx0      <  0
                                          and then dy0 * dx <= dy * dx0))
                     then
                        v0       := w0;
                        dx       := (v1.point.x - v0.point.x) * sign;
                        continue := True;
                     end if;
                  end if;


                  if not continue then

                     if side = 2 then
                        w1 := v1.next;
                     else
                        w1 := v1.prev;
                     end if;


                     if w1 /= v1 then
                        dx1 := (w1.point.x - v1.point.x) * sign;
                        dy1 :=  w1.point.y - v1.point.y;
                        dxn := (w1.point.x - v0.point.x) * sign;

                        if         dxn > 0
                          and then dy1 < 0
                          and then (      dx1 = 0
                                    or else (       dx1      < 0
                                             and then dy1 * dx < dy * dx1))
                        then
                           v1 := w1;
                           dx := dxn;
                           continue := True;
                        end if;
                     end if;

                     if not continue then
                        exit;
                     end if;

                  end if;

               end loop;

            elsif dx < 0 then
               loop
                  dy := v1.point.y - v0.point.y;

                  if side = 2 then
                     w1 := v1.prev;
                  else
                     w1 := v1.next;
                  end if;

                  if w1 /= v1 then
                     dx1 := (w1.point.x - v1.point.x) * sign;
                     dy1 := w1.point.y - v1.point.y;
                     if (dy1 >= 0) and then ((dx1 = 0) or else ((dx1 < 0) and then (dy1 * dx <= dy * dx1))) then
                        v1 := w1;
                        dx := (v1.point.x - v0.point.x) * sign;
                        continue := True;
                     end if;
                  end if;

                  if not continue then

                     if side = 2 then
                        w0 := v0.prev;
                     else
                        w0 := v0.next;
                     end if;

                     if w0 /= v0 then
                        dx0 := (w0.point.x - v0.point.x) * sign;
                        dy0 := w0.point.y - v0.point.y;
                        dxn := (v1.point.x - w0.point.x) * sign;

                        if         dxn < 0
                          and then dy0 > 0
                          and then (      dx0 = 0
                                    or else (       dx0      < 0
                                             and then dy0 * dx < dy * dx0))
                        then
                           v0       := w0;
                           dx       := dxn;
                           continue := True;
                        end if;
                     end if;

                     if not continue then
                        exit;
                     end if;

                  end if;

               end loop;

            else
               declare
                  x  : constant int32_t := v0.point.x;
                  y0 : int32_t := v0.point.y;
                  y1 : int32_t;
                  t  : Vertex_view;
               begin
                  w0 := v0;

                  if side = 2 then
                     t := w0.next;
                  else
                     t := w0.prev;
                  end if;


                  while      t         /= v0
                    and then t.point.x  = x
                    and then t.point.y <= y0
                  loop
                     w0 := t;
                     y0 := t.point.y;
                  end loop;

                  v0 := w0;

                  y1 := v1.point.y;
                  w1 := v1;

                  if side = 2 then
                     t := w1.prev;
                  else
                     t := w1.next;
                  end if;


                  while      t         /= v1
                    and then t.point.x  = x
                    and then t.point.y >= y1
                  loop
                     w1 := t;
                     y1 := t.point.y;
                  end loop;

                  v1 := w1;
               end;
            end if;

            if side = 0 then
               v00  := v0;
               v10  := v1;

               v0   := h0.minXy;
               v1   := h1.minXy;
               sign := -1;
            end if;
         end;
      end loop;


      v0.prev := v1;
      v1.next := v0;

      v00.next := v10;
      v10.prev := v00;

      if h1.minXy.point.x < h0.minXy.point.x then
         h0.minXy := h1.minXy;
      end if;

      if h1.maxXy.point.x >= h0.maxXy.point.x then
         h0.maxXy := h1.maxXy;
      end if;

      h0.maxYx := h1.maxYx;

      c0.all := v00;
      c1.all := v10;

      return True;
   end mergeProjection;










   function  toBtVector (Self : in Internal;   v : in Point32) return math.Vector_3;

   function  toBtVector (Self : in Internal;   v : in Point32) return math.Vector_3
   is
      use math.Algebra.linear.d3;
      p : Vector_3;

   begin
      p (Self.medAxis) := Real (v.x);
      p (Self.maxAxis) := Real (v.y);
      p (Self.minAxis) := Real (v.z);

      return impact.d3.vector.Scaled (p, by => Self.scaling);
   end toBtVector;











   function  getBtNormal (Self : in Internal;   for_face : in Face) return math.Vector_3;


   function  getBtNormal (Self : in Internal;   for_face : in Face) return math.Vector_3
   is
      use math.Algebra.linear.d3,  impact.d3.Vector;
   begin
      return normalized (toBtVector (Self, for_Face.dir0) *  toBtVector (Self, for_Face.dir1));   -- * <-. 'cross product'
   end getBtNormal;










   function  shiftFace (Self : access Internal;   the_face : access Face;
                                                              amount   : in     math.Real;
                                                              use_stack    : in     vertex_Vector) return Boolean;




   function  shiftFace (Self : access Internal;   the_face : access Face;
                        amount   : in     math.Real;
                        use_stack    : in     vertex_Vector) return Boolean
   is
      use math.Vectors, ada.Containers;

      the_Stack : vertex_Vector := use_stack;

      origShift : Vector_3 := getBtNormal (Self.all,  the_face.all)  *  (-amount);
      shift     : Point32;

      normal        : Point64;
      origDot,
      shiftedDot    : int64_t;
      shiftedOrigin : Point32;


      intersection,
      startEdge    : internal_Edge_view;
      optDot       : Rational128;
      cmp          : Integer;

      e            : internal_Edge_view;

      firstIntersection,
      faceEdge,
      firstFaceEdge : internal_Edge_view;

   begin
      if Self.scaling (1) /= 0.0 then   origShift (1) := origShift (1) / Self.scaling (1);   end if;
      if Self.scaling (2) /= 0.0 then   origShift (2) := origShift (2) / Self.scaling (2);   end if;
      if Self.scaling (3) /= 0.0 then   origShift (3) := origShift (3) / Self.scaling (3);   end if;


      shift := to_Point32 (int32_t (origShift (Self.medAxis)),
                           int32_t (origShift (Self.maxAxis)),
                           int32_t (origShift (Self.minAxis)));

      if isZero (shift) then
         return True;
      end if;

      normal        := getNormal (the_face.all);
      origDot       := the_face.origin.dot (normal);
      shiftedOrigin := the_face.origin + shift;
      shiftedDot    := shiftedOrigin.dot (normal);     pragma Assert (shiftedDot <= origDot);

      if shiftedDot >= origDot then
         return False;
      end if;


      startEdge := the_face.nearbyVertex.edges;
      optDot    := dot (the_face.nearbyVertex.all,  normal);
      cmp       := compare (optDot, shiftedDot);

      if cmp >= 0 then
         e := startEdge;

         loop
            declare
               dot : constant Rational128 := convex_hull_Computer.dot (e.target.all, normal);     pragma Assert (compare (dot, origDot)  <=  0);
               c   : Integer;
            begin
               if compare (dot, optDot)  <  0 then
                  c := compare (dot, shiftedDot);

                  optDot    := dot;
                  e         := e.m_reverse;
                  startEdge := e;

                  if c < 0 then
                     intersection := e;
                     exit;
                  end if;

                  cmp := c;
               end if;
            end;

            e := e.prev;
            exit when e = startEdge;
         end loop;


         if  intersection = null then
            return False;
         end if;
      else
         e := startEdge;

         loop
            declare
               dot : constant Rational128 := convex_hull_Computer.dot (e.target.all, normal);     pragma Assert (compare (dot, origDot)  <=  0);
            begin
               if compare (dot, optDot) > 0 then

                  cmp := compare (dot, shiftedDot);

                  if cmp >= 0 then
                     intersection := e;
                     exit;
                  end if;

                  optDot    := dot;
                  e         := e.m_reverse;
                  startEdge := e;
               end if;
            end;

            e := e.prev;
            exit when e = startEdge;
         end loop;

         if intersection = null then
            return True;
         end if;
      end if;


      if cmp = 0 then
         e := intersection.m_reverse.next;

         while compare (dot (e.target.all, normal),  shiftedDot)  <=  0
         loop
            e := e.next;

            if e = intersection.m_reverse then
               return True;
            end if;
         end loop;
      end if;



      loop
         if cmp = 0 then
            e := intersection.m_reverse.next;

            startEdge := e;

            loop
               if compare (dot (e.target.all, normal), shiftedDot) >= 0 then
                  exit;
               end if;

               intersection := e.m_reverse;
               e            := e.next;

               if e = startEdge then
                  return True;
               end if;
            end loop;
         end if;



         if firstIntersection = null then
            firstIntersection := intersection;

         elsif intersection = firstIntersection then
            exit;
         end if;


         declare
            prevCmp          : constant Integer   := cmp;
            prevIntersection : constant internal_Edge_view := intersection;
            prevFaceEdge     : constant internal_Edge_view := faceEdge;

            removed : Vertex_view;
         begin
            e := intersection.m_reverse;

            loop
               e   := e.m_reverse.prev;                            pragma Assert (e /= intersection.m_reverse);
               cmp := compare (dot (e.target.all,  normal),  shiftedDot);

               if cmp >= 0 then
                  intersection := e;
                  exit;
               end if;
            end loop;


            if cmp > 0 then

               removed := intersection.target;

               e := intersection.m_reverse;

               if e.prev = e then
                  removed.edges := null;
               else
                  removed.edges := e.prev;
                  e.prev.link (e.next);
                  e     .link (e);
               end if;

               declare
                  n0 : constant Point64 := getNormal (intersection.face.all);
                  n1 : constant Point64 := getNormal (intersection.m_reverse.face.all);

                  m00 : constant int64_t := the_face.dir0.dot (n0);
                  m01 : constant int64_t := the_face.dir1.dot (n0);
                  m10 : constant int64_t := the_face.dir0.dot (n1);
                  m11 : constant int64_t := the_face.dir1.dot (n1);

                  r0 : constant int64_t := dot ((intersection          .face.origin - shiftedOrigin),  n0);
                  r1 : constant int64_t := dot ((intersection.m_reverse.face.origin - shiftedOrigin),  n1);

                  det : constant Int128 :=    mul (m00, m11) -  mul (m01, m10);     pragma Assert (det.getSign /= 0);

                  v : constant Vertex_view := Self.vertexPool.newObject.all'Access;

               begin
                  v.point.index := -1;
                  v.copy        := -1;

                  v.point128 := PointR128'(x =>
                                              mul (int64_t (the_face.dir0.x) * r0,  m11)  -  mul (int64_t (the_face.dir0.x) * r1,  m01)
                                            + mul (int64_t (the_face.dir1.x) * r1,  m00)  -  mul (int64_t (the_face.dir1.x) * r0,  m10)
                                            + det * int64_t (shiftedOrigin.x),

                                            y =>
                                              mul (int64_t (the_face.dir0.y) * r0,  m11)  -  mul (int64_t (the_face.dir0.y) * r1,  m01)
                                            + mul (int64_t (the_face.dir1.y) * r1,  m00)  -  mul (int64_t (the_face.dir1.y) * r0,  m10)
                                            + det * int64_t (shiftedOrigin.y),

                                            z =>
                                              mul (int64_t (the_face.dir0.z) * r0,  m11)  -  mul (int64_t (the_face.dir0.z) * r1,  m01)
                                            + mul (int64_t (the_face.dir1.z) * r1,  m00)  -  mul (int64_t (the_face.dir1.z) * r0,  m10)
                                            + det * int64_t (shiftedOrigin.z),

                                            denominator =>  det);

                  v.point.x := int32_t (v.point128.xvalue);
                  v.point.y := int32_t (v.point128.yvalue);
                  v.point.z := int32_t (v.point128.zvalue);

                  intersection.target := v;
                  v.edges             := e;

                  the_stack.append (v);
                  the_stack.append (removed);
                  the_stack.append (null);
               end;
            end if;


            if        cmp     /= 0
              or else prevCmp /= 0
              or else (prevIntersection.m_reverse.next.target /= intersection.target)
            then
               faceEdge := Self.newEdgePair (prevIntersection.target, intersection.target);

               if prevCmp = 0 then
                  faceEdge.link (prevIntersection.m_reverse.next);
               end if;

               if prevCmp = 0 or else prevFaceEdge /= null then
                  prevIntersection.m_reverse.link (faceEdge);
               end if;

               if cmp = 0 then
                  intersection.m_reverse.prev.link (faceEdge.m_reverse);
               end if;

               faceEdge.m_reverse.link (intersection.m_reverse);
            else
               faceEdge := prevIntersection.m_reverse.next;
            end if;


            if prevFaceEdge /= null then

               if prevCmp > 0 then
                  faceEdge.link (prevFaceEdge.m_reverse);

               elsif faceEdge /= prevFaceEdge.m_reverse then

                  the_stack.append (prevFaceEdge.target);

                  while faceEdge.next /= prevFaceEdge.m_reverse
                  loop
                     declare
                        removed : constant Vertex_view := faceEdge.next.target;
                     begin
                        Self.removeEdgePair (faceEdge.next);
                        the_stack.append (removed);
                     end;
                  end loop;

                  the_stack.append (null);
               end if;
            end if;

            faceEdge          .face := the_face;
            faceEdge.m_reverse.face := intersection.face;

            if  firstFaceEdge = null then
               firstFaceEdge := faceEdge;
            end if;
         end;
      end loop;



      if cmp > 0 then
         firstFaceEdge.m_reverse.target := faceEdge.target;

         firstIntersection.m_reverse.link (firstFaceEdge);
         firstFaceEdge              .link (faceEdge.m_reverse);

      elsif firstFaceEdge /= faceEdge.m_reverse then

         the_stack.append (faceEdge.target);

         while firstFaceEdge.next /= faceEdge.m_reverse
         loop
            declare
               removed : constant Vertex_view := firstFaceEdge.next.target;
            begin
               Self.removeEdgePair (firstFaceEdge.next);
               the_stack.append   (removed);
            end;
         end loop;

         the_stack.append (null);
      end if;


      pragma Assert (the_stack.Length > 0);

      Self.vertexList := the_stack.Element (1);


      declare
         pos     : Integer := 0;
         the_end : Integer;

         kept,
         removed : Vertex_view;
         deeper  : Boolean;

      begin
         while pos < Integer (the_Stack.Length)
         loop
            the_end := Integer (the_Stack.Length);

            while pos < the_end
            loop
               pos    := pos + 1;
               kept   := the_Stack.Element (pos);
               deeper := False;

               pos     := pos + 1;
               removed := the_Stack.Element (pos);

               while removed /= null
               loop
                  receiveNearbyFaces (kept, removed);

                  while removed.edges /= null
                  loop
                     if not deeper then
                        deeper := True;
                        the_stack.append (kept);
                     end if;

                     the_stack.append    (removed.edges.target);
                     Self.removeEdgePair (removed.edges);
                  end loop;
               end loop;

               if deeper then
                  the_stack.append (null);
               end if;
            end loop;
         end loop;
      end;


      the_stack.clear;
      the_face.origin := shiftedOrigin;

      return True;

   end shiftFace;









   --- compute
   --


   function pointCmp (p, q : in Point32) return Boolean
   is
   begin
      return     p.y < q.y
        or else (        p.y = q.y
                 and then (       p.x < q.x
                           or else (       p.x = q.x
                                    and then p.z < q.z)));
   end pointCmp;


   package Conversions is new system.Address_to_Access_Conversions (c.unsigned_Char);



   procedure compute (Self : in out Internal;   coords       : in     system.Address;
                                                            doubleCoords : in     Boolean;
                                                            stride       : in     Integer;
                                                            count        : in     Integer);


   procedure compute (Self : in out Internal;   coords       : in     system.Address;
                                                            doubleCoords : in     Boolean;
                                                            stride       : in     Integer;
                                                            count        : in     Integer)
   is
      use impact.d3.Vector,  swig.Pointers;

      function to_double_Pointer is new ada.unchecked_Conversion (c_unsigned_char_Pointers.pointer, swig.pointers.double_Pointer);
      function to_float_Pointer  is new ada.unchecked_Conversion (c_unsigned_char_Pointers.pointer, swig.pointers.float_Pointer);

      min : Vector_3 := (1.0e30,  1.0e30,  1.0e30);
      max : Vector_3 := (-1.0e30, -1.0e30, -1.0e30);

      ptr : c_unsigned_char_Pointers.pointer := conversions.to_Pointer (coords).all'Access;
--        ptr : access interfaces.c.char := coords;
      --          const char* ptr = (const char*) coords;

      s : Vector_3;
   begin
      if doubleCoords then
         for i in 1 .. count
         loop
            declare
               use swig.pointers.c_double_Pointers,  c_unsigned_char_Pointers;

               v : constant swig.pointers.double_Pointer := to_double_Pointer (ptr);   -- const double* v = (const double*) ptr;
               p : constant Vector_3              := (Real (double_Pointer (v + 0).all),
                                             Real (double_Pointer (v + 1).all),
                                             Real (double_Pointer (v + 2).all));
            begin
               ptr := ptr + C.ptrdiff_t (stride);
               setMin (min, p);
               setMax (max, p);
            end;
         end loop;

      else

         for i in 1 .. count
         loop
            declare
               use swig.pointers.c_float_Pointers,  c_unsigned_char_Pointers;

               v : constant swig.pointers.float_Pointer := to_float_Pointer (ptr);
               p : constant Vector_3              := (Real (float_Pointer (v + 0).all),
                                             Real (float_Pointer (v + 1).all),
                                             Real (float_Pointer (v + 2).all));
--                 p : bullet.Vector_3             := ((v+0), (v+1), (v+2));
            begin
               ptr := ptr + C.ptrdiff_t (stride);
               setMin (min, p);
               setMax (max, p);
            end;
         end loop;
      end if;


      s := max - min;

      Self.maxAxis := maxAxis (s);
      Self.minAxis := minAxis (s);


      if Self.minAxis = Self.maxAxis then
         Self.minAxis := (Self.maxAxis + 1) mod 3;
      end if;

      Self.medAxis := 3 - Self.maxAxis - Self.minAxis;

      s := s / 10216.0;

      if ((Self.medAxis + 1) mod 3) /= Self.maxAxis then
         s := -s;
      end if;

      Self.scaling := s;

      if s (1) /= 0.0 then
         s (1) := 1.0 / s (1);
      end if;

      if s (2) /= 0.0 then
         s (2) := 1.0 / s (2);
      end if;

      if s (3) /= 0.0 then
         s (3) := 1.0 / s (3);
      end if;

      Self.center := (min + max) * 0.5;


      declare
         type point_array is array (Positive range <>) of Point32;
         points : point_array (1 .. Count);

         procedure sort is new ada.containers.Generic_Array_Sort (Positive, Point32, point_array, pointCmp);

--           btAlignedObjectArray<Point32>    points;
      begin
--           points.resize (count);

--           ptr := (const char*) coords;
         ptr := conversions.to_Pointer (coords).all'Access;


         if doubleCoords then

            for i in 1 .. count
            loop
               declare
                  use swig.pointers.c_double_Pointers,  c_unsigned_char_Pointers;

                  v : constant swig.pointers.double_Pointer := to_double_Pointer (ptr);   -- const double* v := (const double*) ptr;
                  p : Vector_3                     := (Real (double_Pointer (v + 0).all),
                                                       Real (double_Pointer (v + 1).all),
                                                       Real (double_Pointer (v + 2).all));
               begin
                  ptr              := ptr + C.ptrdiff_t (stride);
                  p                := Scaled (p - Self.center, by => s);
                  points (i).x     :=  Integer_32 (p (Self.medAxis));
                  points (i).y     :=  Integer_32 (p (Self.maxAxis));
                  points (i).z     :=  Integer_32 (p (Self.minAxis));
                  points (i).index := i;
               end;

            end loop;

         else

            for i in 1 .. count
            loop
               declare
                  use math.algebra.linear.d3, swig.pointers.c_float_Pointers, c_unsigned_char_Pointers;
                  v : constant swig.pointers.float_Pointer := to_float_Pointer (ptr);     -- const float* v = (const float*) ptr;
                  p : Vector_3                    := (Real (float_Pointer (v + 0).all),
                                                      Real (float_Pointer (v + 1).all),
                                                      Real (float_Pointer (v + 2).all));
               begin
                  ptr              := ptr + C.ptrdiff_t (stride);
                  p                := Scaled (p - Self.center, by => s);
                  points (i).x     :=  Integer_32 (p (Self.medAxis));
                  points (i).y     :=  Integer_32 (p (Self.maxAxis));
                  points (i).z     :=  Integer_32 (p (Self.minAxis));
                  points (i).index := i;
               end;
            end loop;

         end if;


         sort (points);

         Self.vertexPool.reset;
         Self.vertexPool.setArraySize (count);
         Self.originalVertices.set_Length (ada.containers.Count_type (count));

         for i in 1 .. count
         loop
            declare
               v : constant Vertex_view := Self.vertexPool.newObject.all'Access;
            begin
               v.edges := null;
               v.point := points (i);
               v.copy  := -1;
               Self.originalVertices.replace_Element (i, v);
            end;
         end loop;

         Self.edgePool.reset;
         Self.edgePool.setArraySize (6 * count);

         Self.usedEdgePairs    := 0;
         Self.maxUsedEdgePairs := 0;

         Self.mergeStamp := -3;

         declare
            hull : aliased IntermediateHull;
         begin
            Self.computeInternal (0, count, hull'Access);
            Self.vertexList := hull.minXy;
         end;
      end;


   end compute;










   function  getCoordinates (Self : in Internal;   for_vertex : in Vertex) return math.Vector_3;


   function  getCoordinates (Self : in Internal;   for_vertex : in Vertex) return math.Vector_3
   is
      use math.Vectors, math.Algebra.linear.d3;
      p : Vector_3;
   begin
      p (Self.medAxis) := xvalue (for_Vertex);
      p (Self.maxAxis) := yvalue (for_Vertex);
      p (Self.minAxis) := zvalue (for_Vertex);

      return impact.d3.vector.Scaled (p, by => Self.scaling) + Self.center;
   end getCoordinates;






   function getVertexCopy (vertex : Vertex_view;   vertices : access vertex_Vector) return Integer
   is
      index : Integer := vertex.copy;

   begin
      if index < 0 then
         vertices.append (vertex);

         index       := Integer (vertices.Length);
         vertex.copy := index;
      end if;

      return index;
   end getVertexCopy;









   function  shrink (Self : access Internal;   amount      : in math.Real;
                                                           clampAmount : in math.Real) return math.Real;

   function  shrink (Self : access Internal;   amount      : in math.Real;
                     clampAmount : in math.Real) return math.Real
   is
      use ada.Containers, vertex_Vectors;

      stamp : Integer;
      stack : vertex_Vector;
      faces : face_Vector;

      ref          : Point32;
      hullCenterX,
      hullCenterY,
      hullCenterZ,
      volume       : Int128;
   begin

      if Self.vertexList = null then
         return 0.0;
      end if;

      Self.mergeStamp := Self.mergeStamp - 1;
      stamp           := Self.mergeStamp;

      Self.vertexList.copy := stamp;
      stack.append (Self.vertexList);

      ref         := Self.vertexList.point;
      hullCenterX := (0, 0);
      hullCenterY := (0, 0);
      hullCenterZ := (0, 0);
      volume      := (0, 0);


      while stack.Length > 0
      loop
         declare
            v : constant Vertex_view        := stack.last_Element;     --    (stack.Length - 1);
            e : internal_Edge_view := v.edges;
         begin
            stack.delete_Last;

            if e /= null then

               loop
                  if e.target.copy /= stamp then
                     e.target.copy := stamp;
                     stack.append (e.target);
                  end if;

                  if e.copy /= stamp then
                     declare
                        face : constant Face_view := Self.facePool.newObject.all'Access;
                        f    : internal_Edge_view := e;
                        a, b : Vertex_view;
                     begin
                        init (face,  e.target,  e.m_reverse.prev.target,  v);
                        faces.append (face);

                        loop
                           if         a /= null
                             and then b  /= null
                           then
                              declare
                                 vol : constant int64_t := dot (v.point - ref,
                                                       cross (a.point - ref,  b.point - ref));
                                 pragma Assert (vol >= 0);

                                 c   : constant Point32 := v.point + a.point + b.point + ref;
                              begin
                                 hullCenterX := hullCenterX + to_Int128 (vol * Int64_t (c.x));
                                 hullCenterY := hullCenterY + to_Int128 (vol * Int64_t (c.y));
                                 hullCenterZ := hullCenterZ + to_Int128 (vol * Int64_t (c.z));
                                 volume      := volume + to_Int128 (vol);
                              end;
                           end if;

                           pragma Assert (f.copy /= stamp);

                           f.copy := stamp;
                           f.face := face;

                           a := b;
                           b := f.target;

                           f := f.m_reverse.prev;

                           exit when f = e;
                        end loop;
                     end;
                  end if;

                  e := e.next;

                  exit when e = v.edges;
               end loop;
            end if;
         end;
      end loop;


      if volume.getSign <= 0 then
         return 0.0;
      end if;


      declare
         use math.Vectors, Math, math.Algebra.linear;
         use type C.unsigned;
         hullCenter : Vector_3;
         faceCount  : Integer;
         minDist    : Real;

         seed       : C.unsigned;

         the_Amount : Real := Amount;
      begin
         hullCenter (Self.medAxis) := hullCenterX.toScalar;
         hullCenter (Self.maxAxis) := hullCenterY.toScalar;
         hullCenter (Self.minAxis) := hullCenterZ.toScalar;

         hullCenter := hullCenter / (4.0 * volume.toScalar);
         hullCenter := Vector_3 (Scaled (math.Vector (hullCenter),
                                         by => math.Vector (Self.scaling)));

         faceCount := Integer (faces.Length);


         if clampAmount > 0.0 then
            minDist := impact.d3.Scalar.SIMD_INFINITY;

            for i in 1 .. faceCount
            loop
               declare
                  normal : constant Vector_3 := getBtNormal (Self.all,  faces.Element (i).all);
                  dist   : constant Real   := normal  *  (toBtVector (Self.all, faces.Element (i).origin)  -  hullCenter);
               begin

                  if dist < minDist then
                     minDist := dist;
                  end if;
               end;
            end loop;

            if minDist <= 0.0 then
               return 0.0;
            end if;

            the_Amount := Real'Min (the_Amount,  minDist * clampAmount);
         end if;


         seed := 243703;

         for i in 1 .. faceCount
         loop
            faces.swap (i,  Positive (seed mod c.unsigned (faceCount)));     --       btSwap (faces (i),  faces (seed mod faceCount));
            seed := 1664525 * seed + 1013904223;
         end loop;


         for i in 1 .. faceCount
         loop
            if not shiftFace (Self,  faces.Element (i),  amount,  stack) then
               return -amount;
            end if;
         end loop;

         return amount;
      end;

   end shrink;








   --  tbd: <critical> check indices in arrays and vectors below !!!


   function compute (Self : access Item'Class;   coords       : in     system.Address;
                     doubleCoords : in     Boolean;
                     stride       : in     Integer;
                     count        : in     Integer;
                     the_shrink   : in     math.Real;
                     shrinkClamp  : in     math.Real) return math.Real
   is
      hull  : aliased Internal;
      shift : Real;

      oldVertices : aliased vertex_Vector;
      copied      : Integer;

      unused : Integer;
      pragma Unreferenced (unused);

   begin
      if count <= 0 then
         Self.vertices.clear;
         Self.edges.clear;
         Self.faces.clear;

         return 0.0;
      end if;


      compute (hull, coords, doubleCoords, stride, count);

      shift := 0.0;

      if the_shrink > 0.0 then
         shift := shrink (hull'Access,  the_shrink, shrinkClamp);

         if shift < 0.0 then
            Self.vertices.clear;
            Self.edges.clear;
            Self.faces.clear;

            return shift;
         end if;
      end if;


      --          Self.vertices.resize (0);
      --          Self.edges.resize (0);
      --          Self.faces.resize (0);

      Self.vertices.clear;
      Self.edges   .clear;
      Self.faces   .clear;

      unused := getVertexCopy (hull.vertexList,  oldVertices'Access);


      copied := 0;

      while copied < Integer (oldVertices.Length)
      loop
         declare
            v         : constant Vertex_view := oldVertices.Element (copied);

            firstEdge,
            e         : internal_Edge_view;

            r      : constant Edge_view := new Edge;
            c : constant Edge_view := new Edge;

            firstCopy,
            prevCopy,
            s         : Integer;
         begin
            Self.vertices.append (getCoordinates (hull, v.all));

            firstEdge := v.edges;

            if firstEdge /= null then

               firstCopy := -1;
               prevCopy  := -1;

               e := firstEdge;

               loop
                  if e.copy < 0 then

                     s := Integer (Self.edges.Length);

                     --                                             c := &edges[s];
                     --                                             r := &edges[s + 1];

                     e.copy           := s;
                     e.m_reverse.copy := s + 1;
                     c.m_reverse      := 1;
                     r.m_reverse      := -1;
                     c.targetVertex   := getVertexCopy (e.target, oldVertices'Access);
                     r.targetVertex   := copied;

                     Self.edges.append (c);
                     Self.edges.append (r);
                  end if;


                  if prevCopy >= 0 then
                     declare
                        the_Edge : constant Edge_view := Self.edges.Element (e.copy);
                     begin
                        the_Edge.next := prevCopy - e.copy;
                     end;
                  else
                     firstCopy := e.copy;
                  end if;


                  prevCopy := e.copy;
                  e        := e.next;

                  exit when e = firstEdge;
               end loop;


               declare
                  the_Edge : constant Edge_view := Self.edges.Element (firstCopy);
               begin
                  the_Edge.next := prevCopy - firstCopy;
               end;

            end if;

            copied := copied + 1;
         end;
      end loop;



      for i in 1 .. copied
      loop
         declare
            v         : constant Vertex_view        := oldVertices.Element (i);
            firstEdge : constant internal_Edge_view := v.edges;
            e, f      : internal_Edge_view;
         begin

            if firstEdge /= null then

               e := firstEdge;

               loop
                  if e.copy >= 0 then

                     Self.faces.append (e.copy);
                     f := e;

                     loop
                        f.copy := -1;
                        f      := f.m_reverse.prev;

                        exit when f = e;
                     end loop;
                  end if;

                  e := e.next;
                  exit when e = firstEdge;
               end loop;
            end if;
         end;
      end loop;


      return shift;
   end compute;


end impact.d3.convex_hull_Computer;
