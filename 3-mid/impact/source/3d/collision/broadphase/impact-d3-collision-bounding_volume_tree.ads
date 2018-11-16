
with impact.d3.aabb_Util,
     ada.containers.Indefinite_Vectors,
     Interfaces;
with ada.containers.Vectors;



package impact.d3.collision.bounding_volume_Tree
--
--  The impact.d3.collision.bounding_volume_Tree class implements a fast dynamic bounding volume tree based on axis aligned bounding boxes (aabb tree).
--  This impact.d3.collision.bounding_volume_Tree is used for soft body collision detection and for the impact.d3.collision.bounding_volume_TreeBroadphase. It has a fast insert, remove and update of nodes.
--  Unlike the impact.d3.collision.quantized_Bvh, nodes can be dynamically moved around, which allows for change in topology of the underlying data structure.
--
--  Original implementation by Nathanael Presson.
--
is

   type Item is tagged limited private;
   type View is access all Item'Class;




   --- Defaults volumes
   --

   type Vector_3_view_array is array (Positive range <>) of access math.Vector_3;


   --  impact.d3.collision.bounding_volume_TreeAabbMm
   --
   type AabbMm is tagged private;


   function FromCE     (c,  e  : in     math.Vector_3                  )       return AabbMm;
   function FromCR     (c      : in     math.Vector_3;   r : in math.Real)       return AabbMm;
   function FromMM     (mi, mx : in     math.Vector_3)                           return AabbMm;
   function FromPoints (pts    : in     Vector_3_array)                          return AabbMm;
   function FromPoints (ppts   : in     Vector_3_view_array;   n : in Integer)   return AabbMm;


   function  Center  (Self : in AabbMm) return math.Vector_3;
   function  Lengths (Self : in AabbMm) return math.Vector_3;
   function  Extents (Self : in AabbMm) return math.Vector_3;

   function  Mins    (Self : in AabbMm) return math.Vector_3;
   function  Maxs    (Self : in AabbMm) return math.Vector_3;


   procedure Expand       (Self : in out AabbMm;   e : in math.Vector_3);
   procedure SignedExpand (Self : in out AabbMm;   e : in math.Vector_3);


   function  Contain      (Self : in AabbMm'Class;   a : in AabbMm'Class) return Boolean;

   function  Classify     (Self : in AabbMm;   n : in math.Vector_3;
                                                     o : in math.Real;
                                                     s : in Integer) return Integer;

   function  ProjectMinimum (Self : access AabbMm;   v     : in math.Vector_3;
                             signs : in interfaces.Unsigned_32) return math.Real;




   function  Intersect      (a, b : in AabbMm'Class)  return Boolean;

   function  Intersect      (a    : in AabbMm;
                             b    : in math.Vector_3) return Boolean;

   function  Proximity      (a, b : in AabbMm)  return math.Real;


   function  do_Select      (o, a, b : in AabbMm)  return Integer;


   procedure Merge          (a, b : in     AabbMm;
                             r    :    out AabbMm);

   function  NotEqual       (a, b : in     AabbMm)  return Boolean;






   --  impact.d3.collision.bounding_volume_TreeNode
   --

   type Node is tagged;



   subtype Volume     is AabbMm;

   --  Container
   --
   type    Node_view    is access all Node'Class;
   type    Node_views   is array (1 .. 2) of aliased Node_view;

   package Node_Vectors is new ada.containers.Vectors (Positive, Node_view);
   subtype Node_Vector  is     Node_Vectors.Vector;



--     type union_Kind is (use_childs, use_data, use_dataAsInt);
--
--     type Union (Kind : union_Kind := use_childs) is
--        record
--           case Kind
--           is
--              when use_childs    =>   childs    :        impact.d3.collision.bounding_volume_TreeNode_views;
--              when use_data      =>   data      : access bullet.Any'Class;
--              when use_dataAsInt =>   dataAsInt :        Integer;
--           end case;
--        end record;

   type Union is
      record
         childs    :        Node_views;
         data      : access Any'Class;
         dataAsInt :        Integer;
      end record;



   type Node is tagged
      record
         volume : aliased bounding_volume_Tree.Volume;
         parent : Node_view;

         state  : Union;
      end record;


   function  isleaf     (Self : in Node) return Boolean;
   function  isinternal (Self : in Node) return Boolean;









   --- Policies/Interfaces
   --

   --  ICollide
   --

   type ICollide is tagged null record;


   procedure destruct  (Self : in out ICollide)   is null;

   procedure Process   (Self : in out ICollide;   n1, n2 : access Node'Class)                   is null;
   procedure Process   (Self : in out ICollide;   n1     : access Node'Class)                   is null;
   procedure Process   (Self : in out ICollide;   n      : access Node'Class;   a : in math.Real);

   function  Descent   (Self : in     ICollide;   n      : in Node'Class) return Boolean;
   function  AllLeaves (Self : in     ICollide;   n      : in Node'Class) return Boolean;





   --  IWriter
   --
   type IWriter is abstract tagged null record;


   procedure destruct  (Self : in out IWriter)   is null;

   procedure Prepare   (Self : in out IWriter;   root     : in Node'Class;
                                                 numnodes : in Integer       ) is abstract;

   procedure WriteNode (Self : in out IWriter;   n        : in Node'Class;
                                                 index,
                                                 parent,
                                                 child0,
                                                 child1   : in Integer       ) is abstract;


   procedure WriteLeaf (Self : in out IWriter;   n        : in Node'Class;
                                                 index,
                                                 parent   : in Integer       ) is abstract;




   --  IClone
   --
   type IClone is tagged null record;

   procedure destruct  (Self : in out IClone)                              is null;
   procedure CloneLeaf (Self : in out IClone;   n : in Node'Class)   is null;






   --- impact.d3.collision.bounding_volume_Tree
   --

   procedure destruct (Self : in out Item);


   procedure clear (Self : in out Item);
   function  empty (Self : in     Item) return Boolean;

   procedure optimizeBottomUp    (Self : in out Item);
   procedure optimizeTopDown     (Self : in out Item;   bu_treshold : Integer := 128);
   procedure optimizeIncremental (Self : in out Item;   pass_Count : Integer);


   function  insert (Self : access Item;   volume : in     bounding_volume_Tree.Volume'Class;
                                           data   : access Any  'Class) return access Node'Class;


   procedure update (Self : in out Item;   leaf      : access Node'Class;
                                           lookahead : in     Integer         := -1);

   procedure update (Self : in out Item;   leaf      : access Node  'Class;
                                           volume    : in     bounding_volume_Tree.Volume'Class);

   function  update (Self : access Item;   leaf      : access Node  'Class;
                                           volume    : access bounding_volume_Tree.Volume'Class;
                                           velocity  : in     math.Vector_3;
                                           margin    : in     math.Real       ) return Boolean;

   function  update (Self : access Item;   leaf      : access Node  'Class;
                                           volume    : access bounding_volume_Tree.Volume'Class;
                     velocity  : in     math.Vector_3) return Boolean;

   function  update (Self : access Item;   leaf      : access Node  'Class;
                                          volume    : access bounding_volume_Tree.Volume'Class;
                     margin    : in     math.Real       ) return Boolean;


   procedure remove (Self : in out Item;   leaf      : access Node  'Class);


   procedure write (Self : in      Item;   iwriter   : access bounding_volume_Tree.IWriter'Class);
   procedure clone (Self : in      Item;   dest      : access Item'Class;
                                           iclone    : access impact.d3.collision.bounding_volume_Tree.IClone'Class := null);







   function  maxdepth (node : access bounding_volume_Tree.Node) return Integer;

   function  countLeaves   (node : in bounding_volume_Tree.Node) return Integer;
   procedure extractLeaves (node : access bounding_volume_Tree.Node;   leaves : out Node_Vector);







   --          DBVT_IPOLICY must support ICollide policy/interface
   --

   procedure enumNodes (root  : access Node;   policy : access ICollide'Class);
   procedure enumLeaves (root  : access Node;   policy : access ICollide'Class);

   procedure collideTT  (Self : in out Item;   root0, root1 : access Node'Class;
                                               policy       : access ICollide  'Class);


   procedure collideTTpersistentStack  (Self : in out Item;   root0, root1 : access Node'Class;
                                                              policy       : access ICollide  'Class);

   procedure collideTV (Self : in Item;   root   : access Node'Class;
                                          vol    : in     Volume'Class;
                                          policy : access ICollide    'Class);






   --  rayTest is a re-entrant ray test, and can be called in parallel as long as the btAlignedAlloc is thread-safe (uses locking etc)
   --  rayTest is slower than rayTestInternal, because it builds a local stack, using memory allocations, and it recomputes signs/rayDirectionInverses each time
   --
   procedure rayTest (root    : access Node;
                      rayFrom,
                      rayTo   : in     math.Vector_3;
                      policy  : access ICollide'Class);




   --  rayTestInternal is faster than rayTest, because it uses a persistent stack (to reduce dynamic memory allocations to a minimum) and it uses precomputed signs/rayInverseDirections
   --  rayTestInternal is used by impact.d3.collision.bounding_volume_TreeBroadphase to accelerate world ray casts
   --
   procedure rayTestInternal (Self : in out Item;   root                : access Node'Class;
                                                    rayFrom,
                                                    rayTo,
                                                    rayDirectionInverse : in     math.Vector_3;
                                                    signs               : in     impact.d3.aabb_Util.Signs;
                                                    lambda_max          : in     math.Real;
                                                    aabbMin, aabbMax    : in     math.Vector_3;
                              policy              : access ICollide'Class);






   procedure collideKDOP (root    : access Node;
                          normals : in     Vector_3_array;
                          offsets : in     math.Vector;
                          count   : in     Integer;
                          policy  : access ICollide'Class);




   procedure collideOCL (root     : access Node;
                          normals  : in     Vector_3_array;
                          offsets  : in     math.Vector;
                          sortaxis : in     math.Vector_3;
                          count    : in     Integer;
                          policy   : access ICollide'Class;
                         fullsort : in     Boolean       := True);



   procedure collideTU (root    : access Node;
                        policy  : access ICollide'Class);









   --  Stack element
   --

--     type sStkNN (a, b : access impact.d3.collision.bounding_volume_TreeNode) is null record;
   type sStkNN is
      record
         a, b : access Node;
      end record;

--     type sStkNP (node : access impact.d3.collision.bounding_volume_TreeNode) is
   type sStkNP is
      record
         node : access bounding_volume_Tree.Node;
         mask :        Flags;
      end record;

   type sStkNPS is
      record
         node  : access bounding_volume_Tree.Node;
         mask  :        Flags;
         value :        math.Real;
      end record;

--     type sStkCLN (node : access impact.d3.collision.bounding_volume_TreeNode) is
   type sStkCLN is
      record
         node   : access bounding_volume_Tree.Node;
         parent : access bounding_volume_Tree.Node;
      end record;



   type sStkNPS_array is array (Positive range <>) of sStkNPS;




   --  Helpers
   --

   package integer_Vectors is new ada.containers.Vectors (Positive, Integer);
   subtype integer_Vector  is     integer_Vectors.Vector;

   package sStkNPS_Vectors is new ada.containers.indefinite_Vectors (Positive, sStkNPS);
   subtype sStkNPS_Vector  is     sStkNPS_Vectors.Vector;

   package sStkNP_Vectors  is new ada.containers.indefinite_Vectors (Positive, sStkNP);
   subtype sStkNP_Vector   is     sStkNP_Vectors.Vector;

   package sStkCLN_Vectors  is new ada.containers.indefinite_Vectors (Positive, sStkCLN);
   subtype sStkCLN_Vector   is     sStkCLN_Vectors.Vector;



   function nearest (i     : in     Integer_Vector;
                      a     : in     sStkNPS_Vector;
                      v     : in     math.Real;
                      the_l,
                     the_h : in     Integer) return Integer;


   function allocate (ifree : access Integer_Vector;
                      stock : access sStkNPS_Vector;
                      value : in     sStkNPS     ) return Integer;





   function Root   (Self : in Item) return Node_view;
   function Leaves (Self : in Item) return Integer;





private


   type AabbMm is tagged
      record
         mi, mx : aliased math.Vector_3;
      end record;


   procedure AddSpan (Self : in AabbMm;   d        : in math.Vector_3;
                                          smi, smx : in out math.Real);



   --  Constants
   --
   SIMPLE_STACKSIZE : constant := 64;
   DOUBLE_STACKSIZE : constant := SIMPLE_STACKSIZE * 2;




   package sStkNN_Vectors is new ada.containers.Indefinite_Vectors (Positive, sStkNN);
   subtype sStkNN_Vector  is     sStkNN_Vectors.Vector;



   type Item is tagged limited
      record
         m_root     : aliased Node_view;
         m_free     :         Node_view;

         m_lkhd     : Integer := -1;
         m_leaves   : Integer :=  0;

         m_opath    : interfaces.Unsigned_32 := 0;

         m_stkStack : sStkNN_Vector;
      end record;



end impact.d3.collision.bounding_volume_Tree;
