with impact.d3.Vector;
with ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System;
with impact.d3.aabb_Util;


package body impact.d3.collision.bounding_volume_Tree
is



   subtype tNodeArray      is Node_Vector;
   subtype tConstNodeArray is Node_Vector;


   procedure free is new ada.Unchecked_Deallocation (Node'Class, Node_view);





   type NodeEnumerator is new ICollide with
      record
         nodes : tConstNodeArray;
      end record;



   overriding procedure Process   (Self : in out NodeEnumerator;   n1     : access Node'Class)
   is
   begin
      Self.nodes.append (n1.all'Access);
   end Process;






   function indexof (node : access bounding_volume_Tree.Node'Class) return Integer
   is
   begin
      return Boolean'Pos (node.parent.state.childs (2)  =  node) + 1;
   end indexof;











   --- Defaults volumes
   --

   --  AabbMm
   --


   function FromCE     (c,  e  : in     math.Vector_3                  )       return AabbMm
   is
      use Math;
      box : AabbMm;
   begin
      box.mi := c - e;
      box.mx := c + e;

      return box;
   end FromCE;

















   function FromCR     (c      : in     math.Vector_3;   r : in math.Real)       return AabbMm
   is
   begin
      return FromCE (c,  (r, r, r));
   end FromCR;










   function FromMM     (mi, mx : in     math.Vector_3)                           return AabbMm
   is
      box : AabbMm;
   begin
      box.mi := mi;
      box.mx := mx;

      return box;
   end FromMM;






   function FromPoints (pts    : in     Vector_3_array)                   return AabbMm
   is
      use impact.d3.Vector;
      box : AabbMm;
   begin
      box.mi := pts (1);
      box.mx := pts (1);

      for i in 2 .. pts'Last
      loop
         setMin (box.mi,  pts (i));
         setMax (box.mx,  pts (i));
      end loop;


      return box;
   end FromPoints;







   function FromPoints (ppts   : in Vector_3_view_array;   n : in Integer) return AabbMm
   is
      use impact.d3.Vector;
      box : AabbMm;

   begin
      box.mi := ppts (1).all;
      box.mx := ppts (1).all;

      for i in 2 .. n
      loop
         setMin (box.mi, ppts (i).all);
         setMax (box.mx, ppts (i).all);
      end loop;


      return box;
   end FromPoints;







   function  Center  (Self : in AabbMm) return math.Vector_3
   is
      use Math;
   begin
      return (Self.mi + Self.mx) / 2.0;
   end Center;





   function  Lengths (Self : in AabbMm) return math.Vector_3
   is
      use Math;
   begin
      return Self.mx - Self.mi;
   end Lengths;





   function  Extents (Self : in AabbMm) return math.Vector_3
   is
      use Math;
   begin
      return (Self.mx - Self.mi) / 2.0;
   end Extents;





   function  Mins    (Self : in AabbMm) return math.Vector_3
   is
   begin
      return Self.mi;
   end Mins;




   function  Maxs    (Self : in AabbMm) return math.Vector_3
   is
   begin
      return Self.mx;
   end Maxs;






   procedure Expand       (Self : in out AabbMm;   e : in math.Vector_3)
   is
      use Math;
   begin
      Self.mi := Self.mi - e;
      Self.mx := Self.mx + e;
   end Expand;









   procedure SignedExpand (Self : in out AabbMm;   e : in math.Vector_3)
   is
   begin
      if e (1) > 0.0 then
         Self.mx (1) := Self.mx (1) + e (1);
      else
         Self.mi (1) := Self.mi (1) + e (1);
      end if;


      if e (2) > 0.0 then
         Self.mx (2) := Self.mx (2) + e (2);
      else
         Self.mi (2) := Self.mi (2) + e (2);
      end if;


      if e (3) > 0.0 then
         Self.mx (3) := Self.mx (3) + e (3);
      else
         Self.mi (3) := Self.mi (3) + e (3);
      end if;

   end SignedExpand;








   function  Contain      (Self : in AabbMm'Class;   a : in AabbMm'Class) return Boolean
   is
   begin
      return              Self.mi (1) <= a.mi (1)
             and then Self.mi (2) <= a.mi (2)
             and then Self.mi (3) <= a.mi (3)
             and then Self.mx (1) >= a.mx (1)
             and then Self.mx (2) >= a.mx (2)
             and then Self.mx (3) >= a.mx (3);
   end Contain;







   function  Classify     (Self : in AabbMm;   n : in math.Vector_3;
                                                     o : in math.Real;
                                                     s : in Integer) return Integer
   is
      use impact.d3.Vector;

      pi, px : math.Vector_3;

   begin
      case s
      is
         when 0+0+0 =>
            px := Self.mi;
            pi := Self.mx;

         when 1+0+0 =>
            px := (Self.mx (1),  Self.mi (2),  Self.mi (3));
            pi := (Self.mi (1),  Self.mx (2),  Self.mx (3));

         when 0+2+0 =>
            px := (Self.mi (1),  Self.mx (2),  Self.mi (3));
            pi := (Self.mx (1),  Self.mi (2),  Self.mx (3));

         when 1+2+0 =>
            px := (Self.mx (1),  Self.mx (2),  Self.mi (3));
            pi := (Self.mi (1),  Self.mi (2),  Self.mx (3));

         when 0+0+4 =>
            px := (Self.mi (1),  Self.mi (2),  Self.mx (3));
            pi := (Self.mx (1),  Self.mx (2),  Self.mi (3));

         when 1+0+4 =>
            px := (Self.mx (1),  Self.mi (2),  Self.mx (3));
            pi := (Self.mi (1),  Self.mx (2),  Self.mi (3));

         when 0+2+4 =>
            px := (Self.mi (1),  Self.mx (2),  Self.mx (3));
            pi := (Self.mx (1),  Self.mi (2),  Self.mi (3));

         when 1+2+4 =>
            px := (Self.mx (1),  Self.mx (2),  Self.mx (3));
            pi := (Self.mi (1),  Self.mi (2),  Self.mi (3));

         when others =>
            raise Program_Error;
      end case;


      if dot (n, px) + o  <   0.0 then
         return -1;
      end if;

      if dot (n, pi) + o  >=  0.0 then
         return +1;
      end if;


      return 0;
   end Classify;






   function  ProjectMinimum (Self : access AabbMm;   v     : in math.Vector_3;
                                                       signs : in interfaces.Unsigned_32) return math.Real
   is
      use      impact.d3.Vector;
      use type interfaces.Unsigned_32;

      b : constant Vector_3_view_array := (Self.mx'Access,
                                  Self.mi'Access);

      p : constant math.Vector_3 := (b (Integer (signs    and 1)) (1),
                                     b (Integer ((signs / 2) and 1)) (2),
                                     b (Integer ((signs / 4) and 1)) (3));
   begin
      return dot (p, v);
   end ProjectMinimum;








   function  Intersect (a, b : in AabbMm'Class)  return Boolean
   is
   begin
      return         a.mi (1) <= b.mx (1)
        and then a.mx (1) >= b.mi (1)
        and then a.mi (2) <= b.mx (2)
        and then a.mx (2) >= b.mi (2)
        and then a.mi (3) <= b.mx (3)
        and then a.mx (3) >= b.mi (3);
   end Intersect;






   function  Intersect      (a    : in AabbMm;
                             b    : in math.Vector_3) return Boolean
   is
   begin
      return         (b (1) >= a.mi (1))
        and then (b (2) >= a.mi (2))
        and then (b (3) >= a.mi (3))
        and then (b (1) <= a.mx (1))
        and then (b (2) <= a.mx (2))
        and then (b (3) <= a.mx (3));
   end Intersect;





   function  Proximity      (a, b : in AabbMm)  return math.Real
   is
      use Math;
      d : constant math.Vector_3 := (a.mi + a.mx)  -  (b.mi + b.mx);
   begin
      return  abs (d (1)) + abs (d (2)) + abs (d (3));
   end Proximity;






   function  do_Select      (o, a, b : in AabbMm)  return Integer
   is
   begin
      if Proximity (o, a)  <  Proximity (o, b) then
         return 1;
      else
         return 2;
      end if;
   end do_Select;






   procedure Merge (a, b : in     AabbMm;
                    r    :    out AabbMm)
   is
   begin
      for i in 1 .. 3
      loop
         if a.mi (i)  <  b.mi (i) then   r.mi (i) := a.mi (i);
         else   r.mi (i) := b.mi (i);
         end if;

         if a.mx (i)  >  b.mx (i) then   r.mx (i) := a.mx (i);
         else   r.mx (i) := b.mx (i);
         end if;
      end loop;
   end Merge;






   function merge (a, b : in Volume) return Volume
   is
      res : Volume;
   begin
      Merge (a, b,  res);

      return res;
   end merge;







   --  volume+edge lengths
   --
   function size (a : in Volume) return math.Real
   is
      edges : math.Vector_3 renames a.Lengths;
   begin
      return   edges (1) * edges (2) * edges (3)
             + edges (1) + edges (2) + edges (3);
   end size;






   procedure getmaxdepth (node     : access bounding_volume_Tree.Node'Class;
                          depth    : in     Integer;
                          maxdepth : in out Integer)
   is
   begin
      if node.isinternal then
         getmaxdepth (node.state.childs (1),  depth + 1,  maxdepth);
         getmaxdepth (node.state.childs (2),  depth + 1,  maxdepth);
      else
         maxdepth := Integer'Max (maxdepth, depth);
      end if;
   end getmaxdepth;






   procedure deletenode (pdbvt : access impact.d3.collision.bounding_volume_Tree.item;
                         node  : access bounding_volume_Tree.Node'Class)
   is
   begin
      free (pdbvt.m_free);
      pdbvt.m_free := node.all'Access;
   end deletenode;





   procedure recursedeletenode (pdbvt : access impact.d3.collision.bounding_volume_Tree.item;
                                node  : access bounding_volume_Tree.Node'Class)
   is
   begin
      if not node.isleaf then
         recursedeletenode (pdbvt, node.state.childs (1));
         recursedeletenode (pdbvt, node.state.childs (2));
      end if;

      if node = pdbvt.m_root then
         pdbvt.m_root := null;
      end if;

      deletenode (pdbvt, node);
   end recursedeletenode;








   function createnode (pdbvt  : access impact.d3.collision.bounding_volume_Tree.item'Class;
                        parent : access Node'Class;
                        data   : access Any'Class) return Node_view
   is
      node : Node_view;

   begin
      if pdbvt.m_free /= null then
         node := pdbvt.m_free;
         pdbvt.m_free := null;
      else
         node := new bounding_volume_Tree.Node;
      end if;

      node.parent := Node_view (parent);
      node.state  := ( -- kind => use_data,
                      data => data,
                      childs => (others => null),
                      dataasint => 0);

      return node;
   end createnode;





   function createnode (pdbvt  : access impact.d3.collision.bounding_volume_Tree.item'Class;
                        parent : access Node'Class;
                        volume : in     bounding_volume_Tree.Volume;
                        data   : access Any'Class) return Node_view
   is
      node : constant Node_view := createnode (pdbvt, parent, data).all'Access;
   begin
      node.volume := volume;
      return node;
   end createnode;






   function createnode (pdbvt   : access impact.d3.collision.bounding_volume_Tree.item;
                        parent  : access Node'Class;
                        volume0 : in     Volume;
                        volume1 : in     Volume;
                        data    : access Any'Class) return Node_view
   is
      node : constant Node_view := createnode (pdbvt, parent, data).all'Access;
   begin
      Merge (volume0, volume1,  node.volume);

      return node;
   end createnode;








   procedure insertleaf (pdbvt          : access impact.d3.collision.bounding_volume_Tree.item;
                         the_root, leaf : in     Node_view)
   is
      root : Node_view := the_Root;

      prev,
      node : Node_view;

   begin
      if pdbvt.m_root = null then
         pdbvt.m_root := leaf.all'Access;
         leaf.parent  := null;

      else
         if not root.isleaf then
            loop
               root := root.state.childs (do_Select (leaf.volume,
                                                     root.state.childs (1).volume,
                                                     root.state.childs (2).volume));
               exit when root.isleaf;
            end loop;
         end if;


         prev := root.parent;
         node := createnode (pdbvt, prev, leaf.volume, root.volume, null);

         if prev /= null then
            prev.state.childs (indexof (root)) := node.all'Access;

            node.state.childs (1) := root;   root.parent := node;
            node.state.childs (2) := leaf;   leaf.parent := node;

            loop
               if not prev.volume.Contain (node.volume) then
                  Merge (prev.state.childs (1).volume,
                         prev.state.childs (2).volume,
                         prev.volume);
               else
                  exit;
               end if;

               node := prev;
               prev := node.parent;

               exit when prev = null;
            end loop;

         else
            node.state.childs (1) := root;   root.parent := node;
            node.state.childs (2) := leaf;   leaf.parent := node;
            pdbvt.m_root          := node;
         end if;

      end if;

   end insertleaf;






   function removeleaf (pdbvt : access impact.d3.collision.bounding_volume_Tree.item;
                        leaf  : in     Node_view) return Node_view
   is
      parent,
      prev,
      sibling  : Node_view;

   begin
      if leaf = pdbvt.m_root then
         pdbvt.m_root := null;
         return null;

      else
         parent  := leaf.parent;
         prev    := parent.parent;

         if indexof (leaf) = 1 then
            sibling := parent.state.childs (2);
         else
            sibling := parent.state.childs (1);
         end if;

--           sibling := parent.state.childs (1 - indexof (leaf));


         if prev /= null then
            prev.state.childs (indexof (parent)) := sibling;
            sibling.parent                       := prev;

            deletenode (pdbvt, parent);


            while prev /= null
            loop
               declare
                  pb : Volume renames prev.volume;
               begin
                  Merge (prev.state.childs (1).volume,
                         prev.state.childs (2).volume,   prev.volume);

                  if NotEqual (pb, prev.volume) then
                     prev := prev.parent;
                  else
                     exit;
                  end if;
               end;
            end loop;


            if prev /= null then
               return prev;
            else
               return pdbvt.m_root;
            end if;


         else
            pdbvt.m_root   := sibling;
            sibling.parent := null;

            deletenode (pdbvt, parent);

            return pdbvt.m_root;
         end if;
      end if;


   end removeleaf;







   procedure fetchleaves (pdbvt  : access impact.d3.collision.bounding_volume_Tree.item;
                          root   : in     Node_view;
                          leaves : in out tNodeArray;
                          depth  : in     Integer        := -1)
   is
   begin
      if         root.isinternal
        and then depth /= 0
      then
         fetchleaves (pdbvt, root.state.childs (1),  leaves,  depth - 1);
         fetchleaves (pdbvt, root.state.childs (2),  leaves,  depth - 1);
         deletenode  (pdbvt, root);
      else
         leaves.append (root);
      end if;
   end fetchleaves;






   procedure split (leaves      : in     tNodeArray;
                    left, right : in out tNodeArray;
                    org,  axis  : in     math.Vector_3)
   is
      use impact.d3.Vector, Math;

   begin
      left.clear;
      right.clear;

      for i in 1 .. Integer (leaves.Length)
      loop
         if dot (axis, leaves.Element (i).volume.Center - org) < 0.0 then
            left .append (leaves.Element (i));
         else
            right.append (leaves.Element (i));
         end if;
      end loop;

   end split;






   function bounds (leaves : in tNodeArray) return Volume
   is
      volume : bounding_volume_Tree.Volume := leaves.Element (1).volume;
   begin
      for i in 2 .. Integer (leaves.Length)
      loop
         Merge (volume, leaves.Element (i).volume,  volume);
      end loop;

      return volume;
   end bounds;






   procedure bottomup (pdbvt  : access impact.d3.collision.bounding_volume_Tree.item;
                       leaves : in out tNodeArray)
   is
      use type ada.containers.Count_type;

   begin
      while leaves.Length > 1
      loop
         declare
            minsize : math.Real     := math.Real'Last;
            minidx  : math.Integers := (1 => -1,
                                        2 => -1);
            sz      : math.Real;

         begin
            for i in 1 .. Integer (leaves.Length)
            loop
               for j in i + 1 .. Integer (leaves.Length)
               loop
                  sz := size (merge (leaves.Element (i).volume,
                                     leaves.Element (j).volume));

                  if sz < minsize then
                     minsize    := sz;
                     minidx (1)        := i;
                     minidx (2)        := j;
                  end if;
               end loop;
            end loop;


            declare
               n : constant Node_views := (leaves.Element (minidx (1)),  leaves.Element (minidx (2)));
               p : constant Node_view  := createnode (pdbvt,  null,  n (1).volume,
                                                                  n (2).volume,   null);
            begin
               p.state.childs (1)  := n (1);
               p.state.childs (2)  := n (2);
               n (1).parent        := p;
               n (2).parent        := p;
               leaves.replace_Element (minidx (1), p);

               leaves.swap (minidx (2),  Integer (leaves.Length) - 0);
               leaves.delete_Last;
            end;
         end;
      end loop;
   end bottomup;







   axis : constant Vector_3_array := ((1.0, 0.0, 0.0),
                                      (0.0, 1.0, 0.0),
                                      (0.0, 0.0, 1.0));



   function topdown (pdbvt       : access impact.d3.collision.bounding_volume_Tree.item;
                     leaves      : access tNodeArray;
                     bu_treshold : in     Integer  ) return Node_view
   is
      use type ada.containers.Count_type;

   begin
      if leaves.Length > 1 then

         if Integer (leaves.Length) > bu_treshold then
            declare
               use ada.Containers, Interfaces;

               vol        : Volume  renames bounds (leaves.all);
               org        : math.Vector_3 renames vol.Center;

               sets       : array (1 .. 2) of aliased tNodeArray;

               bestaxis   : Integer := -1;
               bestmidp   : Integer := Integer (leaves.Length);

               splitcount : array (1 .. 3, 1 .. 2) of Integer := ((0, 0), (0, 0), (0, 0));

            begin
               for i in 1 .. Integer (leaves.Length)
               loop
                  declare
                     use impact.d3.Vector, Math;
                     x : constant math.Vector_3 := leaves.Element (i).volume.Center - org;
                  begin
                     for j in 1 .. 3
                     loop
                        if dot (x, axis (j)) > 0.0 then
                           splitcount (j, 2) := splitcount (j, 2) + 1;
                        else
                           splitcount (j, 1) := splitcount (j, 1) + 1;
                        end if;
                     end loop;
                  end;
               end loop;


               for i in 1 .. 3
               loop
                  if         (splitcount (i, 1) > 0)
                    and then (splitcount (i, 2) > 0)
                  then
                     declare
                        midp : constant Integer := Integer (abs (math.Real (splitcount (i, 1)
                                                                            - splitcount (i, 2))));
                     begin
                        if midp < bestmidp then
                           bestaxis := i;
                           bestmidp := midp;
                        end if;
                     end;
                  end if;
               end loop;


               if bestaxis >= 0 then
                  sets (1).reserve_Capacity (Count_type (splitcount (bestaxis, 1)));
                  sets (2).reserve_Capacity (Count_type (splitcount (bestaxis, 2)));

                  split (leaves.all, sets (1), sets (2),  org,  axis (bestaxis));

               else
                  sets (1).reserve_Capacity (leaves.Length / 2 + 1);
                  sets (2).reserve_Capacity (leaves.Length / 2);

                  for i in 1 .. Integer (leaves.Length)
                  loop
                     sets (Integer (Unsigned_32 (i) and 1)).append (leaves.Element (i));
                  end loop;
               end if;

               declare
                  node : constant Node_view := createnode (pdbvt, null, vol, null).all'Access;
               begin
                  node.state.childs (1) := topdown (pdbvt,  sets (1)'Access,  bu_treshold);
                  node.state.childs (2) := topdown (pdbvt,  sets (2)'Access,  bu_treshold);

                  node.state.childs (1).parent := node;
                  node.state.childs (2).parent := node;

                  return node;
               end;

            end;
         else
            bottomup (pdbvt, leaves.all);
            return leaves.Element (1);
         end if;
      end if;


      return leaves.Element (1);
   end topdown;








   function sort (n : in     Node_view;
                  r : access Node_view) return Node_view
   is
      use type ada.containers.Count_type;

      p : constant Node_view := n.parent;

      pragma Assert (n.isinternal);

      function to_long_Integer is new ada.unchecked_Conversion (System.address, Long_Integer);

   begin
      if         p /= null
        and then   to_long_Integer (p.all'Address)     -- tbd: check this !
                 > to_long_Integer (n.all'Address)
      then
         declare
            i : Integer renames indexof (n);
            j : constant Integer :=      3 - i;

            s : constant Node_view := p.state.childs (j);
            q : constant Node_view := p.parent;

            pragma Assert (n = p.state.childs (i));

         begin
            if q /= null then
               q.state.childs (indexof (p)) := n.all'Access;
            else
               r.all := n.all'Access;
            end if;

            s.parent := n.all'Access;
            p.parent := n.all'Access;
            n.parent := q;

            p.state.childs (1) := n.state.childs (1);
            p.state.childs (2) := n.state.childs (2);

            n.state.childs (1).parent := p;
            n.state.childs (2).parent := p;

            n.state.childs (i) := p;
            n.state.childs (j) := s;

            declare   -- swap p and q volumes
               Pad : constant Volume := p.volume;
            begin
               p.volume := n.volume;
               n.volume := Pad;
            end;


            return p;
         end;
      end if;


      return n;
   end sort;











   function  NotEqual (a, b : in AabbMm)  return Boolean
   is
   begin
      return        a.mi (1)  /=  b.mi (1)
        or else a.mi (2)  /=  b.mi (2)
        or else a.mi (3)  /=  b.mi (3)

        or else a.mx (1)  /=  b.mx (1)
        or else a.mx (2)  /=  b.mx (2)
        or else a.mx (3)  /=  b.mx (3);
   end NotEqual;










   --  Node
   --


   function  isleaf     (Self : in Node) return Boolean
   is
   begin
      return Self.state.childs (2) = null;
   end isleaf;




   function  isinternal (Self : in Node) return Boolean
   is
   begin
      return not Self.isleaf;
   end isinternal;













   --- Policies/Interfaces
   --

   --  ICollide
   --




   procedure Process   (Self : in out ICollide;   n      : access Node'Class;   a : in math.Real)
   is
      pragma Unreferenced (a);
   begin
      Self.process (n);
   end Process;





   function  Descent   (Self : in ICollide;   n : in Node'Class) return Boolean
   is
      pragma Unreferenced (Self, n);
   begin
      return True;
   end Descent;






   function  Allleaves (Self : in ICollide;   n : in Node'Class) return Boolean
   is
      pragma Unreferenced (Self, n);
   begin
      return True;
   end Allleaves;















   --- impact.d3.collision.bounding_volume_Tree
   --



   procedure destruct (Self : in out Item)
   is
   begin
      Self.clear;
   end destruct;






   procedure clear (Self : in out Item)
   is
   begin
      if Self.m_root /= null then
         recursedeletenode (Self'Access, Self.m_root);
      end if;

      free (Self.m_free);

      Self.m_lkhd  := -1;
      Self.m_stkStack.clear;
      Self.m_opath :=  0;
   end clear;




   function  empty (Self : in     Item) return Boolean
   is
   begin
      return Self.m_root = null;
   end empty;





   procedure optimizeBottomUp    (Self : in out Item)
   is
      use ada.Containers;
   begin
      if Self.m_root /= null then
         declare
            leaves : tNodeArray;
         begin
            leaves.reserve_Capacity (Count_type (Self.m_leaves));

            fetchleaves (Self'Access, Self.m_root,  leaves);
            bottomup    (Self'Access,               leaves);

            Self.m_root := leaves.Element (1);
         end;
      end if;
   end optimizeBottomUp;






   procedure optimizeTopDown     (Self : in out Item;   bu_treshold : Integer := 128)
   is
   begin
      if Self.m_root /= null then
         declare
            use ada.Containers;
            leaves : aliased tNodeArray;
         begin
            leaves.reserve_Capacity (Count_type (Self.m_leaves));

            fetchleaves (Self'Access, Self.m_root, leaves);

            Self.m_root := topdown (Self'Access, leaves'Access, bu_treshold);
         end;
      end if;
   end optimizeTopDown;







   procedure optimizeIncremental (Self : in out Item;   pass_Count : Integer)
   is
      passes : Integer := pass_Count;
   begin
      if passes < 0 then
         passes := Self.m_leaves;
      end if;

      if         Self.m_root /= null
        and then passes      >  0
      then
         loop
            declare
               use Interfaces;
               node : Node_view := Self.m_root;
               bit  : Unsigned_32     := 0;
            begin
               while node.isinternal
               loop
                  node := sort (node, Self.m_root'Access).state.childs (Integer ((Self.m_opath / 2**Natural (bit)) and 1) + 1);
                  bit  := (bit + 1) and (unsigned_32'Size - 1);
               end loop;

               Self.update (node.all'Access);
               Self.m_opath := Self.m_opath + 1;

               passes := passes - 1;
               exit when passes = 0;
            end;
         end loop;
      end if;
   end optimizeIncremental;






   function  insert (Self : access Item;   volume : in     bounding_volume_Tree.Volume'Class;
                                           data   : access Any'Class                      ) return access Node'Class
   is
      leaf : constant Node_view := createnode (Self,  null,  bounding_volume_Tree.Volume (volume),  data);
   begin
      insertleaf (Self, Self.m_root, leaf);
      Self.m_leaves := Self.m_leaves + 1;

      return leaf;
   end insert;






   procedure update (Self : in out Item;   leaf      : access Node'Class;
                                           lookahead : in     Integer         := -1)
   is
      root : Node_view := removeleaf (Self'Access, leaf.all'Access);
      i    : Integer;
   begin
      if root /= null then
         if lookahead >= 0 then
            i := 1;

            while      i <= lookahead
              and then root.parent /= null
            loop
               root := root.parent;
               i    := i + 1;
            end loop;

         else
            root := Self.m_root;
         end if;
      end if;


      insertleaf (Self'Access, root, leaf.all'Access);
   end update;







   procedure update (Self : in out Item;   leaf      : access Node  'Class;
                                           volume    : in     bounding_volume_Tree.Volume'Class)
   is
      root : Node_view := removeleaf (Self'Access, leaf.all'Access);
      i    : Integer;
   begin
      if root /= null then

         if Self.m_lkhd >= 0 then
            i := 1;

            while      i <= Self.m_lkhd
              and then root.parent /= null
            loop
               root := root.parent;
               i    := i + 1;
            end loop;

         else
            root := Self.m_root;
         end if;

      end if;


      leaf.volume := bounding_volume_Tree.Volume (volume);
      insertleaf (Self'Access, root, leaf.all'Access);
   end update;










   function update (Self : access Item;   leaf      : access Node  'Class;
                                          volume    : access bounding_volume_Tree.Volume'Class;
                                          velocity  : in     math.Vector_3;
                                          margin    : in     math.Real       ) return Boolean
   is
   begin
      if leaf.volume.Contain (volume.all) then
         return False;
      end if;

      volume.Expand ((margin, margin, margin));
      volume.SignedExpand (velocity);

      Self.update (leaf, volume.all);

      return True;
   end update;





   function update (Self : access Item;   leaf      : access Node  'Class;
                                           volume    : access     bounding_volume_Tree.Volume'Class;
                                           velocity  : in     math.Vector_3) return Boolean
   is
   begin
      if leaf.volume.Contain (volume.all) then
         return False;
      end if;

      volume.SignedExpand (velocity);
      Self.update (leaf, volume.all);

      return True;
   end update;






   function update (Self : access Item;   leaf      : access Node  'Class;
                                          volume    : access bounding_volume_Tree.Volume'Class;
                    margin    : in     math.Real       ) return Boolean
   is
   begin
      if leaf.volume.Contain (volume.all) then
         return False;
      end if;

      volume.Expand ((margin, margin, margin));
      Self.update   (leaf, volume.all);

      return True;
   end update;






   procedure remove (Self : in out Item;   leaf      : access Node  'Class)
   is
      unused : Node_view;
      pragma Unreferenced (unused);
   begin
      unused := removeleaf (Self'Access, leaf.all'Access);
      deletenode (Self'Access, leaf);

      Self.m_leaves := Self.m_leaves - 1;
   end remove;







   procedure write (Self : in      Item;   iwriter   : access impact.d3.collision.bounding_volume_Tree.IWriter'Class)
   is
      use ada.Containers;
      nodes : aliased NodeEnumerator;
   begin
      nodes.nodes.reserve_Capacity (Count_type (Self.m_leaves) * 2);

      enumNodes (Self.m_root, nodes'Access);

      iwriter.Prepare (Self.m_root.all,  Integer (nodes.nodes.Length));


      for i in 1 .. Integer (nodes.nodes.Length)
        loop
         declare
            n : Node_view renames nodes.nodes.Element (i);
            p : Integer         :=      -1;
         begin
            if n.parent /= null then
               p := nodes.nodes.find_Index (n.parent);
            end if;


            if n.isinternal then
               declare
                  c0 : Integer renames nodes.nodes.find_Index (n.state.childs (1));
                  c1 : Integer renames nodes.nodes.find_Index (n.state.childs (2));
               begin
                  iwriter.WriteNode (n.all, i, p, c0, c1);
               end;
            else
               iwriter.WriteLeaf (n.all, i, p);
            end if;

         end;
      end loop;

   end write;








   procedure clone (Self : in      Item;   dest      : access Item'Class;
                                           iclone    : access impact.d3.collision.bounding_volume_Tree.IClone'Class := null)
   is
   begin
      dest.clear;

      if Self.m_root /= null then
         declare
            use ada.Containers;
            stack : sStkCLN_Vector;
         begin
            stack.reserve_Capacity (Count_type (Self.m_leaves));
            stack.append           (sStkCLN' (Self.m_root,  null));


            loop
               declare
                  use Interfaces;

                  i : constant Integer := Integer (stack.Length);
                  e : sStkCLN renames stack.Element (i);
                  n : constant Node_view := createnode (dest,  e.parent,  e.node.volume, e.node.state.data);
               begin
                  stack.delete_Last;

                  if e.parent /= null then
                     e.parent.state.childs (Integer (Unsigned_32 (i) and 1)) := n;
                  else
                     dest.m_root := n;
                  end if;

                  if e.node.isinternal then
                     stack.append (sStkCLN'(e.node.state.childs (1),  n));
                     stack.append (sStkCLN'(e.node.state.childs (2),  n));
                  else
                     iclone.CloneLeaf (n.all);
                  end if;


                  exit when stack.Length = 0;
               end;
            end loop;

         end;
      end if;

   end clone;











   function maxdepth (node : access bounding_volume_Tree.Node) return Integer
   is
      depth : Integer := 0;
   begin
      if node /= null then
         getmaxdepth (node, 1, depth);
      end if;


      return depth;
   end maxdepth;






   function  countLeaves (node : in bounding_volume_Tree.Node) return Integer
   is
   begin
      if node.isinternal then
         return   countleaves (node.state.childs (1).all)
                + countleaves (node.state.childs (2).all);
      else
         return 1;
      end if;
   end countLeaves;






   procedure extractleaves (node : access bounding_volume_Tree.Node;   leaves : out Node_Vector)
   is
   begin
      if node.isinternal then
         extractleaves (node.state.childs (1),  leaves);
         extractleaves (node.state.childs (2),  leaves);
      else
         leaves.append (node.all'Access);
      end if;
   end extractleaves;











   procedure enumNodes  (root  : access Node;   policy : access ICollide'Class)
   is
   begin
      policy.Process (root);

      if root.isinternal then
         enumNodes (root.state.childs (1),  policy);
         enumNodes (root.state.childs (2),  policy);
      end if;
   end enumNodes;






   procedure enumleaves (root  : access Node;   policy : access ICollide'Class)
   is
   begin
      if root.isinternal then
         enumleaves (root.state.childs (1),  policy);
         enumleaves (root.state.childs (2),  policy);
      else
         policy.Process (root);
      end if;
   end enumleaves;





   procedure collideTT (Self : in out Item;   root0, root1 : access Node'Class;
                                              policy       : access ICollide  'Class)
   is
      pragma Unreferenced (Self);
   begin
      if         root0 /= null
        and then root1 /= null
      then
         declare
            depth    : Integer := 1;
            treshold : Integer := DOUBLE_STACKSIZE - 4;
            stkStack : sStkNN_Vector;             --  btAlignedObjectArray<sStkNN>        stkStack;

         begin
            stkStack.set_Length (DOUBLE_STACKSIZE);
            stkStack.replace_Element (1,  sStkNN'(root0, root1));


            loop
               declare
                  use type ada.containers.Count_type;
                  p  : sStkNN := stkStack.Element (depth);
               begin

                  depth := depth - 1;

                  if depth > treshold then
                     stkStack.set_Length (stkStack.Length * 2);
                     treshold := Integer (stkStack.Length - 4);
                  end if;


                  if  p.a = p.b then

                     if p.a.isinternal then
                        depth := depth + 1;   stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (1).all'Access,  p.a.state.childs (1).all'Access));
                        depth := depth + 1;   stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (2).all'Access,  p.a.state.childs (2).all'Access));
                        depth := depth + 1;   stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (1).all'Access,  p.a.state.childs (2).all'Access));
                     end if;

                  elsif Intersect (p.a.volume,  p.b.volume) then

                     if p.a.isinternal then

                        if p.b.isinternal then
                           depth := depth + 1;   stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (1).all'Access,  p.b.state.childs (1).all'Access));
                           depth := depth + 1;   stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (2).all'Access,  p.b.state.childs (1).all'Access));
                           depth := depth + 1;   stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (1).all'Access,  p.b.state.childs (2).all'Access));
                           depth := depth + 1;   stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (2).all'Access,  p.b.state.childs (2).all'Access));
                        else
                           depth := depth + 1;   stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (1).all'Access,  p.b));
                           depth := depth + 1;   stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (2).all'Access,  p.b));
                        end if;

                     else
                        if p.b.isinternal then
                           depth := depth + 1;   stkStack.replace_Element (depth,  sStkNN'(p.a,  p.b.state.childs (1).all'Access));
                           depth := depth + 1;   stkStack.replace_Element (depth,  sStkNN'(p.a,  p.b.state.childs (2).all'Access));
                        else
                           policy.Process (p.a,  p.b);
                        end if;
                     end if;
                  end if;


                  exit when depth = 0;
               end;
            end loop;

         end;
      end if;

   end collideTT;






   procedure collideTTpersistentStack (Self : in out Item;   root0, root1 : access Node'Class;
                                                             policy       : access ICollide'Class)
   is
      depth,
      threshold : Integer;

   begin
      if root0 /= null and then root1 /= null then
         depth     := 1;
         threshold := DOUBLE_STACKSIZE - 4;

         Self.m_stkStack.set_Length      (DOUBLE_STACKSIZE);
         Self.m_stkStack.replace_Element (1,  sStkNN'(root0, root1));


         loop
            declare
               use type ada.containers.Count_type;
               p : sStkNN := Self.m_stkStack.Element (depth);
            begin
               depth := depth - 1;

               if depth > threshold then
                  Self.m_stkStack.set_Length (Self.m_stkStack.Length * 2);
                  threshold := Integer (Self.m_stkStack.Length) - 4;
               end if;


               if p.a = p.b then

                  if p.a.isinternal then

                     depth := depth + 1;   Self.m_stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (1),  p.a.state.childs (1)));
                     depth := depth + 1;   Self.m_stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (2),  p.a.state.childs (2)));
                     depth := depth + 1;   Self.m_stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (1),  p.a.state.childs (2)));
                  end if;

               elsif Intersect (p.a.volume,  p.b.volume) then

                  if p.a.isinternal then

                     if p.b.isinternal then
                        depth := depth + 1;   Self.m_stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (1),  p.b.state.childs (1)));
                        depth := depth + 1;   Self.m_stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (2),  p.b.state.childs (1)));
                        depth := depth + 1;   Self.m_stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (1),  p.b.state.childs (2)));
                        depth := depth + 1;   Self.m_stkStack.replace_Element (depth,  sStkNN'(p.a.state.childs (2),  p.b.state.childs (2)));
                     else
                        depth := depth + 1;   Self.m_stkStack.replace_Element (depth, sStkNN'(p.a.state.childs (1),  p.b));
                        depth := depth + 1;   Self.m_stkStack.replace_Element (depth, sStkNN'(p.a.state.childs (2),  p.b));
                     end if;

                  else
                     if p.b.isinternal then
                        depth := depth + 1;   Self.m_stkStack.replace_Element (depth,  sStkNN'(p.a,  p.b.state.childs (1)));
                        depth := depth + 1;   Self.m_stkStack.replace_Element (depth,  sStkNN'(p.a,  p.b.state.childs (2)));
                     else
                        policy.Process (p.a,  p.b);
                     end if;
                  end if;
               end if;


               exit when depth = 0;
            end;
         end loop;

      end if;
   end collideTTpersistentStack;







   procedure collideTV (Self : in Item;   root   : access Node'Class;
                                          vol    : in     Volume'Class;
                                          policy : access ICollide    'Class)
   is
      pragma Unreferenced (Self);
   begin
      if root /= null then
         declare
            use Node_Vectors;

            volume : constant bounding_volume_Tree.Volume'Class := vol;
            stack  : Node_Vector;   -- btAlignedObjectArray<const Node*>        ;
         begin
            stack.clear;
            stack.reserve_Capacity (SIMPLE_STACKSIZE);
            stack.append (root.all'Access);


            loop
               declare
                  use type ada.containers.Count_type;

                  n : constant access Node := stack.last_Element;
               begin
                  stack.delete_Last;

                  if Intersect (n.volume, volume) then
                     if n.isinternal then
                        stack.append (n.state.childs (1));
                        stack.append (n.state.childs (2));
                     else
                        policy.Process (n);
                     end if;
                  end if;


                  exit when stack.Length = 0;
               end;
            end loop;

         end;
      end if;

   end collideTV;










   procedure rayTest (root    : access Node;
                      rayFrom,
                      rayTo   : in     math.Vector_3;
                      policy  : access ICollide'Class)
   is
      use impact.d3.Vector,  Math;

      COMPARE_BTRAY_AABB2 : constant Boolean := True;

      rayDir              :          math.Vector_3;
      rayDirectionInverse :          math.vector_3;

   begin
      if root /= null then
         rayDir := Normalized (rayTo - rayFrom);

         --- what about division by zero? --> just set rayDirection[i] to INF/BT_LARGE_FLOAT
         --
         if rayDir (1) = 0.0 then   rayDirectionInverse (1) := BT_LARGE_FLOAT;
         else   rayDirectionInverse (1) := 1.0 / rayDir (1);
         end if;

         if rayDir (2) = 0.0 then   rayDirectionInverse (2) := BT_LARGE_FLOAT;
         else   rayDirectionInverse (2) := 1.0 / rayDir (2);
         end if;

         if rayDir (3) = 0.0 then   rayDirectionInverse (3) := BT_LARGE_FLOAT;
         else   rayDirectionInverse (3) := 1.0 / rayDir (3);
         end if;


         declare
            signs        : constant impact.d3.aabb_Util.Signs := (Boolean'Pos (rayDirectionInverse (1) < 0.0),
                                                         Boolean'Pos (rayDirectionInverse (2) < 0.0),
                                                         Boolean'Pos (rayDirectionInverse (3) < 0.0));

            lambda_max   :         math.Real        := dot (rayDir,  rayTo - rayFrom);
            resultNormal : aliased math.vector_3;
            stack        :         Node_Vector;

            depth        : Integer := 1;
            threshold    : Integer := DOUBLE_STACKSIZE - 2;

            bounds       : impact.d3.aabb_Util.Bounds;

         begin
            stack.set_Length      (DOUBLE_STACKSIZE);
            stack.replace_Element (1, root.all'Access);

            loop
               declare
                  use impact.d3.aabb_Util;
                  use type ada.Containers.Count_type;

                  node : constant access bounding_volume_Tree.Node := stack.Element (depth);

                  tmin       : aliased math.Real := 1.0;
                  lambda_min : constant math.Real := 0.0;

                  result1    : Boolean;

               begin
                  depth      := depth - 1;

                  bounds (1) := node.volume.Mins;
                  bounds (2) := node.volume.Maxs;

                  result1    := btRayAabb2 (rayFrom,    rayDirectionInverse,
                                            signs,      bounds,
                                            tmin'Access,
                                            lambda_min, lambda_max);

                  if COMPARE_BTRAY_AABB2 then
                     declare
                        param   : aliased math.Real := 1.0;
                        result2 : constant Boolean   := btRayAabb (rayFrom,   rayTo,
                                                                  node.volume.Mins,
                                                                  node.volume.Maxs,
                                                                  param       'Access,
                                                                  resultNormal'Access);
                        pragma Assert (result1 = result2);
                     begin
                        null;
                     end;

                  end if;


                  if result1 then

                     if node.isinternal then
                        if depth > threshold then
                           stack.set_Length (stack.Length * 2);
                           threshold := Integer (stack.Length - 2);
                        end if;

                        depth := depth + 1;   stack.replace_Element (depth,  node.state.childs (1));
                        depth := depth + 1;   stack.replace_Element (depth,  node.state.childs (2));

                     else
                        policy.Process (node);
                     end if;

                  end if;


                  exit when depth = 0;
               end;
            end loop;
         end;

      end if;
   end rayTest;








   procedure rayTestInternal (Self : in out Item;   root                : access Node'Class;
                                                    rayFrom,
                                                    rayTo,
                                                    rayDirectionInverse : in     math.Vector_3;
                                                    signs               : in     impact.d3.aabb_Util.Signs;
                                                    lambda_max          : in     math.Real;
                                                    aabbMin, aabbMax    : in     math.Vector_3;
                                                    policy              : access ICollide'Class)
   is
      pragma Unreferenced (Self, rayTo);
   begin

      if root /= null then
         declare
            --              resultNormal : math.Vector_3;

            depth        : Integer := 1;
            threshold    : Integer := DOUBLE_STACKSIZE - 2;

            stack        : Node_Vector;
            bounds       : Vector_3_array (1 .. 2);

         begin
            stack.set_Length (DOUBLE_STACKSIZE);
            stack.replace_Element (1, root.all'Access);


            loop
               declare
                  use impact.d3.aabb_Util, Math;
                  use type ada.containers.Count_type;

                  node       : constant access  bounding_volume_Tree.Node := stack.Element (depth);
                  tmin       : aliased math.Real  := 1.0;
                  lambda_min : constant math.Real  := 0.0;
                  result1    :         Boolean    := False;
               begin
                  depth      := depth - 1;
                  bounds (1) := node.volume.Mins - aabbMax;
                  bounds (2) := node.volume.Maxs - aabbMin;

                  result1 := btRayAabb2 (rayFrom, rayDirectionInverse,  signs, bounds, tmin'Access,  lambda_min, lambda_max);

                  if result1 then
                     if node.isinternal then

                        if depth > threshold then
                           stack.set_Length     (stack.Length  * 2);
                           threshold := Integer (stack.Length) - 2;
                        end if;

                        depth := depth + 1;   stack.replace_Element (depth,  node.state.childs (1));
                        depth := depth + 1;   stack.replace_Element (depth,  node.state.childs (2));

                     else
                        policy.Process (node);
                     end if;
                  end if;

                  exit when depth = 0;
               end;
            end loop;

         end;
      end if;

   end rayTestInternal;







   --  Helpers
   --


   function nearest  (i     : in     Integer_Vector;
                      a     : in     sStkNPS_Vector;
                      v     : in     math.Real;
                      the_l,
                      the_h : in     Integer) return Integer
   is
      l : Integer := the_l;
      h : Integer := the_h;

      m : Integer := 0;

   begin
      while l < h
      loop
         m := (l + h) / 2;

         if a.Element (i.Element (m)).value  >=  v then
            l := m + 1;
         else
            h := m;
         end if;
      end loop;


      return h;
   end nearest;






   function allocate (ifree : access Integer_Vector;
                      stock : access sStkNPS_Vector;
                      value : in     sStkNPS     ) return Integer
   is
      use type ada.containers.Count_Type;
      i : Integer;

   begin
      if ifree.Length > 0 then
         i := ifree.last_Element;
         ifree.delete_Last;

         stock.replace_Element (i, value);

      else
         stock.append (value);
         i := Integer (stock.Length);
      end if;


      return i;
   end allocate;





   --- Collide operations
   --


   procedure collideKDOP (root    : access Node;
                          normals : in     Vector_3_array;
                          offsets : in     math.Vector;
                          count   : in     Integer;
                          policy  : access ICollide'Class)
   is
   begin
      if root /= null then
         declare
            use Interfaces;
            use type Flags;

            inside : constant Flags := 2**count - 1;
            stack  : sStkNP_Vector;
            signs  : array (1 .. Unsigned_32'Size * 8 * 8) of Integer;

            pragma Assert (count  <  Integer ((signs'Size / 8) / (signs (1)'Size / 8)));

         begin
            for i in 1 .. count
            loop
               declare
                  N_x, N_y, N_z : Unsigned_32;
               begin
                  if normals (i) (1) >= 0.0 then   N_x := 1;
                  else   N_x := 0;
                  end if;

                  if normals (i) (2) >= 0.0 then   N_y := 2;
                  else   N_y := 0;
                  end if;

                  if normals (i) (3) >= 0.0 then   N_z := 4;
                  else   N_z := 0;
                  end if;

                  signs (i) := Integer (N_x + N_y + N_z);
               end;
            end loop;

            stack.reserve_Capacity (SIMPLE_STACKSIZE);
            stack.append           (sStkNP'(root, 0));

            loop
               declare
                  use type Flags,  ada.containers.Count_type;

                  se      : sStkNP  := stack.last_Element;
                  the_out : Boolean := False;
                  side    : Integer;

                  i       : Integer;
                  j       : Flags;

               begin
                  stack.delete_Last;

                  i := 1;
                  j := 2;
                  while not the_out and then i <= count
                  loop
                     if 0 = (se.mask and j) then
                        side := se.node.volume.Classify (normals (i), offsets (i), signs (i));

                        case side
                        is
                           when        -1     => the_out := True;
                           when        +1     => se.mask := se.mask or j;
                           when others => raise Program_Error;
                        end case;
                     end if;

                     i := i + 1;
                     j := j * 2;
                  end loop;


                  if not the_out then
                     if (se.mask /= inside) and then (se.node.isinternal) then
                        stack.append (sStkNP '(se.node.state.childs (1),  se.mask));
                        stack.append (sStkNP '(se.node.state.childs (2),  se.mask));
                     else
                        if policy.Allleaves (se.node.all) then
                           enumleaves (se.node, policy);
                        end if;
                     end if;
                  end if;

                  exit when stack.Length = 0;
               end;
            end loop;

         end;
      end if;
   end collideKDOP;








   procedure collideOCL (root     : access Node;
                          normals  : in     Vector_3_array;
                          offsets  : in     math.Vector;
                          sortaxis : in     math.Vector_3;
                          count    : in     Integer;
                          policy   : access ICollide'Class;
                         fullsort : in     Boolean       := True)
   is
   begin
      if root /= null then
         declare
            use Interfaces;
            use type Flags;

            SA_x, SA_y, SA_z : Unsigned_32;
            pragma Unreferenced (SA_y, SA_z);
            srtsgns          : Unsigned_32;

            inside           : constant Flags := 2**count - 1;

            stock            : aliased sStkNPS_Vector;
            ifree            : aliased Integer_Vector;
            stack            : aliased Integer_Vector;

            signs : math.Integers (1 .. Unsigned_32'Size * 8 * 8);
            pragma Assert (count  <  Integer ((signs'Size / 8) / (signs (1)'Size / 8)));

         begin
            if sortaxis (1) >= 0.0 then   SA_x := 1;
            else   SA_x := 0;
            end if;

            if sortaxis (2) >= 0.0 then   SA_y := 2;
            else   SA_y := 0;
            end if;

            if sortaxis (3) >= 0.0 then   SA_z := 4;
            else   SA_z := 0;
            end if;

            srtsgns := SA_x + SA_x + SA_x;


            for i in 1 .. count
            loop
               declare
                  N_x, N_y, N_z : Unsigned_32;
               begin
                  if normals (i) (1) >= 0.0 then   N_x := 1;
                  else   N_x := 0;
                  end if;

                  if normals (i) (2) >= 0.0 then   N_y := 2;
                  else   N_y := 0;
                  end if;

                  if normals (i) (3) >= 0.0 then   N_z := 4;
                  else   N_z := 0;
                  end if;

                  signs (i) := Integer (N_x + N_y + N_z);
               end;
            end loop;


            stock.reserve_Capacity (SIMPLE_STACKSIZE);
            stack.reserve_Capacity (SIMPLE_STACKSIZE);
            ifree.reserve_Capacity (SIMPLE_STACKSIZE);

            stack.append (allocate (ifree'Access,   stock'Access,   sStkNPS'(root,
                                                                              0,
                                                                              root.volume.ProjectMinimum (sortaxis, srtsgns))));

            loop
               declare
                  use type Flags,  ada.containers.Count_type;

                  id      : constant Integer := stack.last_Element;
                  se      : sStkNPS          := stock.Element (id);

                  the_out : Boolean;

                  i       : Integer;
                  j       : Flags;

                  side    : Integer;

                  continue_required : Boolean := False;

               begin
                  stack.delete_Last;
                  ifree.append (id);

                  if se.mask /= inside then
                     the_out := False;
                  end if;


                  i := 1;
                  j := 2;
                  while not the_out and then i <= count
                  loop
                     if (se.mask and j) = 0 then
                        side := se.node.volume.Classify (normals (i),  offsets (i),  signs (i));

                        case side
                        is
                           when -1     =>   the_out := True;
                           when +1     =>   se.mask := se.mask or j;
                           when others =>   raise Program_Error;
                        end case;
                     end if;

                     i := i + 1;
                     j := j * 2;
                  end loop;


                  if the_out then
                     continue_required := True;
                  end if;

                  if not continue_required then
                     if policy.Descent (se.node.all) then

                        if se.node.isinternal then
                           declare
                              pns : Node_views                    := (se.node.state.childs (1),
                                                                                      se.node.state.childs (2));

                              nes : array (Positive range <>) of sStkNPS := (sStkNPS'(pns (1),
                                                                                      se.mask,
                                                                                      pns (1).volume.ProjectMinimum (sortaxis, srtsgns)),
                                                                            sStkNPS'(pns (2),
                                                                                      se.mask,
                                                                                      pns (2).volume.ProjectMinimum (sortaxis, srtsgns)));

                              q : Integer;
                              j : Integer := Integer (stack.Length);

                              k : Integer;
                           begin
                              if nes (1).value  <  nes (2).value then
                                 q := 1;
                              else
                                 q := 0;
                              end if;

                              if fullsort and then j > 0 then
                                 --  Insert 0
                                 --
                                 j := nearest (stack, stock, nes (q).value, 0, Integer (stack.Length));
                                 stack.append (0);

                                 k := Integer (stack.Length) - 1;
                                 while k > j
                                 loop
                                    stack.replace_Element (k,  stack.Element (k - 1));
                                    k         := k - 1;
                                 end loop;

                                 stack.replace_Element (j,   allocate (ifree'Access, stock'Access, nes (q)));

                                 --  Insert 1
                                 --
                                 j := nearest (stack, stock, nes (1-q).value, j, Integer (stack.Length));
                                 stack.append (0);

                                 k := Integer (stack.Length) - 1;
                                 while k > j
                                 loop
                                    stack.replace_Element (k,  stack.Element (k - 1));
                                    k         := k - 1;
                                 end loop;

                                 stack.replace_Element (j,  allocate (ifree'Access, stock'Access, nes (1-q)));

                              else
                                 stack.append (allocate (ifree'Access, stock'Access, nes (q)));
                                 stack.append (allocate (ifree'Access, stock'Access, nes (1-q)));
                              end if;
                           end;
                        else
                           policy.Process (se.node, se.value);
                        end if;
                     end if;
                  end if;

                  exit when stack.Length = 0;
               end;
            end loop;

         end;
      end if;
   end collideOCL;









   procedure collideTU  (root    : access Node;
                         policy  : access ICollide'Class)
   is
   begin
      if root /= null then
         declare
            use type ada.containers.Count_type;

            stack : Node_Vector;
            n     : access Node;

         begin
            stack.reserve_Capacity (SIMPLE_STACKSIZE);
            stack.append (root.all'Access);

            loop
               n := stack.last_Element;
               stack.delete_Last;

               if policy.Descent (n.all) then
                  if n.isinternal then
                     stack.append (n.state.childs (1));
                     stack.append (n.state.childs (2));
                  else
                     policy.Process (n);
                  end if;
               end if;

               exit when stack.Length = 0;
            end loop;

         end;
      end if;
   end collideTU;











   procedure AddSpan (Self : in AabbMm;   d        : in     math.Vector_3;
                                                smi, smx : in out math.Real)
   is
   begin
      for i in 1 .. 3
      loop
         if d (i) < 0.0 then
            smi := smi  +  Self.mx (i) * d (i);
            smx := smx  +  Self.mi (i) * d (i);
         else
            smi := smi  +  Self.mi (i) * d (i);
            smx := smx  +  Self.mx (i) * d (i);
         end if;
      end loop;
   end AddSpan;





   function Root (Self : in Item) return Node_view
   is
   begin
      return Self.m_root;
   end Root;





   function leaves (Self : in Item) return Integer
   is
   begin
      return Self.m_leaves;
   end leaves;



end impact.d3.collision.bounding_volume_Tree;
