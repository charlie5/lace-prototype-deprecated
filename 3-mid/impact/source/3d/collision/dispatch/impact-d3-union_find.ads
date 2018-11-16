--  #include "LinearMath/btAlignedObjectArray.h"

private
with ada.Containers.Vectors,
     ada.unchecked_Deallocation;



package impact.d3.union_Find
--
--    UnionFind calculates connected subsets
--
--    Implements weighted Quick Union with path compression optimization: could use short ints
--  instead of ints (halving memory, would limit the number of rigid bodies to 64k, sounds reasonable).
--
is

   type Item is tagged private;
   type View is access all Item'Class;


   USE_PATH_COMPRESSION                  : constant Boolean := True;
   STATIC_SIMULATION_ISLAND_OPTIMIZATION : constant Boolean := True;
   --
   --  See for discussion of static island optimizations by Vroonsh here: http://code.google.com/p/bullet/issues/detail?id=406.



   type btElement is
      record
         m_id,
         m_sz : Integer;
      end record;



   --- Forge
   --

   procedure destruct (Self : in out Item);





   --- Attributes
   --


   function getNumElements (Self : in Item) return Natural;


--            SIMD_FORCE_INLINE int        getNumElements() const
--            {
--                    return int(m_elements.size());
--            }



   function isRoot         (Self : in Item;   x : in Integer) return Boolean;

--            SIMD_FORCE_INLINE bool  isRoot(int x) const
--            {
--                    return (x == m_elements[x].m_id);
--            }



   function getElement (Self : in Item;   index : in Integer) return access btElement;

--            btElement&        getElement(int index)
--            {
--                    return m_elements[index];
--            }







   function find (Self : in Item;   p, q : in Integer) return Boolean;


--            int find(int p, int q)
--                  {
--                          return (find(p) == find(q));
--                  }




   function find (Self : in Item;   index : in Integer) return Integer;


--                  int find(int x)
--                  {
--                          //btAssert(x < m_N);
--                          //btAssert(x >= 0);
--
--                          while (x != m_elements[x].m_id)
--                          {
--                  //not really a reason not to use path compression, and it flattens the trees/improves find performance dramatically
--
--                  #ifdef USE_PATH_COMPRESSION
--                                  const btElement* elementPtr = &m_elements[m_elements[x].m_id];
--                                  m_elements[x].m_id = elementPtr->m_id;
--                                  x = elementPtr->m_id;
--                  #else//
--                                  x = m_elements[x].m_id;
--                  #endif
--                                  //btAssert(x < m_N);
--                                  //btAssert(x >= 0);
--
--                          }
--                          return x;
--                  }








   --- Operations
   --

   procedure sortIslands (Self : in out Item);
   --
   --  This is a special operation, destroying the content of impact.d3.union_Find.
   --
   --  It sorts the elements, based on island id, in order to make it easy to iterate over islands




   procedure reset (Self : in out Item;   N : in Integer);


   procedure allocate (Self : in out Item;   N : in Integer);
   procedure Free     (Self : in out Item);






   procedure unite (Self : in out Item;   p, q : in Integer);


--                  void unite(int p, int q)
--                  {
--                          int i = find(p), j = find(q);
--                          if (i == j)
--                                  return;
--
--  #ifndef USE_PATH_COMPRESSION
--                          //weighted quick union, this keeps the 'trees' balanced, and keeps performance of unite O( log(n) )
--                          if (m_elements[i].m_sz < m_elements[j].m_sz)
--                          {
--                                  m_elements[i].m_id = j; m_elements[j].m_sz += m_elements[i].m_sz;
--                          }
--                          else
--                          {
--                                  m_elements[j].m_id = i; m_elements[i].m_sz += m_elements[j].m_sz;
--                          }
--  #else
--                          m_elements[i].m_id = j; m_elements[j].m_sz += m_elements[i].m_sz;
--  #endif //USE_PATH_COMPRESSION
--                  }






private

   type btElement_view is access all btElement;

   procedure free is new ada.unchecked_Deallocation (btElement, btElement_view);



   package btElement_Vectors is new ada.Containers.Vectors (Natural, btElement_view);
   subtype btElement_Vector  is     btElement_Vectors.Vector;



   type Item is tagged
      record
         m_elements : btElement_Vector;
      end record;


end impact.d3.union_Find;
