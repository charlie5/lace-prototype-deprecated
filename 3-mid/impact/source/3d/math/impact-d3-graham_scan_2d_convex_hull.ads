with Ada.Containers.Vectors;
--  #include "impact.d3.Vector.h"
--  #include "btAlignedObjectArray.h"

package Impact.d3.graham_scan_2d_convex_Hull
   --
   --
   --
is

   type GrahamVector2 is record
      Vector : Math.Vector_3;

      m_angle    : Math.Real;
      m_orgIndex : Integer;

      --           m_anchor   : math.Vector_3;   -- Only for sorting.
   end record;

   function to_GrahamVector2
     (org      : in Math.Vector_3;
      orgIndex : in Integer)
      return     GrahamVector2;

   --          GrahamVector2(const impact.d3.Vector& org, int orgIndex)
   --                  :impact.d3.Vector(org),
   --                          m_orgIndex(orgIndex)
   --          {
   --          }

   --     type btAngleCompareFunc is
   --        record
   --           m_anchor : math.Vector_3;
   --        end record;
   --
   --
   --
   --     function to_btAngleCompareFunc (anchor : in  math.Vector_3) return
   --btAngleCompareFunc;

   --          btAngleCompareFunc(const impact.d3.Vector& anchor)
   --          : m_anchor(anchor)
   --          {
   --          }

   --     function "<" (a, b : in    GrahamVector2) return Boolean;

   --     function equivalent (Self : in btAngleCompareFunc;   a, b : in
   --GrahamVector2) return Boolean;

   --          bool operator()(const GrahamVector2& a, const GrahamVector2& b)
   --{
   --                  if (a.m_angle != b.m_angle)
   --                          return a.m_angle < b.m_angle;
   --                  else
   --                  {
   --                          impact.d3.Scalar al = (a-m_anchor).length2();
   --                          impact.d3.Scalar bl = (b-m_anchor).length2();
   --                          if (al != bl)
   --                                  return  al < bl;
   --                          else
   --                          {
   --                                  return a.m_orgIndex < b.m_orgIndex;
   --                          }
   --                  }
   --          }

   package GrahamVector2_Vectors is new Ada.Containers.Vectors (
      Positive,
      GrahamVector2);
   subtype GrahamVector2_Vector is GrahamVector2_Vectors.Vector;

   procedure GrahamScanConvexHull2D
     (originalPoints : in out GrahamVector2_Vector;
      hull           : in out GrahamVector2_Vector);

   --  inline void GrahamScanConvexHull2D(btAlignedObjectArray<GrahamVector2>&
   --originalPoints, btAlignedObjectArray<GrahamVector2>& hull)
   --  {
   --          if (originalPoints.size()<=1)
   --          {
   --                  for (int i=0;i<originalPoints.size();i++)
   --                          hull.push_back(originalPoints[0]);
   --                  return;
   --          }
   --          //step1 : find anchor point with smallest x/y and move it to
   --first location
   --          //also precompute angles
   --          for (int i=0;i<originalPoints.size();i++)
   --          {
   --                  const impact.d3.Vector& left = originalPoints[i];
   --                  const impact.d3.Vector& right = originalPoints[0];
   --                  if (left.x() < right.x() || !(right.x() < left.x()) &&
   --left.y() < right.y())
   --                  {
   --                          originalPoints.swap(0,i);
   --                  }
   --          }
   --
   --          for (int i=0;i<originalPoints.size();i++)
   --          {
   --                  impact.d3.Vector xvec(1,0,0);
   --                  impact.d3.Vector ar =
   --originalPoints[i]-originalPoints[0];
   --                  originalPoints[i].m_angle = btCross(xvec,
   --ar).dot(impact.d3.Vector(0,0,1)) / ar.length();
   --          }
   --
   --          //step 2: sort all points, based on 'angle' with this anchor
   --          btAngleCompareFunc comp(originalPoints[0]);
   --          originalPoints.quickSortInternal(comp,1,originalPoints.size()-1)
   --;
   --
   --          int i;
   --          for (i = 0; i<2; i++)
   --                  hull.push_back(originalPoints[i]);
   --
   --          //step 3: keep all 'convex' points and discard concave points
   --(using back tracking)
   --          for (; i != originalPoints.size(); i++)
   --          {
   --                  bool isConvex = false;
   --                  while (!isConvex&& hull.size()>1) {
   --                          impact.d3.Vector& a = hull[hull.size()-2];
   --                          impact.d3.Vector& b = hull[hull.size()-1];
   --                          isConvex =
   --btCross(a-b,a-originalPoints[i]).dot(impact.d3.Vector(0,0,1))> 0;
   --                          if (!isConvex)
   --                                  hull.pop_back();
   --                          else
   --                                  hull.push_back(originalPoints[i]);
   --                  }
   --          }
   --  }

end Impact.d3.graham_scan_2d_convex_Hull;
