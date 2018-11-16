with impact.d3.Shape.convex.internal.sphere;
with Interfaces.C;
with impact.d3.Transform;
with impact.d3.Quaternions;
with impact.d3.Matrix;

with Interfaces;
with impact.d3.Vector;
with impact.d3.collision.gjk_epa;
with impact.d3.Scalar;

--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.internal.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.internal.sphere.h"




package body impact.d3.collision.gjk_epa
is






   package gjkepa2_impl
   is

      -----------
      --- Config
      --


      --- GJK
      --

      GJK_MAX_ITERATIONS : constant := 128;
      GJK_ACCURARY       : constant := 0.0001;
      GJK_MIN_DISTANCE   : constant := 0.0001;
      GJK_DUPLICATED_EPS : constant := 0.0001;
      GJK_SIMPLEX2_EPS   : constant := 0.0;
      GJK_SIMPLEX3_EPS   : constant := 0.0;
      GJK_SIMPLEX4_EPS   : constant := 0.0;



      --- EPA
      --

      EPA_MAX_VERTICES   : constant := 64;
      EPA_MAX_FACES      : constant := EPA_MAX_VERTICES * 2;
      EPA_MAX_ITERATIONS : constant := 255;
      EPA_ACCURACY       : constant := 0.0001;
      EPA_FALLBACK       : constant := 10 * EPA_ACCURACY;
      EPA_PLANE_EPS      : constant := 0.00001;
      EPA_INSIDE_EPS     : constant := 0.01;




      --- Shorthands
      --

      subtype U  is interfaces.Unsigned_64;
      subtype U1 is interfaces.c.unsigned_char;

      type u1_array is array (Positive range <>) of U1;




      --- MinkowskiDiff
      --

      type convex_shape_Pair is array (1 .. 2) of impact.d3.Shape.convex.view;

      type Ls_function is access function (Self : impact.d3.Shape.convex.item'Class;   vec : in math.Vector_3) return math.Vector_3;


      type MinkowskiDiff is tagged
         record
            m_shapes   : convex_shape_Pair;
            m_toshape1 : math.Matrix_3x3;
            m_toshape0 : Transform_3d;
            Ls         : Ls_function;                 --              impact.d3.Vector                                (impact.d3.Shape.convex::*Ls)(const impact.d3.Vector&) const;
         end record;


      procedure EnableMargin (Self : in out MinkowskiDiff;   enable : in Boolean);

      function  Support0     (Self : in     MinkowskiDiff;   d      : in math.Vector_3) return math.Vector_3;
      function  Support1     (Self : in     MinkowskiDiff;   d      : in math.Vector_3) return math.Vector_3;

      function  Support      (Self : in     MinkowskiDiff;   d      : in math.Vector_3) return math.Vector_3;
      function  Support      (Self : in     MinkowskiDiff;   d      : in math.Vector_3;
                                                             index  : in U          ) return math.Vector_3;

      subtype tShape is MinkowskiDiff;





      --- GJK
      --


      type sSV is
         record
           d, w : aliased math.Vector_3;
         end record;

      type sSV_array is array (Positive range <>) of aliased sSV;

      type sSV_view  is access all sSV;
      type sSV_views is array (Positive range <>) of sSV_view;




      type sSimplex is
         record
            c    : sSV_array (1 .. 4);
            p    : math.Vector_4;
            rank : U;
         end record;

      type sSimplex_array is array (Positive range <>) of aliased sSimplex;




      type eStatus is (Valid, Inside, Failed);




      type GJK is tagged
         record
            m_shape     : tShape;
            m_ray       : math.Vector_3;
            m_distance  : math.Real;
            m_simplices : sSimplex_array (1 .. 2);
            m_store     : sSV_array (1 .. 4);
            m_free      : sSV_views (1 .. 4);
            m_nfree     : U;
            m_current   : U;
            m_simplex   : access sSimplex;
            m_status    : eStatus;
         end record;



      function  to_GJK                return GJK;

      procedure Initialize    (Self : in out GJK);

      function  EncloseOrigin (Self : access GJK) return Boolean;

      function  Evaluate      (Self : access GJK;   shapearg : in tShape'Class;
                                                    guess    : in math.Vector_3) return eStatus;



      procedure appendvertice (Self : in out GJK;   simplex : access sSimplex;
                                                    v       : in     math.Vector_3);
      procedure removevertice (Self : in out GJK;   simplex : access sSimplex);







      --------
      --- EPA
      --


      --  Types
      --

      type sFace;
      type sFace_view  is access all sFace;
      type sFace_views is array (Positive range <>) of sFace_view;

      type sFace is
         record
            n    : math.Vector_3;
            d, p : math.Real;

            c    : sSV_views   (1 .. 3);
            f    : sFace_views (1 .. 3);
            l    : sFace_views (1 .. 2);
            e    : U1_array    (1 .. 3);

            pass : U1;
         end record;

      type sFace_array is array (Positive range <>) of aliased sFace;




      type sList is
         record
            root  : sFace_view;
            count : U         := 0;
         end record;


      type sHorizon is
         record
            cf,
            ff : sFace_view;
            nf : U         := 0;
         end record;


      type eStatus_EPA is (Valid,
                           Touching,
                           Degenerated,
                           NonConvex,
                           InvalidHull,
                           OutOfFaces,
                           OutOfVertices,
                           AccuraryReached,
                           FallBack,
                           Failed);
      pragma Unreferenced (Touching);


      type EPA is tagged
         record
            m_status   : eStatus_EPA;
            m_result   : sSimplex;
            m_normal   : math.Vector_3;
            m_depth    : math.Real;

            m_sv_store : sSV_array   (1 .. EPA_MAX_VERTICES);
            m_fc_store : sFace_array (1 .. EPA_MAX_FACES);

            m_nextsv   : U;

            m_hull     : sList;
            m_stock    : sList;
         end record;


      function to_EPA return EPA;

      procedure Initialize (Self : access EPA);

      function Evaluate (Self : in out EPA;   gjk   : in out gjkepa2_impl.GJK'Class;
                                              guess : in     math.Vector_3) return eStatus_EPA;


      function newface (Self : access EPA;   a, b, c : in sSV_view;
                                             forced  : in Boolean) return sFace_view;

      function findbest (Self : access EPA) return sFace_view;

      function expand (Self : access EPA;   pass    : in     U;
                                            w       : in     sSV_view;
                                            f       : in     sFace_view;
                                            e       : in     U;
                                            horizon : access sHorizon) return Boolean;





      --- Utility
      --

      procedure Initialize (shape0      : in     impact.d3.Shape.convex.view;
                            wtrs0       : in     Transform_3d;
                            shape1      : in     impact.d3.Shape.convex.view;
                            wtrs1       : in     Transform_3d;
                            results     :    out impact.d3.collision.gjk_epa.btGjkEpaSolver2.sResults;
                            shape       : in out tShape;
                            withmargins : in     Boolean);
   end gjkepa2_impl;









   package body gjkepa2_impl
   is





      ------------------
      --- MinkowskiDiff
      --


      procedure EnableMargin (Self : in out MinkowskiDiff;   enable : in Boolean)
      is
      begin
         if enable then
            Self.Ls := impact.d3.Shape.convex.localGetSupportVertexNonVirtual'Access;
         else
            Self.Ls := impact.d3.Shape.convex.localGetSupportVertexWithoutMarginNonVirtual'Access;
         end if;
      end EnableMargin;




      function  Support0     (Self : in     MinkowskiDiff;   d      : in math.Vector_3) return math.Vector_3
      is
      begin
         return Self.Ls (Self.m_shapes (1).all, d);
      end Support0;




      function  Support1     (Self : in     MinkowskiDiff;   d      : in math.Vector_3) return math.Vector_3
      is
         use linear_Algebra_3d, impact.d3.Transform, math.Vectors;
      begin
         return Self.m_toshape0  *  Self.Ls (Self.m_shapes (2).all,  Self.m_toshape1 * d);
      end Support1;




      function  Support      (Self : in     MinkowskiDiff;   d      : in math.Vector_3) return math.Vector_3
      is
      begin
         return Self.Support0 (d)  -  Self.Support1 (-d);
      end Support;



      function  Support      (Self : in     MinkowskiDiff;   d      : in math.Vector_3;
                                                             index  : in U          ) return math.Vector_3
      is
         use type Interfaces.Unsigned_64;
      begin
         if index /= 0 then                  -- tbd: '0' or '1' ?
            return Self.Support1 (d);
         else
            return Self.Support0 (d);
         end if;
      end Support;





      --------
      --- GJK
      --


      function  projectorigin (a, b       : access math.Vector_3;
                               w          : access math.Vector;
                               m          : access U) return math.Real;

      function  projectorigin (a, b, c    : access math.Vector_3;
                               w          : access math.Vector;
                               m          : access U) return math.Real;

      function  projectorigin (a, b, c, d : access math.Vector_3;
                               w          : access math.Vector;
                               m          : access U) return math.Real;


      --  Internals
      --

      procedure getsupport (Self : in GJK'Class;    d  : in     math.Vector_3;
                                                    sv :    out sSV)
      is
         use impact.d3.Vector, math.Vectors;
      begin
         sv.d := d / length (d);
         sv.w := Self.m_shape.Support (sv.d);
      end getsupport;



      function det (a, b, c : in math.Vector_3) return math.Real
      is
      begin
         return    a (2) * b (3) * c (1)  +  a (3) * b (1) * c (2)
                -  a (1) * b (3) * c (2)  -  a (2) * b (1) * c (3)
                +  a (1) * b (2) * c (3)  -  a (3) * b (2) * c (1);

      end det;







      procedure Initialize (Self : in out GJK)
      is
      begin
         Self.m_ray      := (0.0, 0.0, 0.0);
         Self.m_nfree    := 1;  -- or 0 ?
         Self.m_status   := Failed;
         Self.m_current  := 1;  -- or 0 ?
         Self.m_distance := 0.0;
      end Initialize;





      function to_GJK return GJK
      is
         Self : GJK;
      begin
         Self.Initialize;
         return Self;
      end to_GJK;






      function Evaluate (Self : access GJK;   shapearg : in tShape'Class;
                                              guess    : in math.Vector_3) return eStatus
      is
         use impact.d3.Vector;
         use type U;

         iterations : U         := 0;
         sqdist     : math.Real := 0.0;
         alpha      : math.Real := 0.0;
         lastw      : Vector_3_array (1 .. 4);
         clastw     : U         := 0;

         sqrl       : math.Real;

      begin
         --  Initialize solver
         --
         Self.m_free (1) := Self.m_store (1)'Unchecked_Access;
         Self.m_free (2) := Self.m_store (2)'Unchecked_Access;
         Self.m_free (3) := Self.m_store (3)'Unchecked_Access;
         Self.m_free (4) := Self.m_store (4)'Unchecked_Access;

         Self.m_nfree    := 5;  -- or 4 ?
         Self.m_current         := 1;  -- or 0 ?
         Self.m_status         := Valid;
         Self.m_shape         := tShape (shapearg);
         Self.m_distance := 0.0;

         --  Initialize simplex
         --
         Self.m_simplices (1).rank := 1;   -- or 0 ?
         Self.m_ray                := guess;

         sqrl                      := length2 (Self.m_ray);

         Self.appendvertice (Self.m_simplices (1)'Access,  (if sqrl > 0.0  then -Self.m_ray else (1.0, 0.0, 0.0)));

         Self.m_simplices (1).p (1) := 1.0;
         Self.m_ray                 := Self.m_simplices (1).c (1).w;
         sqdist                     := sqrl;
         lastw (1)                    := Self.m_ray;
         lastw (2)                    := Self.m_ray;
         lastw (3)                  := Self.m_ray;
         lastw (4)                    := Self.m_ray;


         --  Loop
         loop
            declare

               next  : constant U         := 3 - Self.m_current;   -- or 1 - m_current ?
               cs    : access   sSimplex  := Self.m_simplices (Integer (Self.m_current))'Access; -- or m_current+1 ?
               ns    : constant access   sSimplex  := Self.m_simplices (Integer (next        ))'Access; -- or next+1 ?
               --  Check zero
               rl    : constant math.Real := length (Self.m_ray);

               w     : math.Vector_3;
               found : Boolean;

               omega   : math.Real;
--                 weights : aliased math.Vector_4;
               weights : aliased math.Vector := (1 .. 4 => <>);
               mask    : aliased U;

            begin
               if rl < GJK_MIN_DISTANCE then   -- Touching or inside
                  Self.m_status := Inside;
                  exit;
               end if;


               --  Append new vertice in -'v' direction
               --
               Self.appendvertice (cs, -Self.m_ray);

               w     := cs.c (Integer (cs.rank - 1)).w;  -- or -0 ?
               found := False;


               for i in U'(1) .. 4
               loop
                  if length2 (w - lastw (Integer (i))) < GJK_DUPLICATED_EPS then
                     found := True;
                     exit;
                  end if;
               end loop;


               if found then                    -- Return old simplex
                  Self.removevertice (Self.m_simplices (Integer (Self.m_current))'Access); -- or m_current+1 ?
                  exit;
               else                             -- Update lastw
                  clastw                       := (clastw + 1)  and  3;
                  lastw (Integer (clastw) + 1) := w;
               end if;



               --  Check for termination
               --
               omega := dot (Self.m_ray, w) / rl;
               alpha := math.Real'Max (omega, alpha);

               if ((rl - alpha) - (GJK_ACCURARY * rl)) <= 0.0 then    -- Return old simplex
                  Self.removevertice (Self.m_simplices (Integer (Self.m_current))'Access);
                  exit;
               end if;



               --  Reduce simplex
               --
               mask := 0;

               case cs.rank
               is
               when 3 =>   sqdist := projectorigin (cs.c (1).w'Access,
                                                    cs.c (2).w'Access,
                                                    weights'Access,  mask'Access);
               when 4 =>   sqdist := projectorigin (cs.c (1).w'Access,
                                                    cs.c (2).w'Access,
                                                    cs.c (3).w'Access,
                                                    weights'Access,  mask'Access);
               when 5 =>   sqdist := projectorigin (cs.c (1).w'Access,
                                                    cs.c (2).w'Access,
                                                    cs.c (3).w'Access,
                                                    cs.c (4).w'Access,
                                                    weights'Access,  mask'Access);
               when others => null;
               end case;


               if sqdist >= 0.0 then   -- Valid
                  ns.rank        := 1;
                  Self.m_ray     := (0.0, 0.0, 0.0);
                  Self.m_current := next;

                  for i in 1 .. Integer (cs.rank - 1)
                  loop
                     if (mask and (2**(i - 1))) /= 0 then
                        ns.c (Integer (ns.rank)) := cs.c (i);
                        ns.p (Integer (ns.rank)) := weights (i);
                        ns.rank                    := ns.rank + 1;
                        Self.m_ray                 := Self.m_ray  +  cs.c (i).w * weights (i);
                     else
                        Self.m_free (Integer (Self.m_nfree)) := cs.c (i)'Access;
                        Self.m_nfree                         := Self.m_nfree + 1;
                     end if;
                  end loop;

                  if mask = 15 then
                     Self.m_status := Inside;
                  end if;

               else                  -- Return old simplex
                  Self.removevertice (Self.m_simplices (Integer (Self.m_current))'Access);
                  exit;
               end if;


               iterations    := iterations + 1;
               Self.m_status := (if iterations <= GJK_MAX_ITERATIONS then Self.m_status else Failed);

               exit when Self.m_status /= Valid;
            end;
         end loop;


         Self.m_simplex := Self.m_simplices (Integer (Self.m_current))'Unchecked_Access;

         case Self.m_status
         is
            when Valid  =>   Self.m_distance := length (Self.m_ray);
            when Inside =>   Self.m_distance := 0.0;
            when others =>   null;
         end case;


         return Self.m_status;
      end Evaluate;

--                                  /* Loop                                                */
--                                  do        {
--                                          const U                next=1-m_current;
--                                          sSimplex&        cs=m_simplices[m_current];
--                                          sSimplex&        ns=m_simplices[next];
--                                          /* Check zero                                                        */
--                                          const btScalar        rl=m_ray.length();
--                                          if(rl<GJK_MIN_DISTANCE)
--                                          {/* Touching or inside                                */
--                                                  m_status=eStatus::Inside;
--                                                  break;
--                                          }
--                                          /* Append new vertice in -'v' direction        */
--                                          appendvertice(cs,-m_ray);
--                                          const btVector3&        w=cs.c[cs.rank-1]->w;
--                                          bool                                found=false;
--                                          for(U i=0;i<4;++i)
--                                          {
--                                                  if((w-lastw[i]).length2()<GJK_DUPLICATED_EPS)
--                                                  { found=true;break; }
--                                          }
--                                          if(found)
--                                          {/* Return old simplex                                */
--                                                  removevertice(m_simplices[m_current]);
--                                                  break;
--                                          }
--                                          else
--                                          {/* Update lastw                                        */
--                                                  lastw[clastw=(clastw+1)&3]=w;
--                                          }
--                                          /* Check for termination                                */
--                                          const btScalar        omega=btDot(m_ray,w)/rl;
--                                          alpha=btMax(omega,alpha);
--                                          if(((rl-alpha)-(GJK_ACCURARY*rl))<=0)
--                                          {/* Return old simplex                                */
--                                                  removevertice(m_simplices[m_current]);
--                                                  break;
--                                          }
--                                          /* Reduce simplex                                                */
--                                          btScalar        weights[4];
--                                          U                        mask=0;
--                                          switch(cs.rank)
--                                          {
--                                          case        2:        sqdist=projectorigin(        cs.c[0]->w,
--                                                                          cs.c[1]->w,
--                                                                          weights,mask);break;
--                                          case        3:        sqdist=projectorigin(        cs.c[0]->w,
--                                                                          cs.c[1]->w,
--                                                                          cs.c[2]->w,
--                                                                          weights,mask);break;
--                                          case        4:        sqdist=projectorigin(        cs.c[0]->w,
--                                                                          cs.c[1]->w,
--                                                                          cs.c[2]->w,
--                                                                          cs.c[3]->w,
--                                                                          weights,mask);break;
--                                          }
--                                          if(sqdist>=0)
--                                          {/* Valid        */
--                                                  ns.rank                =        0;
--                                                  m_ray                =        btVector3(0,0,0);
--                                                  m_current        =        next;
--                                                  for(U i=0,ni=cs.rank;i<ni;++i)
--                                                  {
--                                                          if(mask&(1<<i))
--                                                          {
--                                                                  ns.c[ns.rank]                =        cs.c[i];
--                                                                  ns.p[ns.rank++]                =        weights[i];
--                                                                  m_ray                                +=        cs.c[i]->w*weights[i];
--                                                          }
--                                                          else
--                                                          {
--                                                                  m_free[m_nfree++]        =        cs.c[i];
--                                                          }
--                                                  }
--                                                  if(mask==15) m_status=eStatus::Inside;
--                                          }
--                                          else
--                                          {/* Return old simplex                                */
--                                                  removevertice(m_simplices[m_current]);
--                                                  break;
--                                          }
--                                          m_status=((++iterations)<GJK_MAX_ITERATIONS)?m_status:eStatus::Failed;
--                                  } while(m_status==eStatus::Valid);
--                                  m_simplex=&m_simplices[m_current];
--                                  switch(m_status)
--                                  {
--                                  case        eStatus::Valid:                m_distance=m_ray.length();break;
--                                  case        eStatus::Inside:        m_distance=0;break;
--                                  default:
--                                          {
--                                          }
--                                  }
--                                  return(m_status);






      ------ TBD: Check this whole file against C equiv.





      procedure appendvertice (Self : in out GJK;   simplex : access sSimplex;
                                                    v       : in     math.Vector_3)
      is
         use type interfaces.Unsigned_64;
      begin
         simplex.p (Integer (simplex.rank)) := 0.0;

         Self.m_nfree                         := Self.m_nfree  - 1;
         simplex.c (Integer (simplex.rank)) := Self.m_free (Integer (Self.m_nfree)).all;

         getsupport (Self,   v, simplex.c (Integer (simplex.rank)));

         simplex.rank := simplex.rank + 1;
      end appendvertice;



--                          void                                appendvertice(sSimplex& simplex,const btVector3& v)
--                          {
--                                  simplex.p[simplex.rank]=0;
--                                  simplex.c[simplex.rank]=m_free[--m_nfree];
--                                  getsupport(v,*simplex.c[simplex.rank++]);
--                          }



      procedure removevertice (Self : in out GJK;   simplex : access sSimplex)
      is
         use type U;
      begin
         simplex.rank                         := simplex.rank - 1;
         Self.m_free (Integer (Self.m_nfree)) := simplex.c (Integer (simplex.rank))'Access;
         Self.m_nfree                         := Self.m_nfree + 1;
      end removevertice;

--                          void                                removevertice(sSimplex& simplex)
--                          {
--                                  m_free[m_nfree++] = simplex.c[--simplex.rank];
--                          }






      function  projectorigin (a, b : access math.Vector_3;
                               w    : access math.Vector;
                               m    : access U) return math.Real
      is
         use impact.d3.Vector;

         d : constant math.Vector_3 := b.all - a.all;
         l : constant math.Real     := length2 (d);

         t :          math.Real;

      begin
         if l > GJK_SIMPLEX2_EPS then
            t := (if l > 0.0 then -dot (a.all, d) / l   else   0.0);

            if    t >= 1.0 then
               w (1) := 0.0;
               w (2) := 1.0;
               m.all := 2;

               return length2 (b.all);

            elsif t <= 0.0 then
               w (1) := 1.0;
               w (2) := 0.0;
               m.all := 1;

               return length2 (a.all);

            else
               w (2) := t;
               w (1) := 1.0 - w (2);
               m.all := 3;

               return length2 (a.all + d*t);
            end if;

         end if;


         return -1.0;
      end projectorigin;






      function  projectorigin (a, b, c : access math.Vector_3;
                               w       : access math.Vector;
                               m       : access U) return math.Real
      is
         use impact.d3.Vector, math.Vectors;
         use type U;

         imd3 : constant array (1 .. 3) of        U         := (1, 2, 0);
         vt   : constant array (1 .. 3) of access math.Vector_3 := (a, b, c);
         dl   : constant array (1 .. 3) of        math.Vector_3 := (a.all - b.all,
                                                                  b.all - c.all,
                                                                  c.all - a.all);
         n    : constant                        math.Vector_3 := cross (dl (1),  dl (2));

         l    : constant                        math.Real := length2 (n);

      begin
         if l > GJK_SIMPLEX3_EPS then
            declare
               mindist :         math.Real   := -1.0;
               subw    : aliased math.Vector := (1 .. 2 => 0.0);
               subm    : aliased U           := 0;

            begin
               for i in U'(1) .. 3
               loop
                  if dot (vt (Integer (i)).all,  cross (dl (Integer (i)), n))  >  0.0 then
                     declare
                        j    : constant U         := imd3 (Integer (i));
                        subd : constant math.Real := projectorigin (vt (Integer (i)),
                                                                    vt (Integer (j + 1)),
                                                                    subw'Access,
                                                                    subm'Access);
                     begin
                        if mindist < 0.0 or else subd < mindist then
                           mindist           := subd;
                           m.all           := U ( (if (subm and 1) /= 0 then   2**Integer (i - 1)   else   0)
                                                 +  (if (subm and 2) /= 0 then   2**Integer (j)   else   0));
                           w (Integer (i)) := subw (1);
                           w (Integer (j + 1)) := subw (2);

                           w (Integer (imd3 (Integer (j + 1)) + 1)) := 0.0;
                        end if;
                     end;
                  end if;
               end loop;


               if mindist < 0.0 then
                  declare
                     use math.Functions;

                     d : constant math.Real     := dot (a.all, n);
                     s : constant math.Real     := sqRt (l);
                     p : constant math.Vector_3 := n * (d / l);
                  begin
                     mindist := length2 (p);
                     m.all   := 7;                                              -- tbd: 7'7 or '8' ?
                     w (1)   := length (cross (dl (2),  b.all - p))  /  s;
                     w (2)   := length (cross (dl (3),  c.all - p))  /  s;
                     w (3)   := 1.0  -  (w (1) + w (2));
                  end;
               end if;


               return mindist;
            end;
         end if;


         return -1.0;
      end projectorigin;





      function  projectorigin (a, b, c, d : access math.Vector_3;
                               w          : access math.Vector;
                               m          : access U) return math.Real
      is
         use impact.d3.Vector;
         use type U;

         imd3    : constant array (1 .. 3) of        U             := (1, 2, 0);
         vt      : constant array (1 .. 4) of access math.Vector_3 := (a, b, c, d);
         dl      : constant array (1 .. 3) of        math.Vector_3 := (a.all - d.all,
                                                                     b.all - d.all,
                                                                     c.all - d.all);
         vl      :                                 math.Real     := det (dl (1),  dl (2),  dl (3));
         ng      : constant                        Boolean       := (vl  *  dot (a.all,  cross (b.all - c.all, a.all - b.all)))  <=  0.0;

         mindist : math.Real;
         subw    : aliased math.Vector := (1 .. 3 => <>);
         subm    : aliased U;

      begin
         if ng and then abs (vl) > GJK_SIMPLEX4_EPS
         then
            mindist := -1.0;
            subw    := (0.0, 0.0, 0.0);
            subm    := 0;

            for i in U'(1) .. 3
            loop
               declare
                  use math.Vectors;

                  j : constant U         := imd3 (Integer (i)) + 1;
                  s : constant math.Real := vl  *  dot (d.all,  cross (dl (Integer (i)),
                                                                       dl (Integer (j))));

                  subd : aliased math.Real;

               begin
                  if s > 0.0 then
                     subd := projectorigin (vt (Integer (i)),
                                            vt (Integer (j)),
                                            d,
                                            subw'Access, subm'Access);

                     if        mindist < 0.0
                       or else subd    < mindist
                     then
                        mindist                 := subd;
                        m.all                 := U ((if (subm and 1) /= 0 then 2**Natural (i) else 0)
                                               + (if (subm and 2) /= 0 then 2**Natural (j) else 0)
                                               + (if (subm and 4) /= 0 then 8              else 0));
                        w (Integer (i))        := subw (1);
                        w (Integer (j))        := subw (2);
                        w (Integer (imd3 (Integer (j)) + 1)) := 0.0;
                        w (4)           := subw (3);
                     end if;
                  end if;
               end;
            end loop;


            if mindist < 0.0 then
               mindist := 0.0;
               m.all   := 15;
               w (1)   := det (c.all, b.all, d.all) / vl;
               w (2)   := det (a.all, c.all, d.all) / vl;
               w (3)   := det (b.all, a.all, d.all) / vl;
               w (4)   := 1.0 - (w (1) + w (2) + w (3));
            end if;


            return mindist;
         end if;


         return -1.0;
      end projectorigin;





      function EncloseOrigin (Self : access GJK) return Boolean
      is
         use impact.d3.Vector;

         axis,
         d, p, n   : math.Vector_3;

      begin

         case Self.m_simplex.rank
         is
         when 2 =>

            for i in U'(1) .. 3
            loop
               axis := (0.0, 0.0, 0.0);
               axis (Integer (i)) := 1.0;
               Self.appendvertice (Self.m_simplex, axis);

               if Self.EncloseOrigin then
                  return True;
               end if;

               Self.removevertice (Self.m_simplex);
               Self.appendvertice (Self.m_simplex, -axis);

               if Self.EncloseOrigin then
                  return True;
               end if;

               Self.removevertice (Self.m_simplex);
            end loop;


         when 3 =>
            d := Self.m_simplex.c (2).w  -  Self.m_simplex.c (1).w;
            for i in U'(1) .. 3
            loop
               axis     := (0.0, 0.0, 0.0);
               axis (Integer (i)) := 1.0;
               p        := cross (d, axis);

               if length2 (p) > 0.0 then
                  Self.appendvertice (Self.m_simplex, p);

                  if Self.EncloseOrigin then
                     return True;
                  end if;

                  Self.removevertice (Self.m_simplex);
                  Self.appendvertice (Self.m_simplex, -p);

                  if Self.EncloseOrigin then
                     return True;
                  end if;

                  Self.removevertice (Self.m_simplex);
               end if;
            end loop;


         when 4 =>
            n := cross (Self.m_simplex.c (2).w - Self.m_simplex.c (1).w,
                        Self.m_simplex.c (3).w - Self.m_simplex.c (1).w);
            if length2 (n) > 0.0 then

               Self.appendvertice (Self.m_simplex, n);

               if Self.EncloseOrigin then
                  return True;
               end if;

               Self.removevertice (Self.m_simplex);
               Self.appendvertice (Self.m_simplex, -n);

               if Self.EncloseOrigin then
                  return True;
               end if;

               Self.removevertice (Self.m_simplex);
            end if;


         when 5 =>

            if abs (det (Self.m_simplex.c (1).w - Self.m_simplex.c (4).w,
                          Self.m_simplex.c (2).w - Self.m_simplex.c (4).w,
                          Self.m_simplex.c (3).w - Self.m_simplex.c (4).w)) > 0.0
            then
               return True;
            end if;


         when others =>
            raise Program_Error;

         end case;


         return False;
      end EncloseOrigin;








      --------
      --- EPA
      --


      function to_EPA return EPA
      is
         Self : aliased EPA;
      begin
         Self.Initialize;
         return Self;
      end to_EPA;





      procedure bind (fa : in sFace_view;   ea : in U;
                      fb : in sFace_view;   eb : in U)
      is
         use type U;
      begin
         fa.e (Integer (ea) + 1) := U1 (eb + 1);
         fa.f (Integer (ea) + 1) := fb;

         fb.e (Integer (eb) + 1) := U1 (ea + 1);
         fb.f (Integer (eb) + 1) := fa;
      end bind;




      procedure append (list : in out sList;
                        face : in     sFace_view)
      is
         use type U;
      begin
         face.l (1) := null;
         face.l (2) := list.root;

         if list.root /= null then
            list.root.l (1) := face;
         end if;

         list.root  := face;
         list.count := list.count + 1;
      end append;






      procedure remove (list : in out sList;
                        face : in     sFace_view)
      is
         use type U;

      begin
         if face.l (2) /= null then
            face.l (2).l (1) := face.l (1);
         end if;

         if face.l (1) /= null then
            face.l (1).l (2) := face.l (2);
         end if;

         if face = list.root then
            list.root := face.l (2);
         end if;

         list.count := list.count - 1;
      end remove;





      function newface (Self : access EPA;   a, b, c : in sSV_view;
                                             forced  : in Boolean) return sFace_view
      is
         use impact.d3.Vector;

         face : sFace_view;
         l    : math.Real;
         v    : Boolean;
      begin
         if Self.m_stock.root /= null then
            face := Self.m_stock.root;

            remove (Self.m_stock, face);
            append (Self.m_hull, face);

            face.pass  := 0;
            face.c (1) := a;
            face.c (2) := b;
            face.c (3) := c;
            face.n     := cross (b.w - a.w,  c.w - a.w);

            l          := length (face.n);
            v          := l > EPA_ACCURACY;

            face.p     := math.Real'Min (math.Real'Min (dot (a.w,  cross (face.n,  a.w - b.w)),
                                                        dot (b.w,  cross (face.n,  b.w - c.w))),
                                                        dot (c.w,  cross (face.n,  c.w - a.w)))  /  (if v then l else 1.0);
            face.p     := (if face.p >= -EPA_INSIDE_EPS then 0.0 else face.p);

            if v then
               face.d := dot (a.w, face.n) / l;
               face.n := face.n / l;

               if forced or else (face.d >= -EPA_PLANE_EPS) then
                  return face;
               else
                  Self.m_status := NonConvex;
               end if;

            else
               Self.m_status := Degenerated;
            end if;

            remove (Self.m_hull, face);
            append (Self.m_stock, face);

            return null;
         end if;


         Self.m_status := (if Self.m_stock.root /= null then OutOfVertices else OutOfFaces);

         return null;
      end newface;







      function findbest (Self : access EPA) return sFace_view
      is
--           use impact.d3.Vector, math.Vectors;

         minf : sFace_view := Self.m_hull.root;
         mind : math.Real  := minf.d * minf.d;
         maxp : math.Real  := minf.p;

         f    : sFace_view := minf.l (2);
         sqd  : math.Real;

      begin
         while f /= null
         loop
            sqd := f.d * f.d;

            if         f.p >= maxp
              and then sqd <  mind
            then
               minf := f;
               mind := sqd;
               maxp := f.p;
            end if;

            f := f.l (2);
         end loop;


         return minf;
      end findbest;







      function expand (Self : access EPA;   pass    : in     U;
                                            w       : in     sSV_view;
                                            f       : in     sFace_view;
                                            e       : in     U;
                                            horizon : access sHorizon) return Boolean
      is
         use impact.d3.Vector;
         use type U, U1;

         i1m3 : constant array (1 .. 3) of U := (1, 2, 0);
         i2m3 : constant array (1 .. 3) of U := (2, 0, 1);

         e1, e2 : U;
         nf     : sFace_view;

      begin
         if f.pass /= U1 (pass) then
            e1 := i1m3 (Integer (e)) + 1;

            if dot (f.n, w.w) - f.d  <  -EPA_PLANE_EPS then

               nf := Self.newface (f.c (Integer (e1)),  f.c (Integer (e)), w, False);

               if nf /= null then
                  bind (nf, 0,  f, e - 1);

                  if horizon.cf /= null then
                     bind (horizon.cf, 1,  nf, 2);
                  else
                     horizon.ff := nf;
                  end if;

                  horizon.cf := nf;
                  horizon.nf := horizon.nf + 1;

                  return True;
               end if;

            else

               e2     := i2m3 (Integer (e)) + 1;
               f.pass := U1 (pass);

               if          Self.expand (pass,  w,  f.f (Integer (e1)),  U (f.e (Integer (e1))),  horizon)
                 and then Self.expand (pass,  w,  f.f (Integer (e2)),  U (f.e (Integer (e2))),  horizon)
               then
                  remove (Self.m_hull,  f);
                  append (Self.m_stock, f);

                  return True;
               end if;
            end if;
         end if;


         return False;
      end expand;









      procedure Initialize (Self : access EPA)
      is
      begin
         Self.m_status := Failed;
         Self.m_normal := (0.0, 0.0, 0.0);
         Self.m_depth  := 0.0;
         Self.m_nextsv := 0;

         for i in 1 .. EPA_MAX_FACES
         loop
            append (Self.m_stock,
                    Self.m_fc_store (EPA_MAX_FACES - (i - 1))'Unchecked_Access);
         end loop;
      end Initialize;





      function Evaluate (Self : in out EPA;   gjk   : in out gjkepa2_impl.GJK'Class;
                                              guess : in     math.Vector_3) return eStatus_EPA
      is
         use type U;

         simplex : sSimplex renames gjk.m_simplex.all;
         tetra   : sFace_views (1 .. 4);

      begin
         if         simplex.rank > 1
           and then gjk.EncloseOrigin
         then
            --  Clean up
            --
            while Self.m_hull.root /= null
            loop
               declare
                  f : constant sFace_view := Self.m_hull.root;
               begin
                  remove (Self.m_hull,  f);
                  append (Self.m_stock, f);
               end;
            end loop;

            Self.m_status := Valid;
            Self.m_nextsv := 0;

            --  Orient simplex
            --
            if det (simplex.c (1).w - simplex.c (4).w,
                    simplex.c (2).w - simplex.c (4).w,
                    simplex.c (3).w - simplex.c (4).w) < 0.0
            then
               declare
                  Pad1 : constant sSV := simplex.c (1);
                  Pad2 : math.Real;
               begin
                  simplex.c (1) := simplex.c (2);   -- swap
                  simplex.c (2) := Pad1;

                  Pad2          := simplex.p (1);   -- swap
                  simplex.p (1) := simplex.p (2);
                  simplex.p (2) := Pad2;
               end;
            end if;

            --  Build initial hull
            --
            tetra := (Self.newface (simplex.c (1)'Access, simplex.c (2)'Access, simplex.c (3)'Access, True),
                      Self.newface (simplex.c (2)'Access, simplex.c (1)'Access, simplex.c (4)'Access, True),
                      Self.newface (simplex.c (3)'Access, simplex.c (2)'Access, simplex.c (4)'Access, True),
                      Self.newface (simplex.c (1)'Access, simplex.c (3)'Access, simplex.c (4)'Access, True));

            if Self.m_hull.count = 4 then
               declare
                  use impact.d3.Vector;

                  best       : sFace_view   := Self.findbest;
                  outer      : sFace        := best.all;
                  pass       : U            := 0;
                  iterations : U            := 0;
                  projection : math.Vector_3;
                  sum        : math.Real;
               begin
                  bind (tetra (1), 0, tetra (2), 0);
                  bind (tetra (1), 1, tetra (3), 0);
                  bind (tetra (1), 2, tetra (4), 0);
                  bind (tetra (2), 1, tetra (4), 2);
                  bind (tetra (2), 2, tetra (3), 1);
                  bind (tetra (3), 2, tetra (4), 1);

                  Self.m_status := Valid;

                  while iterations < EPA_MAX_ITERATIONS
                  loop
                     if Self.m_nextsv < EPA_MAX_VERTICES then
                        declare
                           horizon : aliased  sHorizon;
                           w       : constant sSV_view := Self.m_sv_store (Integer (Self.m_nextsv) + 1)'Unchecked_Access;
                           valid   : Boolean  := True;
                           wdist   : math.Real;
                        begin
                           Self.m_nextsv := Self.m_nextsv + 1;
                           pass          := pass + 1;
                           best.pass     := U1 (pass);
                           getsupport (gjk,  best.n, w.all);
                           wdist         := dot (best.n, w.w) - best.d;


                           if wdist > EPA_ACCURACY then

                              for j in U'(1) .. 3
                              loop
                                 valid := valid and Self.expand (pass,
                                                                 w,
                                                                 best.f (Integer (j)),
                                                                 U (best.e (Integer (j))),
                                                                 horizon'Access);
                                 exit when not Valid;
                              end loop;


                              if valid and then horizon.nf >= 3 then
                                 bind   (horizon.cf, 1,  horizon.ff, 2);
                                 remove (Self.m_hull,  best);
                                 append (Self.m_stock, best);

                                 best := Self.findbest;

                                 if best.p >= outer.p then
                                    outer := best.all;
                                 end if;

                              else
                                 Self.m_status := InvalidHull;
                                 exit;
                              end if;


                           else
                              Self.m_status := AccuraryReached;
                              exit;
                           end if;

                        end;

                     else
                        Self.m_status := OutOfVertices;
                        exit;
                     end if;


                     iterations := iterations + 1;
                  end loop;


                  projection         := outer.n * outer.d;

                  Self.m_normal             := outer.n;
                  Self.m_depth       := outer.d;
                  Self.m_result.rank := 3;
                  Self.m_result.c (1) := outer.c (1).all;
                  Self.m_result.c (2) := outer.c (2).all;
                  Self.m_result.c (3) := outer.c (3).all;
                  Self.m_result.p (1) := length (cross (outer.c (2).w - projection,
                                                       outer.c (3).w - projection));
                  Self.m_result.p (2) := length (cross (outer.c (3).w - projection,
                                                       outer.c (1).w - projection));
                  Self.m_result.p (3) := length (cross (outer.c (1).w - projection,
                                                       outer.c (2).w - projection));

                  sum                := Self.m_result.p (1) + Self.m_result.p (2) + Self.m_result.p (3);

                  Self.m_result.p (1) := Self.m_result.p (1) / sum;
                  Self.m_result.p (2) := Self.m_result.p (2) / sum;
                  Self.m_result.p (3) := Self.m_result.p (3) / sum;


                  return Self.m_status;
               end;
            end if;
         end if;


         --  Fallback
         --
         Self.m_status        := FallBack;
         Self.m_normal        := -guess;

         declare
            use impact.d3.Vector;
            nl : constant math.Real := length (Self.m_normal);
         begin
            if nl > 0.0 then
               Self.m_normal :=        Self.m_normal / nl;
            else
               Self.m_normal := (1.0, 0.0, 0.0);
            end if;
         end;

         Self.m_depth             := 0.0;
         Self.m_result.rank  := 1;
         Self.m_result.c (1) := simplex.c (1);
         Self.m_result.p (1) := 1.0;


         return Self.m_status;
      end Evaluate;





      procedure Initialize (shape0      : in     impact.d3.Shape.convex.view;
                            wtrs0       : in     Transform_3d;
                            shape1      : in     impact.d3.Shape.convex.view;
                            wtrs1       : in     Transform_3d;
                            results     :    out impact.d3.collision.gjk_epa.btGjkEpaSolver2.sResults;
                            shape       : in out tShape;
                            withmargins : in     Boolean)
      is
         use impact.d3.Transform, impact.d3.Matrix;

      begin
         --  Results
         --
         results.witnesses (1) := (0.0, 0.0, 0.0);
         results.witnesses (2) := (0.0, 0.0, 0.0);
         results.status        := impact.d3.collision.gjk_epa.btGjkEpaSolver2.Separated;

         --  Shape
         --
         shape.m_shapes (1) := shape0;
         shape.m_shapes (2) := shape1;

         shape.m_toshape1 := transposeTimes (getBasis (wtrs1),  getBasis (wtrs0));
         shape.m_toshape0 := inverseTimes (wtrs0, wtrs1);

         shape.EnableMargin (withmargins);
      end Initialize;




   end gjkepa2_impl;











   package body btGjkEpaSolver2
   is

      use gjkepa2_impl;




      function StackSizeRequirement return Integer
      is
      begin
         return (GJK'Size + EPA'Size) / 8;
      end StackSizeRequirement;





      function Distance (shape0  : in     impact.d3.Shape.convex.view;
                         wtrs0   : in     Transform_3d;
                         shape1  : in     impact.d3.Shape.convex.view;
                         wtrs1   : in     Transform_3d;
                         guess   : in     math.Vector_3;
                         results : access sResults) return Boolean
      is
         shape : tShape;
      begin
         gjkepa2_impl.Initialize (shape0, wtrs0, shape1, wtrs1, results.all, shape, False);

         declare
            use linear_Algebra_3d, impact.d3.Vector, impact.d3.Transform;

            gjk        : aliased gjkepa2_impl.GJK;
            gjk_status : constant gjkepa2_impl.eStatus    := gjk.Evaluate (shape, guess);
            w0, w1     : math.Vector_3;
            p          : math.Real;

         begin
            if gjk_status = Valid then
               w0 := (0.0, 0.0, 0.0);
               w1 := (0.0, 0.0, 0.0);

               for i in U'(1) .. gjk.m_simplex.rank
               loop
                  p := gjk.m_simplex.p (Integer (i));

                  w0 := w0  +  shape.Support (gjk.m_simplex.c (Integer (i)).d,  0) * p;
                  w1 := w1  +  shape.Support (-gjk.m_simplex.c (Integer (i)).d,  1) * p;
               end loop;

               results.witnesses (1) := wtrs0 * w0;
               results.witnesses (2) := wtrs0 * w1;
               results.normal       := w0 - w1;
               results.distance     := length (results.normal);
               results.normal       := results.normal / (if results.distance > GJK_MIN_DISTANCE then results.distance else 1.0);

               return True;

            else
               results.status := (if gjk_status = Inside then Penetrating else GJK_Failed);

               return False;
            end if;
         end;
      end Distance;






      function Penetration (shape0     : in     impact.d3.Shape.convex.view;
                            wtrs0      : in     Transform_3d;
                            shape1     : in     impact.d3.Shape.convex.view;
                            wtrs1      : in     Transform_3d;
                            guess      : in     math.Vector_3;
                            results    : access sResults;
                            usemargins : in     Boolean := True) return Boolean
      is
         shape : tShape;
      begin
         gjkepa2_impl.Initialize (shape0, wtrs0, shape1, wtrs1, results.all, shape, usemargins);

         declare
            gjk        : aliased gjkepa2_impl.GJK;
            gjk_status : constant gjkepa2_impl.eStatus := gjk.Evaluate (shape, -guess);

         begin

            case gjk_status
            is
            when Inside =>
               declare
                  use impact.d3.Transform, linear_Algebra_3d;

                  epa        : gjkepa2_impl.EPA := to_EPA;
                  epa_status : constant eStatus_EPA     := epa.Evaluate (gjk, -guess);
                  w0         : math.Vector_3;
               begin
                  if epa_status /= Failed then
                     w0 := (0.0, 0.0, 0.0);

                     for i in U'(1) .. epa.m_result.rank
                     loop
                        w0 := w0 + shape.Support (epa.m_result.c (Integer (i)).d, 0) * epa.m_result.p (Integer (i));
                     end loop;

                     results.status        :=  Penetrating;
                     results.witnesses (1) :=  wtrs0 * w0;
                     results.witnesses (2) :=  wtrs0 * (w0 - epa.m_normal*epa.m_depth);
                     results.normal        := -epa.m_normal;
                     results.distance      := -epa.m_depth;

                     return True;

                  else
                     results.status := EPA_Failed;
                  end if;
               end;


            when Failed =>
               results.status := GJK_Failed;


            when others =>
               null;
            end case;


            return False;
         end;
      end Penetration;







      function SignedDistance (position  : in     math.Vector_3;
                               margin    : in     math.Real;
                               shape0    : in     impact.d3.Shape.convex.view;
                               wtrs0     : in     Transform_3d;
                               results   : access sResults) return math.Real
      is
         shape  : tShape;
         shape1 : aliased impact.d3.Shape.convex.internal.sphere.item := impact.d3.Shape.convex.internal.sphere.to_sphere_Shape (margin);
         wtrs1  : constant Transform_3d   := impact.d3.Transform.to_Transform (impact.d3.Quaternions.to_Quaternion (0.0, 0.0, 0.0, 1.0), position);

      begin
         Initialize (shape0, wtrs0, shape1'Unchecked_Access, wtrs1, results.all, shape, False);

         declare
            use linear_Algebra_3d, impact.d3.Transform, impact.d3.Vector;
            gjk        : aliased gjkepa2_impl.GJK;
            gjk_status : constant gjkepa2_impl.eStatus := gjk.Evaluate (shape, (1.0, 1.0, 1.0));

            w0, w1    : math.Vector_3;
            the_delta : math.Vector_3;

            margin,
            length,
            p         : math.Real;

         begin

            if gjk_status = Valid then
               w0 := (0.0, 0.0, 0.0);
               w1 := (0.0, 0.0, 0.0);

               for i in U'(1) .. gjk.m_simplex.rank
               loop
                  p := gjk.m_simplex.p (Integer (i));
                  w0 := w0  +  shape.Support (gjk.m_simplex.c (Integer (i)).d, 0)  * p;
                  w1 := w1  +  shape.Support (-gjk.m_simplex.c (Integer (i)).d, 1)  * p;
               end loop;

               results.witnesses (1) := wtrs0 * w0;
               results.witnesses (2) := wtrs0 * w1;

               the_delta             := results.witnesses (2) - results.witnesses (1);
               margin                := shape0.getMarginNonVirtual + shape1.getMarginNonVirtual;
               length                := impact.d3.Vector.length (the_delta);

               results.normal        := the_delta / length;
               results.witnesses (1) := results.witnesses (1)  +  results.normal * margin;

               return length - margin;

            else
               if gjk_status = Inside then

                  if Penetration (shape0, wtrs0, shape1'Unchecked_Access, wtrs1, gjk.m_ray, results) then
                     the_delta := results.witnesses (1) - results.witnesses (2);
                     length    := impact.d3.Vector.length (the_delta);

                     if length >= impact.d3.Scalar.SIMD_EPSILON then
                        results.normal := the_delta / length;
                     end if;

                     return -length;
                  end if;

               end if;
            end if;


            return impact.d3.Scalar.SIMD_INFINITY;
         end;
      end SignedDistance;








      function SignedDistance (shape0     : in     impact.d3.Shape.convex.view;
                               wtrs0      : in     Transform_3d;
                               shape1     : in     impact.d3.Shape.convex.view;
                               wtrs1      : in     Transform_3d;
                               guess      : in     math.Vector_3;
                               results    : access sResults) return Boolean
      is
      begin
         if not Distance (shape0, wtrs0, shape1, wtrs1, guess, results) then
            return Penetration (shape0, wtrs0, shape1, wtrs1, guess, results, False);
         else
            return True;
         end if;
      end SignedDistance;



   end btGjkEpaSolver2;




end impact.d3.collision.gjk_epa;
