c---------------------------------------------------------------------
c  Parameter lm (declared and set in "npbparams.h") is the log-base2 of 
c  the edge size max for the partition on a given node, so must be changed 
c  either to save space (if running a small case) or made bigger for larger 
c  cases, for example, 512^3. Thus lm=7 means that the largest dimension 
c  of a partition that can be solved on a node is 2^7 = 128. lm is set 
c  automatically in npbparams.h
c  Parameters ndim1, ndim2, ndim3 are the local problem dimensions. 
c---------------------------------------------------------------------

      include 'npbparams.h'

      integer nm      ! actual dimension including ghost cells for communications
     >      , nv      ! size of rhs array
     >      , nr      ! size of residual array
     >      , nm2     ! size of communication buffer
     >      , maxlevel! maximum number of levels

      parameter( nm=2+2**lm, nv=(2+2**ndim1)*(2+2**ndim2)*(2+2**ndim3) )
      parameter( nm2=2*nm*nm, maxlevel=11 )
      parameter( nr = (8*(nv+nm**2+5*nm+7*lm))/7 )  
c---------------------------------------------------------------------
      integer  nx(maxlevel),ny(maxlevel),nz(maxlevel)
      common /mg3/ nx,ny,nz

      character class
      common /ClassType/class

      integer debug_vec(0:7)
      common /my_debug/ debug_vec

      integer ir(maxlevel), m1(maxlevel), m2(maxlevel), m3(maxlevel)
      integer lt, lb
      common /fap/ ir,m1,m2,m3,lt,lb

c---------------------------------------------------------------------
c  Set at m=1024, can handle cases up to 1024^3 case
c---------------------------------------------------------------------
      integer m
c      parameter( m=1037 )
      parameter( m=nm+1 )

c      double precision buff(nm2,4)
c      common /buffer/ buff

      logical timeron
      common /timers/ timeron
      integer T_init, T_bench, T_psinv, T_resid, T_rprj3, T_interp,
     >        T_norm2, T_mg3P, T_resid2, T_last
      parameter (T_init=1, T_bench=2, T_mg3P=3,
     >        T_psinv=4, T_resid=5, T_resid2=6, T_rprj3=7,
     >        T_interp=8, T_norm2=9, T_last=9)


