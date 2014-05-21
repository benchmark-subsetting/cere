












































































c=======================================================================
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
      subroutine hdfall(filename)
c
c  PURPOSE: Makes an hdf dump of all the active field variables.  The
c  set of field variables dumped is problem specific (depends on what
c  physics is defined).  Data is written in the Scientific Data Set
c  format to the file zhzXXNNNNNN.MMM.
c  Note that data must be stored column major and contiguously in order
c  to interface correctly to the C hdf routines.  All variables are
c  dumped as zone centered quantities.
c
c  EXTERNALS: HDF library routines
c
c  LOCALS:
c
c  LAST MODIFIED: by JCH; 3/12/97.
c-----------------------------------------------------------------------
      implicit NONE
c
      character*15 filename

      return
      end
