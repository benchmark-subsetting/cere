c CLASS = B
c  
c  
c  This file is generated automatically by the setparams utility.
c  It sets the number of processors and the class of the NPB
c  in this directory. Do not modify it by hand.
c  
        integer nx, ny, nz, maxdim, niter_default, ntotal
        parameter (nx=128, ny=64, nz=64, maxdim=128)
        parameter (niter_default=20)
        parameter (ntotal=nx*ny*nz)
        logical  convertdouble
        parameter (convertdouble = .false.)
        character*11 compiletime
        parameter (compiletime='22 Nov 2013')
        character*3 npbversion
        parameter (npbversion='3.0')
        character*3 cs1
        parameter (cs1='lec')
        character*3 cs2
        parameter (cs2='lel')
        character*6 cs3
        parameter (cs3='(none)')
        character*6 cs4
        parameter (cs4='(none)')
        character*3 cs5
        parameter (cs5='-O3')
        character*10 cs6
        parameter (cs6='-lgfortran')
        character*6 cs7
        parameter (cs7='randi8')
