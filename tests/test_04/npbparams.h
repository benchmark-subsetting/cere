c CLASS = B
c  
c  
c  This file is generated automatically by the setparams utility.
c  It sets the number of processors and the class of the NPB
c  in this directory. Do not modify it by hand.
c  
        integer problem_size, niter_default
        parameter (problem_size=10, niter_default=2)
        double precision dt_default
        parameter (dt_default = 0.0003d0)
        logical  convertdouble
        parameter (convertdouble = .false.)
        character*11 compiletime
        parameter (compiletime='10 Oct 2013')
        character*3 npbversion
        parameter (npbversion='3.0')
        character*8 cs1
        parameter (cs1='gfortran')
        character*5 cs2
        parameter (cs2='clang')
        character*6 cs3
        parameter (cs3='(none)')
        character*6 cs4
        parameter (cs4='(none)')
        character*6 cs5
        parameter (cs5='(none)')
        character*6 cs6
        parameter (cs6='(none)')
        character*6 cs7
        parameter (cs7='randi8')
