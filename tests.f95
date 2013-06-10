program tests
   !use varlist_mod
   !use lineparser_mod
   use mod_initprm
   implicit none
   character(len=20):: str='aaa',filename='data.dat'
   logical:: flag=.false.
   integer:: n=1, setstat
   real:: pi=3.
   double precision:: sq2=1.

   print*
   print*,'DEFAULT VALUES:'
   print*,'filename=       ',trim(filename)
   print*,'LOGICAL flag=   ',flag
   print*,'CHARACTER str=  ',trim(str)
   print*,'INTEGER n=      ',n
   print*,'REAL pi=        ',pi
   print*,'DBLE sq2=       ',sq2
   
   call iprm_readfile("filename",filename)
   call iprm_set("n", n, n,setstat)
   print*,'setstat: ', setstat
   call iprm_set("flag",flag, flag)
   call iprm_set("str",str, str)
   call iprm_set("pi", pi, pi)
   call iprm_set("sq2", sq2, sq2)
   

   print*
   print*,'FINAL VALUES:'
   print*,'filename=       ',trim(filename)
   print*,'LOGICAL flag=   ',flag
   print*,'CHARACTER str=  ',trim(str)
   print*,'INTEGER n=      ',n
   print*,'REAL pi=        ',pi
   print*,'DBLE sq2=       ',sq2

end program tests
