program tests
   !use varlist_mod
   !use lineparser_mod
   use initparams_mod
   implicit none
   character(len=20):: str='aaa',filename='data.dat'
   logical:: flag=.false.
   integer:: n=1
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
   
   call ip_parseinputfile("filename",filename,filename)
   call ip_setparam("n", n, n)
   call ip_setparam("flag",flag, flag)
   call ip_setparam("str",str, str)
   call ip_setparam("pi", pi, pi)
   call ip_setparam("sq2", sq2, sq2)
   

   print*
   print*,'FINAL VALUES:'
   print*,'filename=       ',trim(filename)
   print*,'LOGICAL flag=   ',flag
   print*,'CHARACTER str=  ',trim(str)
   print*,'INTEGER n=      ',n
   print*,'REAL pi=        ',pi
   print*,'DBLE sq2=       ',sq2

end program tests
