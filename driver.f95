program tests
   !use varlist_mod
   !use lineparser_mod
   use initparams_mod
   implicit none
   character(len=20):: str='Boa noite',filename='data.dat'
   logical:: flag=.false.
   integer:: n=3
   real:: pi=3.14159
   double precision:: sq2=sqrt(2.d0)

   print*
   print*,'DEFAULT VALUES:'
   !print*,'LOGICAL flag=   ',flag
   !print*,'CHARACTER str=  ',trim(str)
   print*,'filename=       ',trim(filename)
   print*,'INTEGER n=      ',n
   !print*,'REAL pi=        ',pi
   !print*,'DBLE sq2=       ',sq2
   
   call ip_parseinputfile("filename",filename,filename)
   call ip_setparam("n", n, n)
   call ip_setparam("flag",flag, flag)
   call ip_setparam("str",str, str)
   call ip_setparam("pi", pi, pi)
   call ip_setparam("sq2", sq2, sq2)
   

   print*
   print*,'FINAL VALUES:'
   print*,'LOGICAL flag=   ',flag
   print*,'CHARACTER str=  ',trim(str)
   print*,'INTEGER n=      ',n
   print*,'REAL pi=        ',pi
   print*,'DBLE sq2=       ',sq2
   print*,'filename=       ',trim(filename)

end program tests
