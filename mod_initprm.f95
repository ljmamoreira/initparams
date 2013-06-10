module mod_initprm
   use iso_fortran_env
   implicit none
   private
   public:: iprm_set, iprm_readfile
   !initparams vars
   integer:: maxlinelen=200
   logical, save:: first_invoc = .true.
   logical:: debugmode=.false.
   interface iprm_set
      module procedure setvar_log
      module procedure setvar_chr
      module procedure setvar_int
      module procedure setvar_r32
      module procedure setvar_r64
      module procedure setvar_r128
   end interface
   !varlist vars
   integer, parameter:: vl_varnamlen=63
   integer, parameter:: vl_varvallen=200
   type:: varnode_type
      character(len=vl_varnamlen):: varname
      character(len=vl_varvallen):: varvalue
      type(varnode_type), pointer:: next => null()
   end type varnode_type
   
   type(varnode_type), pointer, save:: varlist_head => null()


contains

!-------------------------------------------------------------------------------
!init_params

subroutine setvar_log(varname, var, default_val, op_stat)
   logical, intent(out):: var
   logical, intent(in):: default_val
   character(len=*), parameter:: procname='setvar_log'
   include 'iprm_set-patch.f95'
end subroutine setvar_log

subroutine setvar_chr(varname, var, default_val, op_stat)
   character(len=*), intent(out)::  var
   character(len=*), intent(in)::   default_val
   character(len=*), parameter:: procname='setvar_chr'
   include 'iprm_set-patch.f95'
end subroutine setvar_chr

subroutine setvar_int(varname, var, default_val, op_stat)
   integer, intent(out)::           var
   integer, intent(in)::            default_val
   character(len=*), parameter:: procname='setvar_int'
   include 'iprm_set-patch.f95'
end subroutine setvar_int

subroutine setvar_r32(varname, var, default_val, op_stat)
   real(real32), intent(out)::      var
   real(real32), intent(in)::       default_val
   character(len=*), parameter:: procname='setvar_r32'
   include 'iprm_set-patch.f95'
end subroutine setvar_r32

subroutine setvar_r64(varname, var, default_val, op_stat)
   real(real64), intent(out)::      var
   real(real64), intent(in)::       default_val
   character(len=*), parameter:: procname='setvar_r64'
   include 'iprm_set-patch.f95'
end subroutine setvar_r64

subroutine setvar_r128(varname, var, default_val, op_stat)
   real(real128), intent(out)::     var
   real(real128), intent(in)::      default_val
   character(len=*), parameter:: procname='setvar_r128'
   include 'iprm_set-patch.f95'
end subroutine setvar_r128

subroutine iprm_readfile(fnvarname,default_val)
   character(len=*), intent(in)::   fnvarname, default_val
   integer, parameter:: filenamelen = 20
   integer::                        read_status
   character(len=vl_varvallen)::    value_str
   character(len=filenamelen):: filename
   logical:: fname_specified
   if (debugmode) print*,'Entered ip_parseinputfile.'
   if (debugmode) print*,'  Args: ',trim(fnvarname),'; ',trim(default_val)
   read_status = 0
   if (first_invoc) call init()
   value_str = vl_getvalue(fnvarname)
   if (value_str == 'UNDEFINED') then
      filename = default_val
      fname_specified = .false.
   else
      filename = value_str
      fname_specified = .true.
   endif
   call parsefile(filename, fname_specified)
   if (debugmode) print*,'Leaving ip_parseinputfile.'
end subroutine

subroutine init()
   character(len=maxlinelen):: line
   if (debugmode) print*, 'Entered init.'
   first_invoc = .false.
   call get_command(line)
   call lp_parseline(line)
   if (debugmode) then
      call vl_print()
      print*, 'Leaving init.'
   endif
end subroutine init

subroutine parsefile(filename, fname_specified)
   character(len=*),intent(in):: filename
   logical, intent(in):: fname_specified
   character(len=maxlinelen):: line
   integer:: ioerr
   if (debugmode) print*, 'Entered parsefile.'
   if (debugmode) print*,'  Args: ',trim(filename),'; ',fname_specified
   open(10, file=filename, status='old', action='read', iostat=ioerr)
   if (ioerr == 0) then
      do
         read(10,'(A)',iostat=ioerr) line
         if (ioerr < 0) exit
         call lp_parseline(line)
      end do
      close(10)
   else
      if (debugmode) print*, 'Data file not found (',trim(filename),').'
      if (fname_specified) &
         print*,'Warning: parameter file specified can''t be found (',&
         trim(filename),').'
   endif
   if (debugmode) print*, 'Leaving parsefile.'
end subroutine parsefile

!------------------------------------------------------------------------------
! Line parser (prefix: lp_)

subroutine lp_parseline(line)
   character(len=*), intent(in):: line
   character(len=1):: cchar, cquote_char
   integer, parameter:: QERR=-2, ERR=-1, NRML=0, LHS=1, &
                        LHST=2,  RHSS=3, RHS=4,  QUOT=5
   character(len=vl_varnamlen):: varnam
   character(len=vl_varvallen):: varval
   character(len=100):: errmsg
   integer:: state, ptr, start, finish, line_len
!
   if (debugmode) print*, 'Entered lp_parseline.'
   state = NRML
   cquote_char = ''
   line_len = len_trim(line)
   do ptr = 1, line_len
      cchar = line(ptr: ptr)
      select case (state)
         case (QERR) ! -----------------
            if (cchar == cquote_char) then
               state = NRML
               cquote_char = ''
            end if
         case (ERR)  ! -----------------
            if (cchar == ' ') then
               state = NRML
            else if (is_quote(cchar)) then
               cquote_char = cchar
               state = QERR
            end if
         case (NRML) ! -----------------
            if (is_letter(cchar)) then
               start = ptr
               state = LHS
            else if (cchar == ' ') then
               continue
            else if (is_quote(cchar)) then
               cquote_char = cchar
               state = QERR
            else
               state = ERR
               write(errmsg, '("Ilegal identifier found at char# ",i2)') ptr
            end if
         case (LHS)  ! -----------------
            if (is_quote(cchar)) then
               cquote_char = cchar
               state = QERR
            else if (cchar == ' ') then
               finish = ptr - 1
               varnam = line(start:finish)
               state = LHST
            else if (cchar =='=') then
               finish = ptr - 1
               varnam = line(start:finish)
               state = RHSS
               varnam = line(start:finish)
            else if (.not.(is_ftnchar(cchar))) then
               state = ERR
               write(errmsg, '("Ilegal identifier found at char# ",i2)') ptr
            end if
         case (LHST) ! -----------------
            if (cchar == '=') then
               state = RHSS
            else if (cchar /= ' ') then
               state = ERR
               write(errmsg, '("Ilegal identifier found at char# ",i2)') ptr
            end if
         case (RHSS) ! -----------------
            if (cchar == ' ') then
               continue
            else if (ptr == line_len) then
               finish = ptr
               start = ptr
               varval = line(start:finish)
               call vl_insert(varnam, varval)
            else if (is_quote(cchar)) then
               state = QUOT
               cquote_char = cchar
               start = ptr
            else
               state = RHS
               start = ptr
            end if
         case (RHS)  ! -----------------
            if (is_quote(cchar)) then
               state = QERR
               cquote_char = cchar
            else if (cchar == ' ')  then
               state = NRML
               finish = ptr - 1
               varval = line(start:finish)
               call vl_insert(varnam,varval)
            else if (cchar == '=') then
               state = ERR
            else if (ptr == line_len) then
               finish = ptr
               varval = line(start:finish)
               call vl_insert(varnam,varval)
            end if
         case (QUOT) ! -----------------
            if (cchar == cquote_char) then
               state = NRML
               finish = ptr
               varval = line(start:finish)
               call vl_insert(varnam,varval)
            else if (ptr == line_len) then
               state = ERR
            end if
      end select
   enddo
   if (debugmode) print*, 'Leaving lp_parseline.'
end subroutine lp_parseline

function is_quote(c)
   logical:: is_quote
   character(len=1), intent(in):: c
   is_quote = (scan(c, '"''') > 0)
end function is_quote

function is_letter(c)
   logical:: is_letter
   character(len=1), intent(in):: c
   character(len=52), parameter:: &
      LETTERS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
   if (index(LETTERS, c)>0) then
      is_letter = .true.
   else
      is_letter = .false.
   endif
end function is_letter

function is_ftnchar(c)
   character(len=1), intent(in):: c
   logical:: is_ftnchar
   character(len=11), parameter:: digits='0123456789_'
   is_ftnchar = (is_letter(c) .or. (index(digits, c)>0))
end function is_ftnchar

!------------------------------------------------------------------------------
! Varlist     (prefix: vl_)

function vl_isdefined(varname) result(return_val)
! Returns True if varname is present in varlist, False otherwise
! Not sure if it is needed
   character(len=*), intent(in):: varname
   logical:: return_val
   type(varnode_type), pointer:: ptr => null()
   logical:: defined
   if (debugmode) print*, 'Entered vl_isdefined.'
   ptr => varlist_head
   do
      if (associated(ptr)) print*,'|',trim(varname),'|',trim(ptr%varname),'|',&
         varname==ptr%varname
      if (.not.associated(ptr)) then !Empty list
         defined = .false.
         exit
      else if (ptr%varname == varname) then
         defined = .true.
         exit
      endif
      ptr => ptr%next
   enddo
   return_val = defined
   if (debugmode) print*, 'Leaving vl_isdefined. Return value: ',defined,'.'
end function vl_isdefined

subroutine vl_insert(varname, varvalue)
!Inserts node in varlist if varname isn't present, updates its varvalue
!otherwise. The list is kept ordered alphabetically
   character(len=*), intent(in):: varname
   character(len=*), intent(in):: varvalue
   type(varnode_type), pointer:: ptr, previous, new
   if (debugmode) print*, 'Entered vl_insert.'
   nullify(ptr, previous, new)
   ptr => varlist_head
   do
      if (.not.associated(ptr)) then 
         !End of list reached. Node not present, store it at the end
         allocate(new)
         new%varname = varname
         new%varvalue = varvalue
         if(.not.associated(varlist_head)) then
            varlist_head => new
         else
            previous%next => new
         endif
         exit
      else if (ptr%varname == varname) then
         !Node present, don't update varvalue (uncomment next line for update)
         !ptr%varvalue = varvalue
         exit
      else if (lgt(ptr%varname, varname)) then
         !Located alphabetical position of node. Insert it n the list
         allocate(new)
         new%varname = varname
         new%varvalue = varvalue
         new%next => ptr
         if (associated(previous)) then
            previous%next => new
         else !Put it at the top
            varlist_head => new
         endif
         exit
      end if
      previous => ptr
      ptr => ptr%next
   end do
   if (debugmode) print*, 'Leaving vl_insert.'
end subroutine vl_insert

function vl_index(varname) result(node)
!Returns a pointer to the node storing varname if such a node exists in varlist;
!otherwise, returns a pointer to null()
   character(len=*), intent(in):: varname
   type(varnode_type), pointer :: node, ptr
   nullify(node)
   ptr => varlist_head
   do
      if (.not.associated(ptr)) then
         node => null()
         exit
      else if (ptr%varname == varname) then
         node => ptr
         exit
      else
         ptr => ptr%next
      endif
   end do
end function vl_index
   
function vl_getvalue(varname) result(varvalue)
!Returns the varvalue pair of varnode if such a pair exists in varlist;
!otherwise, returns "UNDEFINED"
   character(len=*), intent(in):: varname
   character(len=vl_varvallen):: varvalue
   type(varnode_type), pointer:: ptr
   ptr => vl_index(varname)
   if (associated(ptr)) then
      varvalue = ptr%varvalue
   else
      varvalue = 'UNDEFINED'
   endif
end function vl_getvalue

subroutine vl_print()
   type(varnode_type), pointer:: ptr
   character(len=vl_varnamlen):: nextname
   if (debugmode) print*, 'Entered vl_print.'
   ptr => varlist_head
   print*
   print*,'Varlist table:'
   do
      if (.not.associated(ptr)) exit
      if (associated(ptr%next)) then
         nextname = ptr%next%varname
      else
         nextname = "END OF VARLIST"
      end if
      print*,trim(adjustl(ptr%varname)),': ',trim(ptr%varvalue), &
         & ' => ',trim(adjustl(nextname))
      ptr => ptr%next
   end do
   print*
   if (debugmode) print*, 'Leaving vl_print.'
end subroutine vl_print
   

end module
