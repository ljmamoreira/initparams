   character(len=*), intent(in)::   varname
   integer, intent(out), optional:: op_stat
   integer::                        read_status
   character(len=vl_varvallen)::    value_str

   if (debugmode) print*, 'Entered ',procname,'.'
   read_status = 0
   if (first_invoc) call init()
   value_str = vl_getvalue(varname)
   if (value_str /= 'UNDEFINED') then
      read(value_str, *, iostat=read_status) var
      if ((read_status > 0).and.(.not.present(op_stat))) then
         print*,'Type error setting ',trim(varname),'. Aborting.'
         stop
      endif
   else
      var = default_val
   end if
   if (present(op_stat)) op_stat = read_status
   if (debugmode) print*, 'Leaving setvar_chr.'
