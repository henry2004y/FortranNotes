subroutine readParam(iconf,conf)

   ! Input parameters
   integer iconf(5)
   real*8  conf(2)
   integer ios
   ! Open 'param' file
   open(unit=1,file='param',iostat=ios,status='old')
   if (ios.ne.0) then
     print *,'Error opening ''param'' file - IOSTAT = ',ios
     stop
   endif
   ! Read parameters
   read(1,*)
   read(1,*) iconf(1)
   read(1,*)
   read(1,*) iconf(2)
   read(1,*)
   read(1,*) iconf(3)
   read(1,*)
   read(1,*) iconf(4)
   read(1,*)
   read(1,*) iconf(5)
   read(1,*)
   read(1,*) conf(1)
   read(1,*)
   read(1,*) conf(2)
   ! Close 'param' file
   close(1)
   return
end
