!=====================================================
!This program calculates the average of dipole moments
!/ polarizability of the dippol.dat file
!then substract it in order to calculate
!delta M = M - <M>
!and
!delta A = A - <A>
!The input file can be split into several parts
!=====================================================
program delta
  implicit none

  !Parameter
  integer, parameter:: nb_h_ln=5 !Number of header lines before the adding of <A> and <M>
  
  !Variables
  integer:: i=0, j=0, err=0
  double precision, dimension(4):: av=0.d0,dum=0.d0
  character(len=200):: chain=""
  character(len=200), dimension(nb_h_ln):: header=""
  logical:: file_exist=.false.
  integer, dimension(:), allocatable:: nb_line
  double precision, dimension(:,:), allocatable:: data
  
  if(iargc()==0)then
     write(*,*)"You have to specify at least the name of one dippol file"
     write(*,*)
     write(*,*)"!!! END OF PROGRAM !!!"
     call exit()
  end if
  allocate(nb_line(iargc()))
  
  !==================================
  !Opening the dippol file one by one
  !then summation of their values
  !==================================
  av(:)=0.d0
  do i=1,iargc()
     call getarg(i,chain)
     inquire(file=chain, exist=file_exist)
     if(.not. file_exist)then 
        write(*,*)'!!!File ', trim(chain),' does not exist!!!'
     else
        open(11,file=chain,action='read')
        !Header
        do j=1,nb_h_ln
           read(11,*)
        enddo
        !Summation of the values
        read(11,*,iostat=err)dum(1),dum(2),dum(3),dum(4)
        nb_line(i)=0
        do while(err==0)
           nb_line(i)=nb_line(i)+1
           !WORK ARBEIT TRAVAIL
           !CHANGE THE SIGN OF DIP AND POL ACCORDING
           !TO THEIR Z AXIS
           !YOU WILL HAVE TO CHANGE THE SFG CODE TO
           !NOT DO THIS OPERATION TWICE
           
           av(1)=av(1)+dum(1)
           av(2)=av(2)+dum(2)
           av(3)=av(3)+dum(3)
           av(4)=av(4)+dum(4)
           read(11,*,iostat=err)dum(1),dum(2),dum(3),dum(4)
        end do
        close(11)
     endif
  end do

  !Normalization of <M> and <A>
  av(:)=av(:)/sum(nb_line)
  
  !===================================
  !Opening the dippol file one by one
  !then rewriting the good dippol_file
  !which is the initial one with one
  !extra line
  !===================================
  do i=1,iargc()
     call getarg(i,chain)
     inquire(file=chain, exist=file_exist)
     if(.not. file_exist)then 
        write(*,*)'!!!File ', trim(chain),' does not exist!!!'
     else

        !Record all the lines
        open(11,file=chain,action='read')
        do j=1,nb_h_ln
           read(11,'(A)')header(j)
        enddo
        allocate(data(4,nb_line(i)))
        do j=1,nb_line(i)
           read(11,*)data(1,j),data(2,j),data(3,j),data(4,j)
        end do
        close(11)

        !Write the file with the extra line
        open(11,file='delta_' // trim(chain),action='write')
        do j=1,nb_h_ln
           write(11,*)trim(header(j))
        enddo
        do j=1,nb_line(i)
           write(11,"(4f16.8)") data(:,j)-av(:)
        end do
        deallocate(data)
        close(11)
     endif
  end do

end program delta
