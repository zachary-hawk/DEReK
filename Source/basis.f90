!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
module basis
  use trace, only : trace_entry, trace_exit,trace_stack,trace_finalise,dp
  use io,    only : stdout, current_params,current_structure,pi,hartree_to_ev
  use memory,only : memory_allocate,memory_deallocate
  use comms, only : comms_scheme,dist_gvec
  implicit none




  type  basis_dat
     ! Standard grid
     integer                             :: ngx,ngy,ngz ! Standard grid dimensions
     integer                             :: num_grid_points
     integer                             :: num_node
     integer                             :: max_node
     real(dp),dimension(:,:),allocatable :: grid_points
     integer   ,dimension(:),allocatable :: local_grid_points ! A Map 
     real(dp),dimension(:,:),allocatable :: real_grid_points
     integer   ,dimension(:),allocatable :: local_real_grid_points ! Map 
     real(dp),dimension(:,:),allocatable :: frac_points

     ! Fine grid
     integer                             :: fine_ngx,fine_ngy,fine_ngz ! Fine grid dimensions
     integer                             :: num_fine_grid_points
     integer                             :: num_fine_node
     integer                             :: max_fine_node
     real(dp),dimension(:,:),allocatable :: fine_grid_points
     integer   ,dimension(:),allocatable :: local_fine_grid_points  ! A map not the actual points
     real(dp),dimension(:,:),allocatable :: real_fine_grid_points
     integer   ,dimension(:),allocatable :: local_real_fine_grid_points ! A map not a the points
     real(dp),dimension(:,:),allocatable :: fine_frac_points
  end type basis_dat


  type(basis_dat),public,save :: current_basis

  ! PUBLIC ROUTINES 
  public  basis_init



contains

  subroutine basis_init()
    !==============================================================================!
    !                             B A S I S _ I N I T                              !
    !==============================================================================!
    ! Subroutine for initialising the basis grids                                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  30/12/2021                                            !
    !==============================================================================!

    implicit none
    
    real(dp)   :: k_cut   ! the kspace cutoff readius
    integer    :: ngz,ngy,ngx,ig=0

    real(dp),dimension(1:3) :: temp_vec

    call trace_entry('basis_init')


    k_cut = sqrt(2.0_dp*current_params%cut_off_energy)
    current_basis%ngx = ceiling(k_cut*current_structure%lattice_a/pi)
    current_basis%ngy = ceiling(k_cut*current_structure%lattice_b/pi)
    current_basis%ngz = ceiling(k_cut*current_structure%lattice_c/pi)


    current_basis%fine_ngx = ceiling(current_params%g_fine_scale*k_cut*current_structure%lattice_a/pi)
    current_basis%fine_ngy = ceiling(current_params%g_fine_scale*k_cut*current_structure%lattice_b/pi)
    current_basis%fine_ngz = ceiling(current_params%g_fine_scale*k_cut*current_structure%lattice_c/pi)


    ! Make them fft compatible
    
    call basis_prime_fact(current_basis%ngx)
    call basis_prime_fact(current_basis%ngy)
    call basis_prime_fact(current_basis%ngz)
    call basis_prime_fact(current_basis%fine_ngx)
    call basis_prime_fact(current_basis%fine_ngy)
    call basis_prime_fact(current_basis%fine_ngz)
    current_basis%num_grid_points=current_basis%ngx*&
         & current_basis%ngy *&
         & current_basis%ngz
    current_basis%num_fine_grid_points=current_basis%fine_ngx*&
         & current_basis%fine_ngy *&
         & current_basis%fine_ngy
    

    
    !Allocate the grids
    call memory_allocate(current_basis%grid_points,1,current_basis%num_grid_points,1,3,"B")
    call memory_allocate(current_basis%fine_grid_points,1,current_basis%num_fine_grid_points,1,3,"B")
    call memory_allocate(current_basis%real_grid_points,1,current_basis%num_grid_points,1,3,"B")
    call memory_allocate(current_basis%real_fine_grid_points,1,current_basis%num_fine_grid_points,1,3,"B")
    call memory_allocate(current_basis%frac_points,1,current_basis%num_grid_points,1,3,"B")
    call memory_allocate(current_basis%fine_frac_points,1,current_basis%num_fine_grid_points,1,3,"B")

    ! Standard grid
    ig=0
    do ngx=-current_basis%ngx/2, current_basis%ngx - current_basis%ngx/2 -1 
       do ngy=-current_basis%ngy/2, current_basis%ngy - current_basis%ngy/2  -1        
          do ngz=-current_basis%ngz/2, current_basis%ngz - current_basis%ngz/2 -1
             ig=ig+1
             ! Set the temp vector to be the integer basis of vectors
             temp_vec(:)=(/real(ngx,dp),real(ngy,dp),real(ngz,dp)/)
             current_basis%grid_points(ig,:) = matmul(current_structure%inv_cell,temp_vec)
          end do
       end do
    end do

    ig=0

    ! Fine grid
    do ngx=-current_basis%fine_ngx/2, current_basis%fine_ngx - current_basis%fine_ngx/2 -1 
       do ngy=-current_basis%fine_ngy/2, current_basis%fine_ngy - current_basis%fine_ngy/2 -1        
          do ngz=-current_basis%fine_ngz/2, current_basis%fine_ngz - current_basis%fine_ngz/2 -1
             ig=ig+1
             ! Set the temp vector to be the integer basis of vectors
             temp_vec(:)=(/real(ngx,dp),real(ngy,dp),real(ngz,dp)/)
             current_basis%fine_grid_points(ig,:) = matmul(current_structure%inv_cell,temp_vec)
          end do
       end do
    end do


    ! Real Grids

    ! Standard grid
    ig=0
    do ngx=1,current_basis%ngx
       do ngy=1,current_basis%ngy
          do ngz=1,current_basis%ngz
             ig=ig+1
             ! Set the temp vector to be the integer basis of vectors

             temp_vec(:)=(/real(ngx,dp)*1.0_dp/real(current_basis%ngx,dp),&
                  & real(ngy,dp)*1.0_dp/real(current_basis%ngy,dp),&
                  & real(ngz,dp)*1.0_dp/real(current_basis%ngz,dp)/)

             current_basis%real_grid_points(ig,:) = matmul(current_structure%cell,temp_vec)
             current_basis%frac_points(ig,:)=temp_vec
          end do
       end do
    end do

    ig=0

    ! Fine grid
    do ngx=1,current_basis%fine_ngx
       do ngy=1,current_basis%fine_ngy
          do ngz=1,current_basis%fine_ngz
             ig=ig+1
             ! Set the temp vector to be the integer basis of vectors
             temp_vec(:)=(/real(ngx,dp)*1.0_dp/real(current_basis%fine_ngx,dp),&
                  & real(ngy,dp)*1.0_dp/real(current_basis%fine_ngy,dp),&
                  & real(ngz,dp)*1.0_dp/real(current_basis%fine_ngz,dp)/)
             
             current_basis%real_fine_grid_points(ig,:) = matmul(current_structure%cell,temp_vec)
             current_basis%fine_frac_points(ig,:)=temp_vec
          end do
       end do
    end do




    ! Write out the basis set information to the .derek
15  format(T2,a,T36,":",T58,a12)   ! Character                                                                                                                                                          
16  format(T2,a,T36,":",T58,i12)   ! Integer
21  format(T2,a,T36,":",T56,3(i4,1x))   ! Integer vec
17  format(T2,a,T36,":",T58,f12.3)   ! Real                                                                                                                                                             
18  format(T2,a,T36,":",T58,ES12.2)   ! Science                                                                                                                                                         
19  format(T2,a,T36,":",T58,L12)   ! Logical 

    write(stdout,*)
    write(stdout,*)"                          Basis Set Parameters"
    write(stdout,*)"                          --------------------"
    write(stdout,17)"Plane wave cut off (eV)",current_params%cut_off_energy*hartree_to_ev
    write(stdout,17)"G vector fine scale",current_params%g_fine_scale
    write(stdout,16)"Number of standard grid points",current_basis%num_grid_points
    write(stdout,16)"Number of fine grid points",current_basis%num_fine_grid_points
    write(stdout,21)"Standard FFT grid",current_basis%ngx,current_basis%ngy,current_basis%ngz
    write(stdout,21)"Fine FFT grid",current_basis%fine_ngx,current_basis%fine_ngy,current_basis%fine_ngz




    call comms_scheme(current_structure%num_kpoints,current_basis%num_grid_points,stdout,.true.)

    ! Now that the scheme has been determined we calculate the max numbers and distribute

    current_basis%max_fine_node=ceiling(real(current_basis%num_fine_grid_points,dp)/real(dist_gvec,dp))
    current_basis%max_node=ceiling(real(current_basis%num_grid_points,dp)/real(dist_gvec,dp))

    call memory_deallocate(current_basis%local_grid_points,'B')
    call memory_deallocate(current_basis%local_fine_grid_points,'B')
    call memory_allocate(current_basis%local_grid_points,1,current_basis%max_node,'B')
    call memory_allocate(current_basis%local_fine_grid_points,1,current_basis%max_fine_node,'B')
    


    call trace_exit('basis_init')
    return
  end subroutine basis_init


  subroutine basis_prime_fact(N)
    integer,intent(inout) :: N

    integer :: N_p,N_p_old,N_tot
    integer :: i,j,k,p,max_cycles=100
    integer,dimension(1:6) :: primes=(/2,3,5,7,11,13/)
    integer,dimension(1:6) :: factors 
    call trace_entry("basis_prime_fact")


    do k=1,max_cycles
       N_p=N
       factors(:)=0
       do j=1,max_cycles
          N_p_old=N_p
          do i=1,size(primes)
             ! the current prime
             p=primes(i)
             if (mod(N_p,p).eq.0)then
                N_p=N_p/p
                factors(i)=factors(i)+1
             end if
          end do
          if (N_p_old .eq. N_p)then
             exit
          end if

       end do

       ! Now we have the factors, we can check if it all adds up!
       !print*,"Factors of",N,":",factors
       N_tot=product(primes**factors)
       if (N_tot.eq.N)then
          exit
       else
          N=N+1
       end if

    end do

    call trace_exit("basis_prime_fact")
    return
  end  subroutine basis_prime_fact

end module basis
