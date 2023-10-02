MODULE PARAMETERS
  use constants
  implicit none
  
  !!!nx,ny,nz is the grid size.  Set nz=1 for 2D.
  !ONLY CURRENTLY WORKS FOR EVEN NX AND NY
  !CAN NOT DO 1X1X1
  integer(int12), parameter :: inx=2 ,iny=2 ,inz=2 !33x33x33 hard limit for banded matrix (cube)
  integer(int12), parameter :: scale = 1
  integer(int12), parameter :: nx = inx*scale,ny=iny*scale,nz=inz*scale

  real(real12), parameter :: room = 293.0

  !!!Verbosity of output, IVERB=1, verbose but slow, IVERB=0, no outputs!
  integer, parameter :: IVERB=1
  
  !!!iboundary selects boundary typ1
  !=1 is outer heat bath
  !=2 is surrounding with varying heat baths (not implemented)
  !=3 is outer cells are heat baths
  !=4 periodic in z direction
  integer(int12), parameter :: iboundary=1


  !!!icell_mix decides what you want to do over the boundary of a cell
  !=1 take the minimal thermal conductivity of the two cells
  !=2 take the thermal conductivity average of the two cells
  !=3 take the cells thermal conductivity (not implemented!)
  integer(int12), parameter :: icell_mix=2

  !!!System config, introduced to allow users to add different geometries
  !0= uniform
  !1-100=Thermoacoustic
  !101-200=bee
  !301-400=tissue
  !400+=something else!
  integer(int12), parameter :: syscfg = 20 !16 for silver device

  !!!Temperature of surroundings (Kelvin)
  real(real12), parameter :: T_bath=room
  real(real12), parameter :: T_bath_1x=room 
  real(real12), parameter :: T_bath_nx=room
  real(real12), parameter :: T_bath_1y=room
  real(real12), parameter :: T_bath_ny=room
  real(real12), parameter :: T_bath_1z=room
  real(real12), parameter :: T_bath_nz=room
  
  !!!time step, in seconds
  real(real12), parameter :: time_step= 1.0  !!! If this timestep is too large it will cause sign flipping of final temperature results.


  !!! Number of time steps (is sequential)
  integer(int12), parameter :: ntime=20

  !!!power density input, in Wm-3  
  real(real12), parameter :: power_in= 1.0 !Remember its a real not an integer

!!!time cutoff for power (check if you are using a case that implements it
  integer(int12), parameter :: cutoff = 1e12


  !!!Periodic power input (What does this mean?)
  real(real12), parameter :: T_period=1.0

  !!!Gaussian broadening for gaussians, currently not used
  real(real12), parameter :: sigma=0.025

 

  !!!Temperature of surroundings using vectors (Kelvin)
  real(real12), dimension(3,3), parameter :: T_bath_varied=room


  !!!Choice of various heating routines, see heater.f90 for documentation
  !1= continuous heat source
  !2 = central square oscillating
  !3 = central square heating for 10 time steps
  !4 = laser fired on pig skin
  !10 = hot bee
  !11 = Heated silver wire (HARDCODED), 13 = AC
  integer(int12), parameter :: iheater = 20


!!!Choice of whether to use relativistic correction, 1=yes, 0=no
  integer(int12),parameter :: Rel = 0

  !Z layer to be output in Temperature.txt
  integer(int12),parameter :: zpos = 1

!!!Frequency for AC signal (Test)
  real(real12), parameter :: freq = (1e0)/(2*pi)
  real(real12), parameter :: om = 2*pi*freq

  !Choice for AC (1 = yes)
  integer(int12),parameter :: ACon = 0
end MODULE PARAMETERS

! mcmodel = medium
