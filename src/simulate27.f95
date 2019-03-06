! ******************************************************************************
! Optimal Financial Knowledge and Wealth Inequality
! Lusardi, Michaud and Mitchell (2015, JPE)
! uses model compiled from know27.f95
! computes results for figures and tables of paper
! *****************************************************************************

module simulate
	use sorting

	implicit none
	character*80 scenario
	character*80 path

	! parameters to set 
	double precision beta, gamma, pi0, pi1, cmin, k0, k1, phi, delta0, delta1, psi,eta, theta, omega, rhoe
	double precision amin, amax, fmin, fmax, inflation, rmax, rbar, sigr, alpha0, alpha1, sigrmax		
	integer T, ny, no, nr, ne, nd, nf, na, nsim, retage
	! switches for scenarios
	double precision issgen, ifin
	integer idifmx, idifeq, idifss, inofin, inotech, icagetti, igparker, izin, igenes

	! person for simulated data
	type person
        integer id
        double precision age
        double precision value
        double precision cons
        double precision wealth
        double precision income
        double precision oop
        double precision rate
        double precision inv
        double precision fin
        integer educ
        double precision share
    end type person

	! baseline results are global
	type (person), allocatable:: pop_baseline(:)
	integer nobs_baseline

	! welfare
	double precision, allocatable :: mort(:,:), eqs(:,:)
       
	contains
		subroutine initialize(scn)
			character*80 scn
			integer i, e, s
			scenario = scn
			path = '/users/michaud/perso/emetrics/know'		
			write(*,*) '+ running scenario : ', scenario
			
			write(*,*) '	* compiling main program (need mpif90) ...'
			call system(trim(path)//'build/compile.sh')
			
			! define baseline parameters
			
			! dimensions
			T = 75
			ny = 7
			no = 7
			nr = 7
			ne = 3
			nd = 2
			nf =  25
			na = 35
			nsim = 5000

			! other parameters
			amin = 0.0d0
			amax = 2.0d6
			fmin = 0.0d0
			fmax = 100.0d0
			inflation = 0.0d0
			rmax = 0.04d0
			rbar = 0.02d0
			sigr = 0.16d0
			retage = 40
			sigrmax = sigr
			rhoe = 0.0d0
						
			! preferences and technology
			beta = 0.96d0		
			gamma = 1.6d0
			pi0 = 50.0d0
			pi1 = 1.75d0
			cmin = 10000.0d0
			k0 = 750.0d0
			k1 = 0.0d0
			phi = 0.0d0
			delta0 = -0.06d0
			delta1 = 0.0d0
			alpha1 = 1.0d0
			alpha0 = rmax/(fmax**alpha1)
			psi = 0.0d0
			eta = gamma
			theta = (sigr - sigrmax)/fmax
			
			! other scenario specific parameters
			
			issgen = 1.0d0
			ifin = 0.0d0	
			idifmx = 1
			idifeq = 1
			idifss = 1			 
			inofin = 0
			inotech = 0
            icagetti = 0
            igparker = 0
            izin = 0
            igenes = 0

			! policy simulations
			if (scenario .eq. 'ssgen') then
				issgen = 0.8d0
			end if
			if (scenario .eq. 'lowfloor') then
				cmin = 5.0d3
			end if
			if (scenario .eq. 'initfin') then
			    inofin = 1
			end if	
			! wealth decomposition scenarios
			if (scenario .eq. 'stripdown') then
				inofin = 1
				inotech = 1
				cmin = 1000.0d0
				idifmx = 0
				idifeq = 0
				idifss = 0
			end if			
			if (scenario .eq. 'hsz') then
				inofin = 1
				inotech = 1
				idifmx = 0
				idifeq = 0
				idifss = 0
			end if			
			if (scenario .eq. 'reprate') then
				inofin = 1
				inotech = 1
				idifmx = 0
				idifeq = 0
			end if		
			if (scenario .eq. 'demo') then
				inofin = 1
				inotech = 1
				inofin = 1				
				idifmx = 0
			end if		
			if (scenario .eq. 'mortality') then
				inofin = 1
				inotech = 1
			end if	
			if (scenario .eq. 'genes')	then	
				igenes = 1
				pi0    = 1.0d3
				delta0 = 0.0d0
			end if				
			if (scenario .eq. 'hetpref') then
				inofin = 1
				icagetti = 1
			end if	
			if (scenario .eq. 'hetpref2') then
				icagetti = 1
			end if	
			if (scenario .eq. 'drra1') then
				phi = 0.75d0
			end if
			if (scenario .eq. 'drra2') then
				phi = 0.9d0
			end if
			if (scenario .eq. 'learning1') then
			    pi0 = 1.0d3
				psi = 5.0d0
			end if
			if (scenario .eq. 'learning2') then
			    pi0 = 1.0d3
				psi = 10.0d0
			end if
			if (scenario .eq. 'ssgen-learn') then
				issgen = 0.8d0
				pi0 = 1.0d3
				psi = 10.0d0
			end if
			if (scenario .eq. 'lowfloor-learn') then
				cmin = 5.0d3
				pi0 = 1.0d3
				psi = 10.0d0
			end if			
			if (scenario .eq. 'diverse') then
				sigrmax = 0.25d0 
				theta = (sigr-sigrmax)/fmax
			end if	
			if (scenario .eq. 'zin') then
			    izin = 1
				eta = 1.6d0
				gamma = 4.0d0
			end if	
			if (scenario .eq. 'zinstrip') then
			    izin = 1
				eta = 1.6d0
				gamma = 4.0d0
				inofin = 1
				inotech = 1
				cmin = 1000.0d0
				idifmx = 0
				idifeq = 0
				idifss = 0				
			end if		
			if (scenario .eq. 'zinmortality') then
			    izin = 1
				eta = 1.6d0
				gamma = 4.0d0
				inofin = 1
				inotech = 1				
			end if			
			! sensitivity scenarios										
			if (scenario .eq. 'sens1') then
				gamma = 1.2d0
			end if	
			if (scenario .eq. 'sens2') then
				gamma = 2.0d0
			end if
			if (scenario .eq. 'sens3') then
				delta0 = -0.03d0
			end if
			if (scenario .eq. 'sens4') then
				delta0 = -0.09d0
			end if
			if (scenario .eq. 'sens5') then
				pi0 = 25.0d0
			end if
			if (scenario .eq. 'sens6') then
				pi0 = 75.0d0
			end if
			if (scenario .eq. 'sens7') then
				pi1 = 1.25d0
			end if
			if (scenario .eq. 'sens8') then
				pi1 = 2.0d0
			end if
			if (scenario .eq. 'sens9') then
				k0 = 1000.0d0
			end if
			if (scenario .eq. 'sens10') then
				k0 = 500.0d0
			end if
			if (scenario .eq. 'sens11') then
				beta = 0.95d0
			end if
			if (scenario .eq. 'sens12') then
				beta = 0.97d0
			end if
			if (scenario .eq. 'sens13') then
				alpha1 = 0.9d0
				alpha0 = rmax/(fmax**alpha1)
			end if
			if (scenario .eq. 'sens14') then
				alpha1 = 0.75d0
				alpha0 = rmax/(fmax**alpha1)
			end if
			if (scenario .eq. 'sens15') then
				alpha1 = 0.5d0
				alpha0 = rmax/(fmax**alpha1)
			end if
			
			if (scenario .eq. 'adhoc') then
			    inofin = 1
			    !inotech = 1
			    phi = 0.8d0
			end if
			    		
			! writing scenario parameters to file
			open(unit=1, file=trim(path)//'params/scenario-parameters.asc')		
			write(1,*) scenario	
			write(1,*) issgen
			write(1,*) ifin	
			write(1,*) idifmx 
			write(1,*) idifeq 
			write(1,*) idifss 	
			write(1,*) inofin
			write(1,*) inotech		
			write(1,*) icagetti
			write(1,*) igparker 
			write(1,*) izin
			write(1,*) igenes
			close(1)
				 	
			! writing to file preference and technology parameters	
			open(unit=1, file=trim(path)//'params/structural-parameters.asc')
			write(1,*) beta
			write(1,*) gamma
			write(1,*) pi0
			write(1,*) pi1
			write(1,*) cmin
			write(1,*) k0
			write(1,*) k1
			write(1,*) phi
			write(1,*) delta0
			write(1,*) delta1
			write(1,*) alpha0
			write(1,*) alpha1	
			write(1,*) psi	
			write(1,*) eta
			write(1,*) theta
			close(unit=1)
			! writing other parameters 
			open(unit=1, file=trim(path)//'params/other-parameters.asc')
			write(1,*) amin
			write(1,*) amax
			write(1,*) fmin
			write(1,*) fmax
			write(1,*) inflation
			write(1,*) rmax
			write(1,*) rbar
			write(1,*) sigr
			write(1,*) retage
			write(1,*) sigrmax
			write(1,*) rhoe
			close(unit=1)
			! writing  dimensions 
			open(unit=1, file=trim(path)//'params/dimensions-parameters.asc')
			write(1,*) T
			write(1,*) ny
			write(1,*) no
			write(1,*) nr
			write(1,*) ne
			write(1,*) nd
			write(1,*) nf		
			write(1,*) na
			write(1,*) nsim
			close(unit=1)	
			
			
			! load elements needed for welfare computations
			if (scenario .eq. 'baseline') then
				allocate(mort(T,ne))
					open(2,file=trim(path)//'params/mortality-par.csv') 
				! reading header
				read(2,*)
				do i =  1 ,T, 1 
					mort(i,:) = 0.0d0
					read(2,*) mort(i,:)
					if (idifmx.eq.0) then	
						do e= 1, ne, 1
							mort(i,e) = mort(i,2)
						end do
					end if
				end do
				close(2) 
			
			! retrieve eq scale 
				allocate(eqs(T,ne))
				open(1,file=trim(path)//'params/equivalence-par.csv')
				read(1,*)
				do s = 1, T, 1
					read(1,*) eqs(s,:)
				end do
				close(1)
			end if
					
		end subroutine initialize

		subroutine draws
			double precision rdraws(nsim,T), mdraws(nsim,T), &
					odraws(nsim,T), ydraws(nsim,T), u
			integer i, s
			
			do i = 1, nsim, 1
				do s = 1, T, 1
					call random_number(u)
					rdraws(i,s) = u
					call random_number(u)
					mdraws(i,s) = u
					call random_number(u)
					odraws(i,s) = u
					call random_number(u)
					ydraws(i,s) = u
				end do
			end do
			
			open(1,file=trim(path)//'data/simulations/rdraws.dat')
			do i = 1, nsim, 1
				write(1,*) rdraws(i,:)
			end do
			close(1)
			open(1,file=trim(path)//'data/simulations/odraws.dat')
			do i = 1, nsim, 1
				write(1,*) odraws(i,:)
			end do
			close(1)
			open(1,file=trim(path)//'data/simulations/ydraws.dat')
			do i = 1, nsim, 1
				write(1,*) ydraws(i,:)
			end do
			close(1)
			open(1,file=trim(path)//'data/simulations/mdraws.dat')
			do i = 1, nsim, 1
				write(1,*) mdraws(i,:)
			end do
			close(1)
								
					
		end subroutine draws

		
		subroutine runsim(pop,nobs,irun)
			type (person), allocatable, target :: pop(:)
			integer i, nobs, irun
			if (irun.eq.1) then
			    write(*,*) '	* running simulation (need mpi) ...'
			    write(*,*) ''
			    call system('mpirun -machinefile mf -n 60 '//trim(path)//'build/run')
			end if
			open(1,file=trim(path)//'data/simulations/simknow_'//trim(scenario)//'.dat')
			read(1,*) nobs
			allocate(pop(nobs))
			do i = 1, nobs, 1
				read(1,*) pop(i)%id, pop(i)%age, pop(i)%value, pop(i)%cons, pop(i)%wealth &
					, pop(i)%income, pop(i)%oop, pop(i)%rate, pop(i)%inv, pop(i)%fin, &
					pop(i)%educ, pop(i)%share
			end do
			close(1)		
			! produce log for online appendix
			call runlog(pop,nobs)
		end subroutine runsim
				
		subroutine get_modulename
			write(*,*) '***********************************************************'
			write(*,*) '* Optimal knowledge model (Lusardi, Michaud and Mitchell) *'
			write(*,*) '  final version (JPE): simulation of scenarios										   '
			write(*,*) '***********************************************************'
		end subroutine get_modulename

		! routine to produce log for appendix
		subroutine runlog(pop,nobs)
			integer nobs, a, e, i, n
			type (person) pop(nobs)
			integer nageg, nstats
			integer, allocatable :: ageg(:)
			real, allocatable :: stats(:,:,:)
			double precision temp(nobs)
			character*80 fmt
			! selected ages for log
			nageg = 9
			allocate(ageg(nageg))
			ageg = (/35,40,45,50,55,60,65,70,75/)

			! number of stats
			nstats = 5

			! allocate for results
			allocate(stats(ne,nageg,nstats))

			do e = 1, ne, 1

				do a = 1, nageg, 1
					! median wealth
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. dble(ageg(a)) &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pop(i)%wealth					
						end if	
					end do				
					stats(e,a,1) = real(median(temp,n))

					! average fin knowledge
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. dble(ageg(a))  &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pop(i)%fin					
						end if	
					end do				
					stats(e,a,2) = real(mean(temp,n))	

					! participation in sophisticated technology
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. dble(ageg(a))  &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							if (pop(i)%share .gt. 0.0d0) then
								temp(n) = 1.0d0
							else
								temp(n) = 0.0d0
							end if 					
						end if	
					end do	
					stats(e,a,3) = real(mean(temp,n))	

					! conditional share in sophisticated technology
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. dble(ageg(a))  &
							.and. pop(i)%educ .eq. e) then			
							if (pop(i)%share .gt. 0.0d0) then
								n = n + 1
								temp(n) = pop(i)%share		
							end if 					
						end if	
					end do			
					if (n.gt.1) then	
						stats(e,a,4)  = real(mean(temp,n))
					else
						stats(e,a,4)  = 0.0
					end if

					! average consumption
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. dble(ageg(a))  &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pop(i)%income					
						end if	
					end do				
					stats(e,a,5) = real(mean(temp,n))	



				end do

			end do


			! with stats, create latex table
			fmt = '(I2,A3,F9.2,A3,F9.2,A3,F9.2,A3,F9.2,A3,F9.2,A4)'
			open(1,file=trim(path)//'tex/appendix_'//trim(scenario)//'.tex')
			write(1,*) '\begin{tabular}{lccccc}'
			write(1,*) '\hline \hline'
			write(1,*) ' age & wealth & knowledge & participation & share & income \\'
			write(1,*) '\hline'
			write(1,*) 'Less HS & & & & & \\'
			write(1,*) '\hline'
			do a = 1, nageg, 1
				write(1,fmt) ageg(a), ' & ', stats(1,a,1),' & ',stats(1,a,2),' & ',stats(1,a,3),' & ', &
					stats(1,a,4),' & ', stats(1,a,5),' \\ '
			end do
			write(1,*) '\hline'
			write(1,*) 'HS & & & & & \\'
			write(1,*) '\hline'
			do a = 1, nageg, 1
				write(1,fmt) ageg(a), ' & ', stats(2,a,1),' & ',stats(2,a,2),' & ',stats(2,a,3),' & ', &
					stats(2,a,4),' & ', stats(2,a,5),' \\ '
			end do		
			write(1,*) '\hline'
			write(1,*) 'College & & & & & \\'
			write(1,*) '\hline'
			do a = 1, nageg, 1
				write(1,fmt) ageg(a), ' & ', stats(3,a,1),' & ',stats(3,a,2),' & ',stats(3,a,3),' & ', &
					stats(3,a,4),' & ', stats(3,a,5),' \\ '
			end do		
			write(1,*) '\hline \hline'
			write(1,*) '\end{tabular}'
			close(1)
		end subroutine runlog

		subroutine runbaseline(irun)
			character*80 scn	
			integer irun
			! run baseline scenario
			scn = 'baseline'
			call initialize(scn)
			! save random draws for simulations
			call draws
			call runsim(pop_baseline, nobs_baseline, irun)
		end subroutine runbaseline

		subroutine tab2(irun)	
			double precision, allocatable :: temp(:)
			double precision table(7,4)
			integer i, n, e, irun
			
			! compute stats (median wealth)	
			allocate(temp(nobs_baseline))
			
			do e = 1, ne, 1
				n = 0
				temp(:) = -9.0d6
				! median wealth
				do i = 1, nobs_baseline, 1
					if (pop_baseline(i)%age .eq. 65.0d0 &
						.and. pop_baseline(i)%educ .eq. e) then
						n = n + 1				
						temp(n) = pop_baseline(i)%wealth					
					end if	
				end do				
				table(1,e) = median(temp,n)

				! average income
				n = 0
				temp(:) = -9.0d6
				do i = 1, nobs_baseline, 1
					if (pop_baseline(i)%educ .eq. e) then
						n = n + 1				
						temp(n) = pop_baseline(i)%income					
					end if	
				end do	
				table(2,e) = mean(temp,n)			
				table(3,e) = table(1,e)/table(2,e)

				! income less than assets
				n = 0
				temp(:) = -9.0d6
				do i = 1, nobs_baseline, 1
					if (pop_baseline(i)%age .eq. 65.0d0 &
						.and. pop_baseline(i)%educ .eq. e) then
						n = n + 1				
						if (2.0d0*pop_baseline(i)%income .gt. pop_baseline(i)%wealth) then
							temp(n) = 1.0d0
						else
							temp(n) = 0.0d0
						end if 					
					end if	
				end do				
				table(4,e) = mean(temp,n)

				! participation in sophisticated technology
				n = 0
				temp(:) = -9.0d6
				do i = 1, nobs_baseline, 1
					if (pop_baseline(i)%age .eq. 65.0d0 &
						.and. pop_baseline(i)%educ .eq. e) then
						n = n + 1				
						if (pop_baseline(i)%share .gt. 0.0d0) then
							temp(n) = 1.0d0
						else
							temp(n) = 0.0d0
						end if 					
					end if	
				end do				
				table(5,e) = mean(temp,n)

				! conditional share in sophisticated technology
				n = 0
				temp(:) = -9.0d6
				do i = 1, nobs_baseline, 1
					if (pop_baseline(i)%age .eq. 65.0d0 &
						.and. pop_baseline(i)%educ .eq. e) then			
						if (pop_baseline(i)%share .gt. 0.0d0) then
							n = n + 1
							temp(n) = pop_baseline(i)%share		
						end if 					
					end if	
				end do			
				if (n.gt.1) then	
					table(6,e) = mean(temp,n)
				else
					table(6,e) = 0.0d0
				end if	


				! low FL
				n = 0
				temp(:) = -9.0d6
				do i = 1, nobs_baseline, 1
					if (pop_baseline(i)%age .eq. 65.0d0 &
						.and. pop_baseline(i)%educ .eq. e) then
						n = n + 1				
						if (pop_baseline(i)%fin .lt. 25.0d0) then
							temp(n) = 1.0d0
						else
							temp(n) = 0.0d0
						end if 					
					end if	
				end do				
				table(7,e) = mean(temp,n)

			end do
			
			! compute ratio (college/less hs)
			do i = 1, 7, 1
				table(i,4) = table(i,3)/table(i,1)
			end do

			! report results
			write(*,*) ''			
			write(*,*) '*** table 2 results'
			open(1,file=trim(path)//'tables/table-2.txt')
			do i = 1, 7, 1
				write(*,*) i, table(i,:) 
				write(1,*) i, table(i,:) 
			end do
			close(1)
			
		end subroutine tab2

		subroutine tab3(irun)
			double precision, allocatable :: temp(:)
			double precision table(3,5,4)
			double precision vf,cf(ne),uf,evb,vb,cb(ne),ub,wta(ne)
			integer i, n, e, j, nobs
			type (person), allocatable, target :: pop_ssgen(:)
			type (person), allocatable, target :: pop_lowfloor(:)
			type (person), allocatable, target :: pop_initfin(:)
			integer nobs_ssgen, nobs_lowfloor, nobs_initfin
			type (person), pointer :: pop(:)
			character*80 scn	
			double precision discount, sx
			integer s, ss, irun
			double precision pv(ne), invexp, age(T)
			
			do s = 1, T, 1
				age(s) = dble(24 + s)
			end do
			! run scenarios
			scn = 'ssgen'
			call initialize(scn)
			call runsim(pop_ssgen, nobs_ssgen, irun)
			scn = 'lowfloor'
			call initialize(scn)
			call runsim(pop_lowfloor, nobs_lowfloor, irun)
			scn = 'initfin'
			call initialize(scn)
			call runsim(pop_initfin, nobs_initfin, irun)

												
			! compute stats , store data temporarily here	
			allocate(temp(nobs_baseline))
			
			do j = 1, 3, 1		
				if (j.eq.1) then
					pop => pop_ssgen
					nobs = nobs_ssgen
				end if
				if (j.eq.2) then
					pop => pop_lowfloor
					nobs = nobs_lowfloor
				end if 
				if (j.eq.3) then
					pop => pop_initfin
					nobs = nobs_initfin				
				end if		
									
				do e = 1, ne, 1
					n = 0
					temp(:) = -9.0d6
					! median wealth
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pop(i)%wealth					
						end if	
					end do				
					table(j,1,e) = median(temp,n)

					! average income
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pop(i)%income					
						end if	
					end do				
					table(j,2,e) = table(j,1,e)/mean(temp,n)

					! income less than assets
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							if (2.0d0*pop(i)%income .gt. pop(i)%wealth) then
								temp(n) = 1.0d0
							else
								temp(n) = 0.0d0
							end if 					
						end if	
					end do				
					table(j,3,e) = mean(temp,n)

					! participation in sophisticated technology
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							if (pop(i)%share .gt. 0.0d0) then
								temp(n) = pop(i)%share
							else
								temp(n) = 0.0d0
							end if 					
						end if	
					end do				
					table(j,4,e) = mean(temp,n)

					! low FL
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							if (pop(i)%fin .lt. 25.0d0) then
								temp(n) = 1.0d0
							else
								temp(n) = 0.0d0
							end if 					
						end if	
					end do				
					table(j,5,e) = mean(temp,n)
								
					! CV measure for FL
					if (j.eq.3) then
						n = 0
						temp(:) = -9.0d6
						! expected utility in FL scenario
						do i = 1, nobs, 1
							if (pop(i)%age .eq. 25.0d0 &
								.and. pop(i)%educ .eq. e) then
								n = n + 1				
								temp(n) = pop(i)%value
							end if	
						end do				
						vf = mean(temp,n)

						n = 0
						temp(:) = -9.0d6
						
						! find permanent consumption in FL scenario
						discount = 0.0d0
						do s = 1, T, 1
							! probability of survival
							sx = 1.0d0
							do ss = 1, s-1, 1
								sx = sx*(1.0d0-mort(ss,e)) 
							end do
							discount = discount + sx*(beta**(s-1))*(eqs(s,e)**(gamma))
						end do
						
						cf(e) = ((1.0d0-gamma)*vf/discount)**(1.0d0/(1.0d0-gamma))
												
						n = 0
						temp(:) = -9.0d6
						! expected utility in baseline scenario
						do i = 1, nobs_baseline, 1
							if (pop_baseline(i)%age .eq. 25.0d0 &
								.and. pop_baseline(i)%educ .eq. e) then
								n = n + 1				
								temp(n) = pop_baseline(i)%value							
							end if	
						end do				
						vb = mean(temp,n)


						n = 0
						temp(:) = -9.0d6
						! consumption in baseline scenario
						cb(e) = ((1.0d0-gamma)*vb/discount)**(1.0d0/(1.0d0-gamma))
                        
                        ! get utility one period ahead
                        evb = vb - utility(cb(e),eqs(1,e),1)
                        ! compute change in consumption today
                        wta(e) = utility(vf-evb,eqs(1,e),-1) - cb(e)
                        !write(*,*) 'baseline ', wta(e), vf,vb, evb, cb(e)

						! obtain wta
						!wta(e) = (cf(e) - cb(e))*discount
						
											
					end if
					
					
					
				end do

				! compute ratio (college/less hs)
				do i = 1, 5, 1
					table(j,i,4) = table(j,i,3)/table(j,i,1)
				end do

            
               ! investment costs
                do e = 1, ne, 1
                    pv(e) = 0.0d0
                    sx = 1.0d0
                    do s = 1, T, 1
                        n = 0
                        temp(:) = 0.0d0
                        do i = 1, nobs, 1
                            if (pop(i)%age .eq. age(s) &
                                .and. pop(i)%educ .eq. e) then
                                n = n + 1				
                                temp(n) = pi0*(pop(i)%inv)**pi1					
                            end if	
                        end do			
                        if (n.gt.0) then
                            invexp = mean(temp,n)	
                        else
                           invexp = 0.0d0
                        end if    
                        pv(e) = pv(e) + (beta**s) * sx * invexp	
                    
                        sx = sx*(1.0d0-mort(s,e))				
                    end do 
                end do
			
			    write(*,*) 'PV ',j,pv
	
               ! value of retirement income
                do e = 1, ne, 1
                    pv(e) = 0.0d0
                    sx = 1.0d0
                    do s = retage, T, 1
                        n = 0
                        temp(:) = 0.0d0
                        do i = 1, nobs, 1
                            if (pop(i)%age .eq. age(s) &
                                .and. pop(i)%educ .eq. e) then
                                n = n + 1				
                                temp(n) = pop(i)%income					
                            end if	
                        end do			
                        if (n.gt.0) then
                            invexp = mean(temp,n)	
                        else
                           invexp = 0.0d0
                        end if    
                        pv(e) = pv(e) + (beta**(s-retage)) * sx * invexp	
                    
                        sx = sx*(1.0d0-mort(s,e))				
                    end do 
                end do	
                write(*,*) 'INCOME ', pv
                					
			end do
        
               ! investment costs
                do e = 1, ne, 1
                    pv(e) = 0.0d0
                    sx = 1.0d0
                    do s = 1, T, 1
                        n = 0
                        temp(:) = 0.0d0
                        do i = 1, nobs, 1
                            if (pop_baseline(i)%age .eq. age(s) &
                                .and. pop_baseline(i)%educ .eq. e) then
                                n = n + 1				
                                temp(n) = pi0*(pop_baseline(i)%inv)**pi1					
                            end if	
                        end do			
                        if (n.gt.0) then
                            invexp = mean(temp,n)	
                        else
                           invexp = 0.0d0
                        end if    
                        pv(e) = pv(e) + (beta**s) * sx * invexp	
                    
                        sx = sx*(1.0d0-mort(s,e))				
                    end do 
                end do
			
			    write(*,*) 'PV ', pv

               ! value of retirement income
                do e = 1, ne, 1
                    pv(e) = 0.0d0
                    sx = 1.0d0
                    do s = retage, T, 1
                        n = 0
                        temp(:) = 0.0d0
                        do i = 1, nobs_baseline, 1
                            if (pop_baseline(i)%age .eq. age(s) &
                                .and. pop_baseline(i)%educ .eq. e) then
                                n = n + 1				
                                temp(n) = pop_baseline(i)%income					
                            end if	
                        end do			
                        if (n.gt.0) then
                            invexp = mean(temp,n)	
                        else
                           invexp = 0.0d0
                        end if    
                        pv(e) = pv(e) + (beta**(s-retage)) * sx * invexp	
                    
                        sx = sx*(1.0d0-mort(s,e))				
                    end do 
                end do	
                write(*,*) 'INCOME ', pv
                			    						
			! report results
			write(*,*) ''			
			write(*,*) '*** table 3 results'
			open(1,file=trim(path)//'tables/table-3.txt')
			write(*,*) ''			
			write(*,*) '- lowering retirement benefits'
			do i = 1, 5, 1
				write(*,*) i, table(1,i,:) 
				write(1,*) i, table(1,i,:) 
			end do
			write(*,*) ''			
			write(*,*) '- lowering means testing benefits'
			do i = 1, 5, 1
				write(*,*) i, table(2,i,:) 
				write(1,*) i, table(2,i,:) 
			end do
			write(*,*) ''			
			write(*,*) '- perfect knowledge'
			do i = 1, 5, 1
				write(*,*) i, table(3,i,:) 
				write(1,*) i, table(3,i,:) 
			end do
			write(1,*) 6, wta, 0.0d0
			write(1,*) 7, cb, 0.0d0
			write(1,*) 8, cf, 0.0d0
			close(1)
		end subroutine tab3

		subroutine tab3learn(irun)
			double precision, allocatable :: temp(:)
			double precision table(3,5,4)
			double precision vf,cf(ne),uf,evb,vb,cb(ne),ub,wta(ne)
			integer i, n, e, j, nobs
			type (person), allocatable, target :: pop_ssgen(:)
			type (person), allocatable, target :: pop_lowfloor(:)
			type (person), allocatable, target :: pop_initfin(:)
			integer nobs_ssgen, nobs_lowfloor, nobs_initfin
			type (person), pointer :: pop(:)
			character*80 scn	
			double precision discount, sx
			integer s, ss, irun
			double precision pv(ne), invexp, age(T)
			
			do s = 1, T, 1
				age(s) = dble(24 + s)
			end do
			! run scenarios
			scn = 'ssgen-learn'
			call initialize(scn)
			call runsim(pop_ssgen, nobs_ssgen, 0)
			scn = 'lowfloor-learn'
			call initialize(scn)
			call runsim(pop_lowfloor, nobs_lowfloor, 0)
			scn = 'learning2'
			call initialize(scn)
			call runsim(pop_initfin, nobs_initfin, irun)

												
			! compute stats , store data temporarily here	
			allocate(temp(nobs_baseline))
			
			do j = 1, 3, 1		
				if (j.eq.1) then
					pop => pop_ssgen
					nobs = nobs_ssgen
				end if
				if (j.eq.2) then
					pop => pop_lowfloor
					nobs = nobs_lowfloor
				end if 
				if (j.eq.3) then
					pop => pop_initfin
					nobs = nobs_initfin				
				end if		
									
				do e = 1, ne, 1
					n = 0
					temp(:) = -9.0d6
					! median wealth
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pop(i)%wealth					
						end if	
					end do				
					table(j,1,e) = median(temp,n)

					! average income
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pop(i)%income					
						end if	
					end do				
					table(j,2,e) = table(j,1,e)/mean(temp,n)

					! income less than assets
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							if (2.0d0*pop(i)%income .gt. pop(i)%wealth) then
								temp(n) = 1.0d0
							else
								temp(n) = 0.0d0
							end if 					
						end if	
					end do				
					table(j,3,e) = mean(temp,n)

					! participation in sophisticated technology
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							if (pop(i)%share .gt. 0.0d0) then
								temp(n) = pop(i)%share
							else
								temp(n) = 0.0d0
							end if 					
						end if	
					end do				
					table(j,4,e) = mean(temp,n)

					! low FL
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							if (pop(i)%fin .lt. 25.0d0) then
								temp(n) = 1.0d0
							else
								temp(n) = 0.0d0
							end if 					
						end if	
					end do				
					table(j,5,e) = mean(temp,n)
								
					! CV measure for FL
					if (j.eq.3) then
						n = 0
						temp(:) = -9.0d6
						! expected utility in FL scenario
						do i = 1, nobs, 1
							if (pop(i)%age .eq. 25.0d0 &
								.and. pop(i)%educ .eq. e) then
								n = n + 1				
								temp(n) = pop(i)%value
							end if	
						end do				
						vf = mean(temp,n)

						n = 0
						temp(:) = -9.0d6
						
						! find permanent consumption in FL scenario
						discount = 0.0d0
						do s = 1, T, 1
							! probability of survival
							sx = 1.0d0
							do ss = 1, s-1, 1
								sx = sx*(1.0d0-mort(ss,e)) 
							end do
							discount = discount + sx*(beta**(s-1))*(eqs(s,e)**(gamma))
						end do
						
						cf(e) = ((1.0d0-gamma)*vf/discount)**(1.0d0/(1.0d0-gamma))
												
						n = 0
						temp(:) = -9.0d6
						! expected utility in baseline scenario
						do i = 1, nobs_baseline, 1
							if (pop_baseline(i)%age .eq. 25.0d0 &
								.and. pop_baseline(i)%educ .eq. e) then
								n = n + 1				
								temp(n) = pop_baseline(i)%value							
							end if	
						end do				
						vb = mean(temp,n)


						n = 0
						temp(:) = -9.0d6
						! consumption in baseline scenario
						cb(e) = ((1.0d0-gamma)*vb/discount)**(1.0d0/(1.0d0-gamma))
                        
                        ! get utility one period ahead
                        evb = vb - utility(cb(e),eqs(1,e),1)
                        ! compute change in consumption today
                        wta(e) = utility(vf-evb,eqs(1,e),-1) - cb(e)
                        !write(*,*) 'baseline ', wta(e), vf,vb, evb, cb(e)

						! obtain wta
						!wta(e) = (cf(e) - cb(e))*discount
						
											
					end if
					
					
					
				end do

				! compute ratio (college/less hs)
				do i = 1, 5, 1
					table(j,i,4) = table(j,i,3)/table(j,i,1)
				end do

            
               ! investment costs
                do e = 1, ne, 1
                    pv(e) = 0.0d0
                    sx = 1.0d0
                    do s = 1, T, 1
                        n = 0
                        temp(:) = 0.0d0
                        do i = 1, nobs, 1
                            if (pop(i)%age .eq. age(s) &
                                .and. pop(i)%educ .eq. e) then
                                n = n + 1				
                                temp(n) = pi0*(pop(i)%inv)**pi1					
                            end if	
                        end do			
                        if (n.gt.0) then
                            invexp = mean(temp,n)	
                        else
                           invexp = 0.0d0
                        end if    
                        pv(e) = pv(e) + (beta**s) * sx * invexp	
                    
                        sx = sx*(1.0d0-mort(s,e))				
                    end do 
                end do
			
			    write(*,*) 'PV ',j,pv
	
               ! value of retirement income
                do e = 1, ne, 1
                    pv(e) = 0.0d0
                    sx = 1.0d0
                    do s = retage, T, 1
                        n = 0
                        temp(:) = 0.0d0
                        do i = 1, nobs, 1
                            if (pop(i)%age .eq. age(s) &
                                .and. pop(i)%educ .eq. e) then
                                n = n + 1				
                                temp(n) = pop(i)%income					
                            end if	
                        end do			
                        if (n.gt.0) then
                            invexp = mean(temp,n)	
                        else
                           invexp = 0.0d0
                        end if    
                        pv(e) = pv(e) + (beta**(s-retage)) * sx * invexp	
                    
                        sx = sx*(1.0d0-mort(s,e))				
                    end do 
                end do	
                write(*,*) 'INCOME ', pv
                					
			end do
        
               ! investment costs
                do e = 1, ne, 1
                    pv(e) = 0.0d0
                    sx = 1.0d0
                    do s = 1, T, 1
                        n = 0
                        temp(:) = 0.0d0
                        do i = 1, nobs, 1
                            if (pop_baseline(i)%age .eq. age(s) &
                                .and. pop_baseline(i)%educ .eq. e) then
                                n = n + 1				
                                temp(n) = pi0*(pop_baseline(i)%inv)**pi1					
                            end if	
                        end do			
                        if (n.gt.0) then
                            invexp = mean(temp,n)	
                        else
                           invexp = 0.0d0
                        end if    
                        pv(e) = pv(e) + (beta**s) * sx * invexp	
                    
                        sx = sx*(1.0d0-mort(s,e))				
                    end do 
                end do
			
			    write(*,*) 'PV ', pv

               ! value of retirement income
                do e = 1, ne, 1
                    pv(e) = 0.0d0
                    sx = 1.0d0
                    do s = retage, T, 1
                        n = 0
                        temp(:) = 0.0d0
                        do i = 1, nobs_baseline, 1
                            if (pop_baseline(i)%age .eq. age(s) &
                                .and. pop_baseline(i)%educ .eq. e) then
                                n = n + 1				
                                temp(n) = pop_baseline(i)%income					
                            end if	
                        end do			
                        if (n.gt.0) then
                            invexp = mean(temp,n)	
                        else
                           invexp = 0.0d0
                        end if    
                        pv(e) = pv(e) + (beta**(s-retage)) * sx * invexp	
                    
                        sx = sx*(1.0d0-mort(s,e))				
                    end do 
                end do	
                write(*,*) 'INCOME ', pv
                			    						
			! report results
			write(*,*) ''			
			write(*,*) '*** table 3 results (with learning)'
			open(1,file=trim(path)//'tables/table-3-learn.txt')
			write(*,*) ''			
			write(*,*) '- lowering retirement benefits'
			do i = 1, 5, 1
				write(*,*) i, table(1,i,:) 
				write(1,*) i, table(1,i,:) 
			end do
			write(*,*) ''			
			write(*,*) '- lowering means testing benefits'
			do i = 1, 5, 1
				write(*,*) i, table(2,i,:) 
				write(1,*) i, table(2,i,:) 
			end do
			write(*,*) ''			
			write(*,*) '- baseline learning'
			do i = 1, 5, 1
				write(*,*) i, table(3,i,:) 
				write(1,*) i, table(3,i,:) 
			end do
			write(1,*) 6, wta, 0.0d0
			write(1,*) 7, cb, 0.0d0
			write(1,*) 8, cf, 0.0d0
			close(1)
		end subroutine tab3learn

		subroutine tab4(irun)
			double precision, allocatable :: temp(:)
			double precision results(15,5,4), table(15,4)
			integer i, n, e, j, nobs, irun, s, run
			type (person), allocatable :: pop(:)
			character*80 scn	
            double precision pv(ne), inv, sx, age(T)
			allocate(temp(nobs_baseline))

			do s = 1, T, 1
				age(s) = dble(24 + s)
			end do
			
			do j = 1, 15, 1

				if (j.eq.1) then
					scn = 'sens1'
				end if
				if (j.eq.2) then
					scn = 'sens2'
				end if
				if (j.eq.3) then
					scn = 'sens3'
				end if
				if (j.eq.4) then
					scn = 'sens4'
				end if
				if (j.eq.5) then
					scn = 'sens5'
				end if
				if (j.eq.6) then
					scn = 'sens6'
				end if
				if (j.eq.7) then
					scn = 'sens7'
				end if
				if (j.eq.8) then
					scn = 'sens8'
				end if
				if (j.eq.9) then
					scn = 'sens9'
				end if
				if (j.eq.10) then
					scn = 'sens10'
				end if
				if (j.eq.11) then
					scn = 'sens11'
				end if
				if (j.eq.12) then
					scn = 'sens12'
				end if
				if (j.eq.13) then
					scn = 'sens13'
				end if
				if (j.eq.14) then
					scn = 'sens14'
				end if
				if (j.eq.15) then
					scn = 'sens15'
				end if
	

				call initialize(scn)
				call runsim(pop, nobs, run)

				do e = 1, ne, 1
					n = 0
					temp(:) = -9.0d6
					! median wealth
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pop(i)%wealth					
						end if	
					end do				
					results(j,1,e) = median(temp,n)

					! average income
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pop(i)%income					
						end if	
					end do				
					results(j,2,e) = results(j,1,e)/mean(temp,n)

					! income less than assets
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							if (2.0d0*pop(i)%income .gt. pop(i)%wealth) then
								temp(n) = 1.0d0
							else
								temp(n) = 0.0d0
							end if 					
						end if	
					end do				
					results(j,3,e) = mean(temp,n)

					! participation in sophisticated technology
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							if (pop(i)%share .gt. 0.0d0) then
								temp(n) = pop(i)%share
							else
								temp(n) = 0.0d0
							end if 					
						end if	
					end do				
					results(j,4,e) = mean(temp,n)

					! low FL
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							if (pop(i)%fin .lt. 25.0d0) then
								temp(n) = 1.0d0
							else
								temp(n) = 0.0d0
							end if 					
						end if	
					end do				
					results(j,5,e) = mean(temp,n)

				end do
				! compute ratio (college/less hs)
				do i = 1, 5, 1
					results(j,i,4) = results(j,i,3)/results(j,i,1)
				end do				
				
				! assign stats to table
				table(j,1) = results(j,2,1)
				table(j,2) = results(j,2,4)
				table(j,3) = results(j,4,4)
				table(j,4) = results(j,5,4)
				
                ! for j = 3 et 4, compute NPV of expenditures                
                if (j.eq.3 .or. j.eq.4) then				
                    ! investment costs
                    do e = 1, ne, 1
                        pv(e) = 0.0d0
                        sx = 1.0d0
                        do s = 1, T, 1
                            n = 0
                            temp(:) = 0.0d0
                            do i = 1, nobs, 1
                                if (pop(i)%age .eq. age(s) &
                                    .and. pop(i)%educ .eq. e) then
                                    n = n + 1				
                                    temp(n) = pi0*(pop(i)%inv)**pi1					
                                end if	
                            end do			
                            if (n.gt.0) then
                                inv = mean(temp,n)	
                            else
                               inv = 0.0d0
                            end if    
                            pv(e) = pv(e) + (beta**s) * sx * inv	
                            sx = sx*(1.0d0-mort(s,e))				
                        end do 
                    end do
                
                    write(*,*) 'NPV:::'
                    write(*,*) delta0, pv
                    
                end if
							
				deallocate(pop)
				
			end do
											 
			! report results
			write(*,*) ''			
			write(*,*) '*** table 4 results'
			open(1,file=trim(path)//'tables/table-4.txt')
			do i = 1, 15, 1
				write(*,*) i, table(i,:) 
				write(1,*) i, table(i,:) 
			end do
			close(1)		
		
		end subroutine tab4


		! age profile of financial knowledge and its cost		
		subroutine fig4(irun)
			double precision temp(nobs_baseline)
			double precision table(2,T,ne), age(T),pv(3,ne), sx
			integer e, s, n, i, irun
			
			do s = 1, T, 1
				age(s) = dble(24 + s)
			end do
			
			! financial knowledge
			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs_baseline, 1
						if (pop_baseline(i)%age .eq. age(s) &
							.and. pop_baseline(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pop_baseline(i)%fin					
						end if	
					end do			
					table(1,s,e) = mean(temp,n)						
				end do 
			end do
						
            ! investment costs
			do e = 1, ne, 1
			    pv(:,e) = 0.0d0
			    sx = 1.0d0
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs_baseline, 1
						if (pop_baseline(i)%age .eq. age(s) &
							.and. pop_baseline(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pi0*(pop_baseline(i)%inv)**pi1					
						end if	
					end do			
					if (n.gt.0) then
					    table(2,s,e) = mean(temp,n)	
					else
					   table(2,s,e) = 0.0d0
					end if    
					pv(1,e) = pv(1,e) + (beta**s) * sx * table(2,s,e)	
					pv(2,e) = pv(2,e) + (0.98d0**s) * sx * table(2,s,e)	
					pv(3,e) = pv(3,e) +  sx * table(2,s,e)	
					
					sx = sx*(1.0d0-mort(s,e))				
				end do 
			end do
			
			! report results for Figure 4
			write(*,*) ''
			write(*,*) '*** figure 4 results'
			open(1,file=trim(path)//'figures/fig-4.txt')
			do s = 1, T, 1
				if (age(s).le.90.0d0) then
					write(*,*) age(s), table(1,s,:),table(2,s,:)
					write(1,*) age(s), table(1,s,:),table(2,s,:)
				end if
			end do
			! last line holds pv for different discount rates
			write(1,*) 110, 0.0d0,0.0d0,0.0d0,pv(1,:)			
			write(*,*) 'NPV:::'
			write(*,*) beta,pv(1,:)
			write(1,*) 110, 0.0d0,0.0d0,0.0d0,pv(2,:)			
			write(*,*) 0.98d0,pv(2,:)
			write(1,*) 110, 0.0d0,0.0d0,0.0d0,pv(3,:)			
			write(*,*) 1.0d0,pv(3,:)
			close(1)

		end subroutine fig4

		subroutine fig5(irun)
			double precision, allocatable :: temp(:)
			double precision table(6,4)
			integer i, n, e, j, nobs, irun
			type (person), allocatable :: pop(:)
			character*80 scn	

			allocate(temp(nobs_baseline))

			do j = 1, 6, 1
				if (j.eq.1) then
					scn = 'stripdown'
				end if
				if (j.eq.2) then
					scn = 'hsz'
				end if
				if (j.eq.3) then
					scn = 'reprate'
				end if
				if (j.eq.4) then
					scn = 'demo'
				end if
				if (j.eq.5) then
					scn = 'mortality'
				end if
				if (j.eq.6) then
					scn = 'baseline'
				end if
					
				if (j .ne. 6) then
					call initialize(scn)
					call runsim(pop, nobs, irun)
				else
					allocate(pop(nobs_baseline))
					pop = pop_baseline
					nobs = nobs_baseline
				end if
				
				do e = 1, ne, 1
					n = 0
					temp(:) = -9.0d6
					! median wealth
					do i = 1, nobs, 1
						if (pop(i)%age .eq. 65.0d0 &
							.and. pop(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pop(i)%wealth					
						end if	
					end do				
					table(j,e) = median(temp,n)

					! average income
					n = 0
					temp(:) = -9.0d6
					do i = 1, nobs, 1
						if (pop(i)%educ .eq. e) then
							n = n + 1				
							temp(n) = pop(i)%income					
						end if	
					end do				
					table(j,e) = table(j,e)/mean(temp,n)
				end do
				
				! compute ratio (college/less hs)
				table(j,4) = table(j,3)/table(j,1)

				deallocate(pop)
				
			end do
											 
			! report results
			write(*,*) ''			
			write(*,*) '*** figure 5 results'
			open(1,file=trim(path)//'figures/fig-5.txt')
			do i = 1, 6, 1
				write(*,*) i, table(i,:)
				write(1,*) i, table(i,:) 
			end do
			close(1)		

		end subroutine fig5

		subroutine fig6a(irun)
			double precision temp(nobs_baseline)
			double precision table(4,T,ne), age(T)
			integer e, s, n, i, nobs, irun
			character*80 scn
			type (person), allocatable :: pop(:)
			
			do s = 1, T, 1
				age(s) = dble(24 + s)
			end do
	
			scn = 'hetpref2'
			call initialize(scn)
			call runsim(pop, nobs, irun)

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs_baseline, 1
						if (pop_baseline(i)%age .eq. age(s) &
							.and. pop_baseline(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop_baseline(i)%share					
						end if	
					end do		
					if (n.eq.0) then
					    table(1,s,e) = 0.0d0				
					else
					    table(1,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs_baseline, 1
						if (pop_baseline(i)%age .eq. age(s) &
							.and. pop_baseline(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop_baseline(i)%wealth					
						end if	
					end do		
					if (n.eq.0) then
					    table(2,s,e) = 0.0d0				
					else
					    table(2,s,e) = median(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop(i)%share					
						end if	
					end do		
					if (n.eq.0) then
					    table(3,s,e) = 0.0d0				
					else
					    table(3,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop(i)%wealth					
						end if	
					end do		
					if (n.eq.0) then
					    table(4,s,e) = 0.0d0				
					else
					    table(4,s,e) = median(temp,n)					
					end if						
				end do 
			end do
															
			! report results for Figure 6
			write(*,*) ''
			write(*,*) '*** figure 6a results'
			open(1,file=trim(path)//'figures/fig-6a.txt')
			do s = 1, T, 1
				if (age(s).le.90.0d0) then
					write(*,*) age(s), table(1,s,:), table(2,s,:), table(3,s,:), table(4,s,:)
					write(1,*) age(s), table(1,s,:), table(2,s,:), table(3,s,:), table(4,s,:)
				end if
			end do
			close(1)

		end subroutine fig6a


		subroutine fig6b(irun)
			double precision temp(nobs_baseline)
			double precision table(4,T,ne), age(T)
			integer e, s, n, i, nobs, irun
			character*80 scn
			type (person), allocatable :: pop(:)
            double precision tablex(3,4)
            integer j
            			
			do s = 1, T, 1
				age(s) = dble(24 + s)
			end do
			
			scn = 'zin'
			call initialize(scn)
			call runsim(pop, nobs, irun)
			
			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							 n = n + 1
							 if (pop(i)%share .gt. 0.0d0) then
							    temp(n) = 1.0d0
							 else
							    temp(n) = 0.0d0
							 end if          
						end if	
					end do		
					if (n.eq.0) then
					    table(1,s,e) = 0.0d0				
					else
					    table(1,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop(i)%wealth					
						end if	
					end do		
					if (n.eq.0) then
					    table(2,s,e) = 0.0d0				
					else
					    table(2,s,e) = median(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop(i)%share					
						end if	
					end do		
					if (n.eq.0) then
					    table(3,s,e) = 0.0d0				
					else
					    table(3,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop(i)%rate					
						end if	
					end do		
					if (n.eq.0) then
					    table(4,s,e) = 0.0d0				
					else
					    table(4,s,e) = mean(temp,n)					
					end if						
				end do 
			end do
															
			! report results for Figure 6b
			write(*,*) ''
			write(*,*) '*** figure 6b results'
			open(1,file=trim(path)//'figures/fig-6b.txt')
			do s = 1, T, 1
				if (age(s).le.90.0d0) then
					write(*,*) age(s), table(1,s,:), table(2,s,:), table(3,s,:), table(4,s,:)
					write(1,*) age(s), table(1,s,:), table(2,s,:), table(3,s,:), table(4,s,:)
				end if
			end do
			close(1)


            ! compute wealth ratio at retirement
            do e = 1, ne, 1
				n = 0
				temp(:) = -9.0d6
				! median wealth
				do i = 1, nobs, 1
					if (pop(i)%age .eq. 65.0d0 &
						.and. pop(i)%educ .eq. e) then
						n = n + 1				
						temp(n) = pop(i)%wealth					
					end if	
				end do				
				tablex(1,e) = median(temp,n)

				! average income
				n = 0
				temp(:) = -9.0d6
				do i = 1, nobs, 1
					if (pop(i)%educ .eq. e) then
						n = n + 1				
						temp(n) = pop(i)%income					
					end if	
				end do	
				tablex(1,e) = tablex(1,e)/mean(temp,n)			
            end do
            tablex(1,4) = tablex(1,3)/tablex(1,1)
            
            ! run zin on mortality 
            deallocate(pop)
			scn = 'zinmortality'
			call initialize(scn)
			call runsim(pop, nobs, irun)
            ! compute wealth ratio at retirement
            do e = 1, ne, 1
				n = 0
				temp(:) = -9.0d6
				! median wealth
				do i = 1, nobs, 1
					if (pop(i)%age .eq. 65.0d0 &
						.and. pop(i)%educ .eq. e) then
						n = n + 1				
						temp(n) = pop(i)%wealth					
					end if	
				end do				
				tablex(2,e) = median(temp,n)

				! average income
				n = 0
				temp(:) = -9.0d6
				do i = 1, nobs, 1
					if (pop(i)%educ .eq. e) then
						n = n + 1				
						temp(n) = pop(i)%income					
					end if	
				end do	
				tablex(2,e) = tablex(2,e)/mean(temp,n)			
            end do
            tablex(2,4) = tablex(2,3)/tablex(2,1)
            
             ! run zin on stripdown 
            deallocate(pop)
			scn = 'zinstrip'
			call initialize(scn)
			call runsim(pop, nobs, irun)
            ! compute wealth ratio at retirement
            do e = 1, ne, 1
				n = 0
				temp(:) = -9.0d6
				! median wealth
				do i = 1, nobs, 1
					if (pop(i)%age .eq. 65.0d0 &
						.and. pop(i)%educ .eq. e) then
						n = n + 1				
						temp(n) = pop(i)%wealth					
					end if	
				end do				
				tablex(3,e) = median(temp,n)

				! average income
				n = 0
				temp(:) = -9.0d6
				do i = 1, nobs, 1
					if (pop(i)%educ .eq. e) then
						n = n + 1				
						temp(n) = pop(i)%income					
					end if	
				end do	
				tablex(3,e) = tablex(3,e)/mean(temp,n)			
            end do
            tablex(3,4) = tablex(3,3)/tablex(3,1)           
            
            write(*,*) 'decomposition with Epstein-Zin'
            
            do j = 1,3, 1
                write(*,*) tablex(j,:)
            end do
            
            write(*,*) 'share explained by FK = ', (tablex(1,4)-tablex(2,4))/(tablex(1,4)-tablex(3,4))
            
            
		end subroutine fig6b
		
		subroutine fig9(irun)
			double precision temp(nobs_baseline)
			double precision table(4*2,T,ne), age(T)
			integer e, s, n, i, nobs, irun
			character*80 scn
			type (person), allocatable :: pop(:)
			
			do s = 1, T, 1
				age(s) = dble(24 + s)
			end do
			
			scn = 'diverse'
			call initialize(scn)
			call runsim(pop, nobs, irun)
			
			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							 n = n + 1
							 if (pop(i)%share .gt. 0.0d0) then
							    temp(n) = 1.0d0
							 else
							    temp(n) = 0.0d0
							 end if          
						end if	
					end do		
					if (n.eq.0) then
					    table(1,s,e) = 0.0d0				
					else
					    table(1,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop(i)%wealth					
						end if	
					end do		
					if (n.eq.0) then
					    table(2,s,e) = 0.0d0				
					else
					    table(2,s,e) = median(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e &
							    .and. pop(i)%share .gt. 0.0d0 ) then
							n = n + 1				
							temp(n) = pop(i)%share					
						end if	
					end do		
					if (n.eq.0) then
					    table(3,s,e) = 0.0d0				
					else
					    table(3,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop(i)%rate					
						end if	
					end do		
					if (n.eq.0) then
					    table(4,s,e) = 0.0d0				
					else
					    table(4,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs_baseline, 1
						if (pop_baseline(i)%age .eq. age(s) &
							.and. pop_baseline(i)%educ .eq. e ) then
							 n = n + 1
							 if (pop_baseline(i)%share .gt. 0.0d0) then
							    temp(n) = 1.0d0
							 else
							    temp(n) = 0.0d0
							 end if          
						end if	
					end do		
					if (n.eq.0) then
					    table(5,s,e) = 0.0d0				
					else
					    table(5,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs_baseline, 1
						if (pop_baseline(i)%age .eq. age(s) &
							.and. pop_baseline(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop_baseline(i)%wealth					
						end if	
					end do		
					if (n.eq.0) then
					    table(6,s,e) = 0.0d0				
					else
					    table(6,s,e) = median(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs_baseline, 1
						if (pop_baseline(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e &
							    .and. pop_baseline(i)%share .gt. 0.0d0 ) then
							n = n + 1				
							temp(n) = pop_baseline(i)%share					
						end if	
					end do		
					if (n.eq.0) then
					    table(7,s,e) = 0.0d0				
					else
					    table(7,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs_baseline, 1
						if (pop_baseline(i)%age .eq. age(s) &
							.and. pop_baseline(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop_baseline(i)%rate					
						end if	
					end do		
					if (n.eq.0) then
					    table(8,s,e) = 0.0d0				
					else
					    table(8,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			! report results for Figure 9
			write(*,*) ''
			write(*,*) '*** figure 9 results'
			open(1,file=trim(path)//'figures/fig-9.txt')
			do s = 1, T, 1
				if (age(s).le.90.0d0) then
					write(*,*) age(s), table(1,s,:), table(2,s,:), table(3,s,:), table(4,s,:)
					write(1,*) age(s), table(1,s,:), table(2,s,:), table(3,s,:), table(4,s,:), &
						table(5,s,:), table(6,s,:), table(7,s,:), table(8,s,:)
				end if
			end do
			close(1)

		end subroutine fig9

		subroutine fig8(irun)
			double precision temp(nobs_baseline)
			double precision table(6,T,ne), age(T)
			integer e, s, n, i, nobs, irun
			character*80 scn
			type (person), allocatable :: pop(:)
			
			do s = 1, T, 1
				age(s) = dble(24 + s)
			end do
			
			scn = 'learning1'
			call initialize(scn)
			call runsim(pop, nobs, irun)
			
			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							 n = n + 1
							 if (pop(i)%share .gt. 0.0d0) then
							    temp(n) = 1.0d0
							 else
							    temp(n) = 0.0d0
							 end if          
						end if	
					end do		
					if (n.eq.0) then
					    table(1,s,e) = 0.0d0				
					else
					    table(1,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop(i)%wealth					
						end if	
					end do		
					if (n.eq.0) then
					    table(2,s,e) = 0.0d0				
					else
					    table(2,s,e) = median(temp,n)					
					end if						
				end do 
			end do

            deallocate(pop)
			scn = 'learning2'
			call initialize(scn)
			call runsim(pop, nobs, irun)
			
			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							 n = n + 1
							 if (pop(i)%share .gt. 0.0d0) then
							    temp(n) = 1.0d0
							 else
							    temp(n) = 0.0d0
							 end if          
						end if	
					end do		
					if (n.eq.0) then
					    table(3,s,e) = 0.0d0				
					else
					    table(3,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop(i)%wealth					
						end if	
					end do		
					if (n.eq.0) then
					    table(4,s,e) = 0.0d0				
					else
					    table(4,s,e) = median(temp,n)					
					end if						
				end do 
			end do	
			! baseline
			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs_baseline, 1
						if (pop_baseline(i)%age .eq. age(s) &
							.and. pop_baseline(i)%educ .eq. e ) then
							 n = n + 1
							 if (pop_baseline(i)%share .gt. 0.0d0) then
							    temp(n) = 1.0d0
							 else
							    temp(n) = 0.0d0
							 end if          
						end if	
					end do		
					if (n.eq.0) then
					    table(5,s,e) = 0.0d0				
					else
					    table(5,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs_baseline, 1
						if (pop_baseline(i)%age .eq. age(s) &
							.and. pop_baseline(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop_baseline(i)%wealth					
						end if	
					end do		
					if (n.eq.0) then
					    table(6,s,e) = 0.0d0				
					else
					    table(6,s,e) = median(temp,n)					
					end if						
				end do 
			end do

			! report results for Figure 8
			write(*,*) ''
			write(*,*) '*** figure 8 results'
			open(1,file=trim(path)//'figures/fig-8.txt')
			do s = 1, T, 1
				if (age(s).le.90.0d0) then
					write(*,*) age(s), table(1,s,:), table(2,s,:), table(3,s,:), table(4,s,:),table(5,s,:), table(6,s,:)
					write(1,*) age(s), table(1,s,:), table(2,s,:), table(3,s,:), table(4,s,:),table(5,s,:), table(6,s,:)
				end if
			end do
			close(1)

		end subroutine fig8

		subroutine adhoc(irun)
			double precision temp(nobs_baseline)
			double precision table(4,T,ne), age(T)
			integer e, s, n, i, nobs, irun
			character*80 scn
			type (person), allocatable :: pop(:)
			
			do s = 1, T, 1
				age(s) = dble(24 + s)
			end do
			
			scn = 'genes'
			call initialize(scn)
			call runsim(pop, nobs, irun)
			
			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							 n = n + 1
							 if (pop(i)%share .gt. 0.0d0) then
							    temp(n) = 1.0d0
							 else
							    temp(n) = 0.0d0
							 end if          
						end if	
					end do		
					if (n.eq.0) then
					    table(1,s,e) = 0.0d0				
					else
					    table(1,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop(i)%wealth					
						end if	
					end do		
					if (n.eq.0) then
					    table(2,s,e) = 0.0d0				
					else
					    table(2,s,e) = median(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e &
							    .and. pop(i)%share .gt. 0.0d0 ) then
							n = n + 1				
							temp(n) = pop(i)%share					
						end if	
					end do		
					if (n.eq.0) then
					    table(3,s,e) = 0.0d0				
					else
					    table(3,s,e) = mean(temp,n)					
					end if						
				end do 
			end do

			do e = 1, ne, 1
				do s = 1, T, 1
					n = 0
					temp(:) = 0.0d0
					do i = 1, nobs, 1
						if (pop(i)%age .eq. age(s) &
							.and. pop(i)%educ .eq. e ) then
							n = n + 1				
							temp(n) = pop(i)%rate					
						end if	
					end do		
					if (n.eq.0) then
					    table(4,s,e) = 0.0d0				
					else
					    table(4,s,e) = mean(temp,n)					
					end if						
				end do 
			end do
															
			! report results for Figure 10
			write(*,*) ''
			write(*,*) '*** figure genes results'
			open(1,file=trim(path)//'figures/fig-genes.txt')
			do s = 1, T, 1
				if (age(s).le.90.0d0) then
					write(*,*) age(s), table(1,s,:), table(2,s,:), table(3,s,:), table(4,s,:)
					write(1,*) age(s), table(1,s,:), table(2,s,:), table(3,s,:), table(4,s,:)
				end if
			end do
			close(1)

		end subroutine adhoc
								
        !* utility function
        double precision function utility(x,eq,option)
            double precision x, eq
            integer option            
            if (option.eq.1) then
                    utility = eq*((x/eq)**(1.0d0-gamma))/(1.0d0-gamma) 
            else
                    utility = eq*(x*(1.0d0-gamma)/eq)**(1.0/(1.0d0-gamma))
            end if
        end function

			
	   double precision  function median(x, n)
		  implicit none
		  integer n, i
		  double precision x(:), temp(n)
		  do i = 1, n                       ! make a copy
			 temp(i) = x(i)
		  end do
		  call  sort(temp, n)               ! sort the copy
		  if (mod(n,2) == 0) then           ! compute the median
			 median = (temp(n/2) + temp(n/2+1)) / 2.0d0
		  else
			 median = temp(n/2+1)
		  end if
	   end function  median

	   double precision function mean(x,n)
			implicit none
			integer n, i
			double precision x(:), xmean
			xmean = 0.0d0
			do i = 1, n, 1
				xmean = xmean + x(i)
			end do
			xmean = xmean/dble(n)
			mean = xmean
		end function mean

							
end module simulate


! program to produce results in paper (flag = 1 if run scenario, flag = 0 if not)
program main
	use simulate
	! print name of module in output
	call get_modulename
	! data from baseline 
	call runbaseline(1)
	! data for table 2	(simulated outcomes)
	call tab2(1)
	! data for table 3 (policy simulations)
	call tab3(1)
	! Alternative Table 3 in Appendix
	call tab3learn(1)
	! data for table 5 (sensitivity, check output for NPV)
	call tab4(1)		
	! data for figure 4 (investment in knowledge)
	call fig4(1)
	! data for figure 5 (basic decomposition)
	call fig5(1)
	! data for figure 6 (heterogeneity in preferences)
	call fig6a(1)	
	! data for figure 6 (epstein-zin preferences)
    call fig6b(1)
	! data for figure 8 (learning by doing)
	call fig8(1)	
	! data for figure 9 (diversification)
	call fig9(1)
	! adhoc figure (initial conditions for appendix)
	call adhoc(1)

end program main
	
