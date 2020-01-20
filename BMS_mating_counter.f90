      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !                                                             !
      !             program BasidiomyceteSexualSelection            !
      !         Written and tested: February-September 2018,        !
      !                         Wageningen                          !
      !                                                             !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!********************************************************************
      program BasidiomyceteSexualSelection

      use procedures_dff

      implicit none

      integer*4 i,j,k,winner,spdispr,sporeposition(2000000,2)
      integer*4 nind,ndea,nveg,nspo,ssr
      integer*4 mat(2)
      integer*4 Myseed(1),tt,nmt,ini,ngen,nmap,s
      integer*4 update,xn,yn
      integer*4 sporenumber,locspnum,locspores(1000)
      integer*4 m
      integer*4 num_mat

      real p(6), pp(3), q, dr, mr, sigma 
      real wfit(0:4),w,vgr,lde,ww(3,2),theta,sum
      real fit(2,3)
!These are for the average of the fitness components for the cells
      real avg_cell(3)
!And the average for the spores
      real avg_spo(3)
!if the number of mating types is increased, then this needs to be increased too!!!!!
      real mat_counter(50),shannon
      double precision shannon_sum
      character*1024 filename,filenameroot

      open(1,file='Temporary_Input.dat')
      open(2,file='Temporary_Output.res')

!***** Parameter and variable input *********************************
      
      read (1,*) nmt,dmf,dff,vgr,lde,theta,sigma,ssr,dr,mr,spdispr,gs,ini,ngen,   &
&         Myseed,nmap

      write(*,600) nmt,dmf,dff,vgr,lde,theta,sigma,ssr,dr,mr,spdispr,gs,ini,ngen,   &
&         Myseed,nmap
      write(2,600) nmt,dmf,dff,vgr,lde,theta,sigma,ssr,dr,mr,spdispr,gs,ini,ngen,   &
&         Myseed,nmap
      write(2,601) nmt,dmf,dff,vgr,lde,theta,sigma,ssr,dr,mr,spdispr
           
  600 format (/////i10//2(l10//)4(f10.2//)i10//f10.2//f10.5//i10///5(i10//))
  601 format ("BMS_LD_nmt=",i0,"_dmf=",l1,"_dff=",l1,"_vgr=",f3.1,"_lde=",f3.1,   &
&     "_theta=",f3.1,"_sigma=",f3.1,"_ssr=",i0,"_dr=",f3.1,"_mr=",       &
&     f4.2,"_spdispr=",i0,//)
  602 format (i10/2(l10/)4(f10.2/)i10/f10.2/f10.5/i10/5(i10/))


      close(1)

      


!***** Initial state ************************************************
      print *, "Gen nind ndea nveg nspo num_mat shannon avg_spo1 avg_spo2 avg_spo3 avg_cell1 avg_cell2 avg_cell3"    
      tt=0
      matingtype=0
      veg_fitness=0.
      sex_fitness=0.
      sexiness=0.

      nind=0
      ndea=0
      nveg=0
      nspo=0
      mat_counter=0
! Place "ini" no. of spores of random matingtype on grid at random sites
      do 10 i=1,ini 
    5    call random_number(p)
         x=int(gs*p(1))+1
         y=int(gs*p(2))+1
         if (matingtype(x,y,1).ne.0) goto 5
         call random_number(q)
         matingtype(x,y,1)=int(q*nmt)+1

! Allocate traded-off fitness components to the initial spores at random
         call random_number(pp)
         sum=pp(1)+pp(2)+pp(3)
         
         veg_fitness(x,y,1)=pp(1)/sum
         sex_fitness(x,y,1)=pp(2)/sum
         sexiness(x,y,1)=pp(3)/sum

         nind=nind+1

   10 continue

      write(2,50) "  time #mycelia #deaths #veget #spore" 

   50 format (a37/)
   51 format (i5,4i8)

      goto 200

 !***** Site update *************************************************

  100 tt=tt+1
      !print*, "Time:",tt

      ndea=0
      nveg=0
      nspo=0

      do 40 update=1,gs**2
         call random_number(p)       ! Choose focal site
         x=int(gs*p(1))+1
         y=int(gs*p(2))+1

! If site is empty, check for its occupation by vegetative growth
         if (matingtype(x,y,1).eq.0) goto 105

! print*, "Checking site",x,y,matingtype(x,y,1),matingtype(x,y,2)


!_____ Check for survival ___________________________________________

         call random_number(q)
         if (q.gt.dr) goto 110


         do 20 i=1,2
            matingtype(x,y,i)=0
            veg_fitness(x,y,i)=0.
            sex_fitness(x,y,i)=0.
            sexiness(x,y,i)=0.
   20    continue

         nind=nind-1
         ndea=ndea+1

 ! print*, ndea, "Death at site:", x,y

         goto 40

!_____ Sex attempt in the "female" role _____________________________

  110    if ((matingtype(x,y,2).gt.0).and.(.not.dff)) goto 40      


         call mating(x,y,xn,yn,mat,fit,nmale) ! Mate with sexiest neighbor
         if (nmale.le.0) goto 40

! Spread the nuclei of the "zygote" mycelium along both "parental" mycelium clusters 
         call spread_nuclei(x,y,mat,fit) 
         call spread_nuclei(xn,yn,mat,fit)

! print*,x,y,matingtype(x,y,:),xn,yn,matingtype(xn,yn,:)

         goto 40

!_____ Vegetative growth _______________________________________________

  105    call random_number(q)
         if (q.gt.vgr) goto 40

         wfit=0. ! wfit will be the cumulative vegetative fitness vector of the four neighbours of the empty site

         call random_number(q)
         
         do 155 i=1,4  ! go through the four neighbours of the empty site
            xn=torus(x+dx(i),gs)
            yn=torus(y+dy(i),gs)

! The vegetative fitness of a monokaryon is that of the (single) nucleus it has; the vegetative fitness of a dikaryon is the weighted average of the vegetative fitnesses of its two nuclei. The weight (Theta) specifies the dominance of the more fit nucleus over the less fit one (Theta = 1 is absolute dominance, Theta = 0 is absolute recessivity)


            if (matingtype(xn,yn,2).eq.0) then
               wfit(i)=wfit(i-1)+veg_fitness(xn,yn,1)
              else
               wfit(i)=wfit(i-1)+theta*amax1(veg_fitness(xn,yn,1), & 
&              veg_fitness(xn,yn,2))+(1.-theta)*                   &
&              amin1(veg_fitness(xn,yn,1),veg_fitness(xn,yn,2))
            end if

  155    continue

         do 156 i=1,4
            if (wfit(i).le.q*wfit(4)) goto 156
            xn=torus(x+dx(i),gs)
            yn=torus(y+dy(i),gs)
            matingtype(x,y,1)=matingtype(xn,yn,1)
            matingtype(x,y,2)=matingtype(xn,yn,2)
            veg_fitness(x,y,1)=veg_fitness(xn,yn,1)
            veg_fitness(x,y,2)=veg_fitness(xn,yn,2)
            sex_fitness(x,y,1)=sex_fitness(xn,yn,1)
            sex_fitness(x,y,2)=sex_fitness(xn,yn,2)
            sexiness(x,y,1)=sexiness(xn,yn,1)
            sexiness(x,y,2)=sexiness(xn,yn,2)

            nind=nind+1
            nveg=nveg+1

! print*, nveg,"Veg. growth to",x,y,"from",xn,yn

            goto 40
  156    continue

   40 continue

avg_cell = 0.
m = 0
mat_counter=0
num_mat=0
shannon=0.
shannon_sum=0.
        do 157 i=1,gs
        do 157 j=1,gs
           if (matingtype(i,j,1).eq.0) goto 157
           m = m + 1
           avg_cell(1) = avg_cell(1) + veg_fitness(i,j,1)
           avg_cell(2) = avg_cell(2) + sex_fitness(i,j,1)
           avg_cell(3) = avg_cell(3) + sexiness(i,j,1)
           !Since 5 lines previous checked for mating type i,j,1 is not zero, can skip occupancy check
           mat_counter(matingtype(i,j,1)) = mat_counter(matingtype(i,j,1)) + 1
	   
           if (matingtype(i,j,2).ne.0) then
              mat_counter(matingtype(i,j,2)) = mat_counter(matingtype(i,j,2)) + 1
           end if

  157 continue
  !print*, mat_counter(1:nmt)
  !This calculates the number of mating types
  do 158 k=1,nmt
    if (mat_counter(k).ne.0) num_mat = num_mat + 1
    shannon_sum = shannon_sum + mat_counter(k)
  !now to calculate the shannon diversity index
  158 continue
  do 159 k=1,nmt
    if (mat_counter(k).ne.0) then
    shannon = shannon + (mat_counter(k)/shannon_sum)*(log(mat_counter(k)/shannon_sum))
    !print*, mat_counter(k), mat_counter(k),shannon_sum,mat_counter(k)/shannon_sum
    end if
  159 continue
  avg_cell(1) = avg_cell(1)/m
  avg_cell(2) = avg_cell(2)/m
  avg_cell(3) = avg_cell(3)/m


!***** Spore recruitment and dispersal ******************************

! Expected number of spores produced in a cell = ssr/2 x dikaryon sex_fitness for each of the nuclei in a dikaryon (fair meiosis). Sex fitness of a dikaryotic mycelium is the average of the sex fitnesses of the two nuclei. The new spore phenotypes will be drawn from Gaussians centered at the "recombinants" of the parent nucleus phenotypes with standard deviation mr. The recombinant phenotypes of the spores are the weighted averages of the phenotypes of the parent nuclei, the weight is lde, the "phenotypic linkage disequilibrium" of the nuclei (lde=0. is complete linkage equilibrium: the recombinant feature is the average of the parental features; at lde=1. nuclear features do not recombine at all). Mating type is inherited from the parent nucleus, and the spores are dispersed on the lattice at random at the end of the "season" (generation).

      if (ssr.le.0) goto 200
      avg_spo = 0.
      mtspore=0
      sporetraits=0.
      s=0
      do 300 i=1,gs
      do 300 j=1,gs

         if (matingtype(i,j,2).eq.0) goto 300
      
         ww(1,:)=recombinant(veg_fitness(i,j,1),veg_fitness(i,j,2),lde)
         ww(2,:)=recombinant(sex_fitness(i,j,1),sex_fitness(i,j,2),lde)
         ww(3,:)=recombinant(sexiness(i,j,1),sexiness(i,j,2),lde)

! Dikarya attempt to produce spores, at a rate dependent on the weighted average of the sex-fitnesses of their nuclei. The weight (Sigma) specifies the dominance of the more fit nucleus over the less fit one (Sigma = 1 is absolute dominance, Sigma = 0 is absolute recessivity). The chances of the two (recombined) nuclei of getting into the spores are the same. Spores are locally dispersed on the grid onto sites within the dispersal radius of the parental dikaryon.                             

         w=sigma*amax1(sex_fitness(i,j,1), sex_fitness(i,j,2)) +(1.-sigma)*amin1(sex_fitness(i,j,1), sex_fitness(i,j,2))


         do k=1,ssr

         call random_number(p)    

         if (p(1).le.w) then
            s=s+1
            mtspore(s)=matingtype(i,j,1)
            sporetraits(s,:)=tradeoff_mutation(ww(:,1),mr)
            sporeposition(s,1)=torus(i+int(p(3)*(2*spdispr+1))-spdispr,gs)
            sporeposition(s,2)=torus(j+int(p(4)*(2*spdispr+1))-spdispr,gs)
            avg_spo(1) = avg_spo(1) + sporetraits(s,1)
            avg_spo(2) = avg_spo(2) + sporetraits(s,2)
            avg_spo(3) = avg_spo(3) + sporetraits(s,3)
         end if

         if (p(2).le.w) then
            s=s+1
            mtspore(s)=matingtype(i,j,2)
            sporetraits(s,:)=tradeoff_mutation(ww(:,2),mr)
            sporeposition(s,1)=torus(i+int(p(5)*(2*spdispr+1))-spdispr,gs)
            sporeposition(s,2)=torus(j+int(p(6)*(2*spdispr+1))-spdispr,gs)
            avg_spo(1) = avg_spo(1) + sporetraits(s,1)
            avg_spo(2) = avg_spo(2) + sporetraits(s,2)
            avg_spo(3) = avg_spo(3) + sporetraits(s,3)
         end if

         end do
  300 continue
  avg_spo(1) = avg_spo(1)/s
  avg_spo(2) = avg_spo(2)/s
  avg_spo(3) = avg_spo(3)/s
  !print *, avg_spo(1), avg_spo(2), avg_spo(3)
  !print*, sporetraits(1,1), sporetraits(1,2),sporetraits(1,3)


! Spores falling on occupied sites are lost, those landing on the same empty site have equal chances to win the site.


      if (s.le.0) goto 200
      sporenumber=s

      do 320 x=1,gs
      do 320 y=1,gs
         if (matingtype(x,y,1).ne.0) goto 320
         locspnum=0
         locspores(:)=0
         do 310 k=1,sporenumber
            if ((sporeposition(k,1).ne.x).or.(sporeposition(k,2).ne.y)) goto 310
            locspnum=locspnum+1
            locspores(locspnum)=k
  310    continue
         if (locspnum.eq.0) goto 320
         
         call random_number(q)
         winner=locspores(int(locspnum*q)+1)
         matingtype(x,y,1)=mtspore(winner)
         veg_fitness(x,y,1)=sporetraits(winner,1)
         sex_fitness(x,y,1)=sporetraits(winner,2)
         sexiness(x,y,1)=sporetraits(winner,3)         
         nspo=nspo+1
         nind=nind+1
  320 continue


!***** Output and statistics ****************************************
  
  200 write(2,51) tt,nind,ndea,nveg,nspo 
      shannon = shannon/log(real(nmt))
      print 999,tt, nind,ndea,nveg,nspo,num_mat,shannon,avg_spo(1),avg_spo(2),avg_spo(3), avg_cell(1), avg_cell(2), avg_cell(3)
  999 format(6I6,F7.3,6F6.3)
      if (tt.ne.100 .and. tt.ne.500 .and. tt.ne.1000 .and. tt.ne.ngen) goto 100

      write(filenameroot,"(a,i0,a,F4.2)") 'BasidiomyceteSex_Ternary_',tt,'_',lde
      filename=trim(filenameroot)//".dat"
      open(3,file=filename)

      write(3,602) nmt,dmf,dff,vgr,lde,theta,sigma,ssr,dr,mr,spdispr,gs,ini,ngen,   &
&         Myseed,nmap



!      write (*,'(NMT,I10)')
! (nmt,dmf,dff,vgr,lde,theta,sigma,ssr,dr,mr,spdispr,gs,ini,ngen,Myseed,nmap)



!  600 format (/////i10//2(l10//)4(f10.2//)i10//f10.2//f10.5//i10///5(i10//))
!  601 format ("BMS_LD_nmt=",i0,"_dmf=",l1,"_dff=",l1,"_vgr=",f3.1,"_lde=",f3.1,   &
!&     "_theta=",f3.1,"_sigma=",f3.1,"_ssr=",i0,"_dr=",f3.1,"_mr=",       &
!&     f4.2,"_spdispr=",i0,//)



      do i=1,gs
      do j=1,gs

      write (3,22) i, j, matingtype(i,j,:),                             &
&     veg_fitness(i,j,1), sex_fitness(i,j,1), sexiness(i,j,1),          &
&     veg_fitness(i,j,2), sex_fitness(i,j,2), sexiness(i,j,2)
   22 format (2i5,3x,2i5,6f10.6)

      end do
      end do

      close(3)

      if (tt.lt.ngen) goto 100

      close(2)

      end program BasidiomyceteSexualSelection      


