      module procedures_dff

      integer*4 matingtype(1000,1000,2),mtspore(2000000),gs
      integer*4 x,y,nmale
      integer*4,parameter,dimension(4) :: dx=(/1,0,-1,0/), dy=(/0,1,0,-1/)
      real veg_fitness(1000,1000,2),sex_fitness(1000,1000,2),             &
&     sexiness(1000,1000,2),sporetraits(2000000,3)
      logical dff,dmf


      contains
!******* Gaussian random number E=expected, SD=stdev ***************

      function gaussian(expected,stdev) result(gauss_draw)
         implicit none
         real, intent(in):: expected, stdev
         real w, q, gauss_draw
         integer*4 i
         w=0.
         do i=1,12
            call random_number(q)
            w=w+q
         end do
         gauss_draw = (w-6.0)*stdev+expected
      end function gaussian

!******* Function coin_draw E=expected ******************************

      function coin_draw(expected) result(k)
         implicit none
         integer*4, intent(in):: expected
         integer*4 k, i
         real p(2*expected)
         k=0
         call random_number(p)
         do i=1,2*expected
            if (p(i).le.0.5) k=k+1
         end do
      end function coin_draw

!******* Function tradeoff_mutation *********************************

      function tradeoff_mutation(parent,mut_width)
         implicit none
         real parent(:)
         real mutant(size(parent)), tradeoff_mutation(size(parent)),        &
&             outsum, mut_width
         integer*4  k

         mutant = (/(gaussian(parent(k),mut_width),k=1,size(parent))/)

         do k=1,size(parent)
            if (mutant(k).lt.0.) mutant(k)=-1.*mutant(k)
            if (mutant(k).gt.1.) mutant(k)=2.-mutant(k)
         end do

         outsum=sum(mutant)
         tradeoff_mutation=mutant/outsum

      end function tradeoff_mutation


!******* Function torus *********************************************

      function torus(k,m)
         implicit none
         integer*4 k, m, n, torus
         n=mod(k,m)
         if (n.le.0) n=n+m
         torus=n
      end function torus

!******* Function recombinant *********************************************

      function recombinant(parent1,parent2,linkage_disequilibrium)
         implicit none
         real parent1, parent2, recombinant(2),linkage_disequilibrium,alfa
         alfa=1.-linkage_disequilibrium
         recombinant(1)=parent1+alfa*(parent2-parent1)/2.
         recombinant(2)=parent2+alfa*(parent1-parent2)/2.
      end function recombinant

!******* Subroutine mating ************************************************

      subroutine mating(xfoc,yfoc,xmat,ymat,mattype,fitness,nmale)

      implicit none

      integer*4 xfoc,yfoc,xmat,ymat,xx(4),yy(4),x1,y1,x2,y2
      integer*4 mtf1,mtf2,mtn(4,2),ml1,ml2,n1,n2,nm,winmyc(8),winnuc(8)
      integer*4 nmale,k,j,myc,mattype(2),mloc(2),ww,w
      real snn(4,2),sn(4),q,snf1,snf2,max
      real fitness(2,3)

      mtf1=matingtype(xfoc,yfoc,1)
      mtf2=matingtype(xfoc,yfoc,2)
      snf1=sexiness(xfoc,yfoc,1)
      snf2=sexiness(xfoc,yfoc,2)

      nmale=4
      snn=-1.

      do k=1,4                              ! Potential neighbors for sex
         xx(k)=torus(xfoc+dx(k),gs)
         yy(k)=torus(yfoc+dy(k),gs)

         mtn(k,1)=matingtype(xx(k),yy(k),1) ! Neighbor k nucl 1 matingtype
         mtn(k,2)=matingtype(xx(k),yy(k),2) ! Neighbor k nucl 2 matingtype

! If neighbor is empty, or it's a dikaryon but no dikaryon male function is allowed, or if the two mycelia have one pair of matingtype nuclei in common: no mating
      if ((mtn(k,1).eq.0)                                                  &
&            .or.                                                          &
&        ((mtn(k,2).gt.0).and.(.not.dmf))                                  &
&            .or.                                                          &
&        ((mtf1.gt.0).and.((mtn(k,1).eq.(mtf1)).or.(mtn(k,2).eq.(mtf1))))  &
&            .or.                                                          &
&        ((mtf2.gt.0).and.((mtn(k,1).eq.(mtf2)).or.(mtn(k,2).eq.(mtf2))))) &
&          then
             nmale=nmale-1
! print*,"Not proper male:",mtf1,mtf2,mtn(k,1),mtn(k,2)
           else
! otherwise the neighbor is a potential male mycelium to mate with
             snn(k,1)=sexiness(xx(k),yy(k),1)  
             snn(k,2)=sexiness(xx(k),yy(k),2)
! print*, "Compatible male:",mtf1,mtf2,mtn(k,1),mtn(k,2)
         end if
      end do

! print*, "snn array:",snn

      if (nmale.eq.0) then ! If no compatible male neighbor: exit subroutine
         return
      end if

      max=maxval(snn) ! Find the sexiness of sexiest compatible nucleus in neighborhood
      mloc=maxloc(snn)

! print*,max,mloc

! Find all neighboring mycelia containing equally (maximally) sexy nuclei
      winmyc=0
      winnuc=0
      ww=0
      do k=1,4
      do j=1,2
         if (snn(k,j).eq.max) then 
            ww=ww+1
            winmyc(ww)=k ! The mycelium containing a max sexy nucleus
            winnuc(ww)=j ! The max sexy nucleus of that mycelium
            exit
         end if
      end do
      end do
!print*, "Number of potential winners:",ww
!print*, "Winning mycelia", winmyc
!print*, "Winning nucleus", winnuc

!stop

! Choose one of the sexiest neighbor mycelia at random
      call random_number(q) 
      
      w=int(ww*q)+1
      myc=winmyc(w)  ! Neighborhood position of the winning male mycelium
      nm=winnuc(w)   ! Nucleus number (1 or 2) of the winning male nucleus
      xmat=xx(myc)   ! Absolute "x" position of the winning male mycelium 
      ymat=yy(myc)   ! Absolute "y" position of the winning male mycelium
      

! These are the sexinesses of the 2 to 4 nuclei, 2 of which will form the dikaryotic "zygote" 
      sn(1)=snf1          ! Female nucleus 1 sexiness
      sn(2)=snf2          ! Female nucleus 2 sexiness
      sn(3)=snn(myc,1)    ! Male nucleus 1 sexiness
      sn(4)=snn(myc,2)    ! Male nucleus 2 sexiness

!print*,"Male   1",snn(:,1),"Focal 1",snf1
!print*,"Male   2",snn(:,2),"Focal 2",snf2
!print*,"4 nuclei",sn

! No dikaryon female function: female nucleus1 + sexiest male neighbor nucleus form the "zygote" dikarion 
      if (.not.dff) then  
!if (matingtype(xfoc,yfoc,2).ne.0) print*,"Hiba!!"
         x1=xfoc; y1=yfoc; n1=1
         x2=xmat; y2=ymat; n2=nm
         goto 25
      end if

! With dikaryon female function: choose the sexiest pair from the max. 4 nuclei to form the "zygote" dikaryon

      ml1=maxloc(sn,1)
      sn(ml1)=-1
      ml2=maxloc(sn,1)

      select case (ml1)
         case (1) 
              x1=xfoc; y1=yfoc; n1=1
         case (2) 
              x1=xfoc; y1=yfoc; n1=2
         case (3) 
              x1=xmat; y1=ymat; n1=1
         case (4) 
              x1=xmat; y1=ymat; n1=2
      end select

      select case (ml2)
         case (1) 
              x2=xfoc; y2=yfoc; n2=1
         case (2) 
              x2=xfoc; y2=yfoc; n2=2
         case (3) 
              x2=xmat; y2=ymat; n2=1
         case (4) 
              x2=xmat; y2=ymat; n2=2
      end select

! Replace all the nucleus-specific parameters of the two mating mycelia with those of the two nuclei of the "zygote"
  25  mattype(1)  =matingtype(x1,y1,n1)
      fitness(1,1)=veg_fitness(x1,y1,n1)
      fitness(1,2)=sex_fitness(x1,y1,n1)
      fitness(1,3)=sexiness(x1,y1,n1)
!print*,"Nucleus 1 of zygote:",mattype(1),fitness(1,:)
      mattype(2)  =matingtype(x2,y2,n2)
      fitness(2,1)=veg_fitness(x2,y2,n2)
      fitness(2,2)=sex_fitness(x2,y2,n2)
      fitness(2,3)=sexiness(x2,y2,n2)
!print*,"Nucleus 2 of zygote:",mattype(2),fitness(2,:)

if ((mattype(1).lt.1).or.(mattype(1).gt.50).or.(mattype(2).lt.0).or.(mattype(2).gt.50).or.(mattype(1).eq.mattype(2))) then
        print*,"Invalid mating type",mattype
        nmale=0
        
end if

! print*, "Mating",x1,y1,n1,mattype(1),"with",x2,y2,n2,mattype(2) 

      end subroutine mating

!******* Subroutine spread_nuclei *****************************************

      subroutine spread_nuclei(startx,starty,mattype,nucleus)

      implicit none
      integer*4 list(1000000,2),focal(2),mattype(2),i
      integer*4 startx, starty, xlist,ylist,xnext,ynext
      integer*4 n,m
      real nucleus(2,3)

      list=0

      list(1,1)=startx
      list(1,2)=starty
      focal(1)=matingtype(startx,starty,1)
      focal(2)=matingtype(startx,starty,2)     ! "target" mating type(s)

! print*, focal,mattype

      if ((focal(1).eq.mattype(1)).and.(focal(2).eq.mattype(2))) then
         return
      end if


      n=0
      m=1

      do while (n.ne.m)
         n=n+1
         xlist=list(n,1)
         ylist=list(n,2)
         

         do i=1,4
            xnext=torus(xlist+dx(i),gs)     ! neighbor's x coordinate
            ynext=torus(ylist+dy(i),gs)     ! neighbor's y coordinate
          if ((matingtype(xnext,ynext,1).eq.focal(1)).and.                    &
&             (matingtype(xnext,ynext,2).eq.focal(2)))    then
            m=m+1

! if (m.gt.1000000) print*,list(1:100,:)

            list(m,1)=xnext
            list(m,2)=ynext

            matingtype(xnext,ynext,1)=mattype(1)
            veg_fitness(xnext,ynext,1)=nucleus(1,1)
            sex_fitness(xnext,ynext,1)=nucleus(1,2)
            sexiness(xnext,ynext,1)=nucleus(1,3)
            matingtype(xnext,ynext,2)=mattype(2)
            veg_fitness(xnext,ynext,2)=nucleus(2,1)
            sex_fitness(xnext,ynext,2)=nucleus(2,2)
            sexiness(xnext,ynext,2)=nucleus(2,3)

!print*,xnext,ynext,matingtype(xnext,ynext,2),veg_fitness(xnext,ynext,2),sex_fitness(xnext,ynext,2),sexiness(xnext,ynext,2)

          end if
         end do

      end do

      matingtype(startx,starty,1)=mattype(1)
      veg_fitness(startx,starty,1)=nucleus(1,1)
      sex_fitness(startx,starty,1)=nucleus(1,2)
      sexiness(startx,starty,1)=nucleus(1,3)
      matingtype(startx,starty,2)=mattype(2)
      veg_fitness(startx,starty,2)=nucleus(2,1)
      sex_fitness(startx,starty,2)=nucleus(2,2)
      sexiness(startx,starty,2)=nucleus(2,3)

      end subroutine spread_nuclei

!****** Function matingtype_distribution ****************************

      function matingtype_distribution(grid) result(mtd)

         implicit none
         integer*4 i,j,mtd(0:9),grid(1000,1000,2)
         mtd=0
         do i=1,gs
         do j=1,gs
            mtd(grid(i,j,1))=mtd(grid(i,j,1))+1
            mtd(grid(i,j,2))=mtd(grid(i,j,2))+1
         end do
         end do

      end function matingtype_distribution

!********************************************************************

      end module procedures_dff
