*define linuxlit
*define linux
*define fourbyt
*define hconden
*define impnon
*define in32
*define newnrc
*define ploc
*define sphaccm
*define unix
*define noselap
*define noextvol
*define noextv20
*define noextsys
*define noextjun
*define noextj20
*define nonanscr
*define nonpa
*define nomap
*deck strip
       subroutine strip
c
c  $Id: selap.s,v 2.104 1999/06/11 16:27:15 randyt Exp randyt $
c
c  strip writes the stripf file by copying user selected information
c  from the restart-plot file.
c
c  Cognizant engineer: rjw.
c
cblh $if def,impnon,1
*if def,impnon
       implicit none
*endif
!       include 'comctl.h'
!       include 'contrl.h'
!       include 'fast.h'
!       include 'machas.h'
!       include 'machns.h'
!       include 'machos.h'
!       include 'machss.h'
!       include 'maxmem.h'
!       include 'ufiles.h'
c
       integer inx(2),outx(2)
       integer i,ib,ii,in,io,j,k,kb,ke,l,lcontg,lcntgs,le,ler,li,lx,nerr
cblh $if -def,bufr,5
*if -def,bufr
       integer filsz4,iwrd
cblh $if def,fourbyt,3
*if def,fourbyt
       integer inx4
       real*4 ha(2*lfsiz)
       equivalence (ha(1),fa(1))
*endif
*endif
cblh $if def,bufr,1
*if def,bufr
*      integer ibo
*endif
       real*8 ftbnid
       character chwrd*8
       logical hit,hit1,hit2
       external ftbnid,ftbrsv,ftbsft,lcontg,lcntgs,unsqoz
cblh $if def,bufr,1
*if def,bufr
*      external ftbexp,ftbrdc
*endif
c
      include 'machaf.h'
      include 'machnf.h'
      include 'machof.h'
      include 'machsf.h'
c
c  Move file 2 containing requests to low part of memory.
       call ftbsft (filid(2),filsiz(2),2,filndx(2))
c  Get space for position indicators.
       filid(4) = ftbnid(1)
       filid(5) = -filid(4)
       filid(3) = filid(4) + 1.0d0
       filsiz(4) = filsiz(2)/3
cblh $if -def,bufr,1
*if -def,bufr
       filsz4 = filsiz(4) + 2
*endif
       if (filsiz(4) .gt. lcontg(1)) go to 100
       call ftbrsv (filid(4),filsiz(4),-1,kb)
       ke = kb + filsiz(4) - 1
c  Get buffers for stripf file.
       if (filsiz(3) .gt. lcontg(1)) go to 100
       call ftbrsv (filid(3),filsiz(3),-1,outx(1))
       outx(2) = outx(1) + filsiz(4) + 2
       chwrd = 'plotrec'
       read (chwrd,'(a8)') fa(outx(1))
       fa(outx(2)) = fa(outx(1))
       io = 2
c  Get buffer and read plotinf record.
       if (lcontg(1) .lt. 4) go to 100
       filsiz(5) = 4
       call ftbrsv (filid(5),filsiz(5),-1,inx(1))
cblh $if def,fourbyt,1
*if def,fourbyt
       inx4 = 2*inx(1)
*endif
cblh $if def,bufr,4
*if def,bufr
*      buffer in (rstplt,1) (fa(inx(1)),fa(inx(1)+3))
*      lx = unit(rstplt)
*      if (lx .ge. 0) go to 61
*      lx = length(rstplt)
*endif
*if -def,bufr
         read (rstplt,end=74,err=62) lx,iwrd
         read (rstplt,end=74,err=62) (fa(in+inx(1)-1),in=1,lx)
*endif
       write (chwrd,'(a8)') fa(inx(1))
       if (chwrd .ne. 'plotinf') go to 204
       ib = 1
   20  if (lx .ne. 3) go to 205
       ler = ia(2,inx(ib)+2)
       le = ia(2,inx(ib)+1)
       li = le - 1
       j = le + 1
*if def,bufr
*      filsiz(5) = 2*j
*      i = filsiz(5) - lcntgs(filid(5),1)
*      if (i .gt. 0) then
*        call ftbexp (1,i,0)
*      endif
*        if (filsiz(5) .gt. lcntgs(filid(5),1)) go to 100
*        call ftbsft (filid(5),filsiz(5),2,inx(1))
*        call ftbrdc
*endif
cblh $if -def,bufr,2
*if -def,bufr
       filsiz(5) = lcntgs(filid(5),1)
       call ftbsft (filid(5),filsiz(5),2,inx(1))
*endif
       inx(2) = inx(1) + j
c  Read plotalf and plotnum records.
cblh $if def,bufr,4
*if def,bufr
*      buffer in (rstplt,1) (fa(inx(1)),fa(inx(1)+le))
*      lx = unit(rstplt)
*      if (lx .ge. 0) go to 61
*      if (length(rstplt) .ne. le) go to 102
*endif
*if -def,bufr
         read (rstplt,end=74,err=62) lx,iwrd
         read (rstplt,end=74,err=62) (fa(in+inx(1)-1),in=1,lx)
       if (lx .ne. le) go to 102
*endif
       write (chwrd,'(a8)') fa(inx(1))
       if (chwrd .ne. 'plotalf') go to 202
cblh $if def,bufr,4
*if def,bufr
*      buffer in (rstplt,1) (fa(inx(2)),fa(inx(2)+le))
*      lx = unit(rstplt)
*      if (lx .ge. 0) go to 61
*      if (length(rstplt) .ne. le) go to 102
*endif
*if -def,bufr
         read (rstplt,end=74,err=62) lx,iwrd
         read (rstplt,end=74,err=62) (fa(in+inx(2)-1),in=1,lx)
       if (lx .ne. le) go to 102
*endif
       write (chwrd,'(a8)') fa(inx(2))
       if (chwrd .ne. 'plotnum') go to 202
c  Set pointers by scanning plotalf-plotnum records.
       i = filndx(2)
       do 22 k = kb,ke
         ia(2,k) = 0
         do 23 l = 1,li
cblh $if -def,in32,2
*if -def,in32
*          if (ia(inx(1)+l).eq.ia(i+1) .and. ia(inx(2)+l).eq.ia(i+2))
*    *     go to 24
*endif
cblh $if def,in32,2
*if def,in32
           if (ia(1,inx(1)+l).eq.ia(1,i+1) .and. ia(2,inx(1)+l).eq.ia(2,
     #i+1)
     *     .and. ia(2,inx(2)+l).eq.ia(2,i+2)) go to 24
*endif
   23    continue
         go to 25
   24    ia(2,i) = ior(ia(2,i),ishft(1,24))
         ia(2,k) = l
   25    i = i + 3
   22  continue
cblh $if def,bufr,2
*if def,bufr
*  27  buffer in (rstplt,1) (fa(inx(1)),fa(inx(1)+le))
*      ib = 2
*endif
cblh $if -def,bufr,1
*if -def,bufr
   27  ib = 1
*endif
cblh $if def,bufr,5
*if def,bufr
*  10  if (unit(rstplt)) 11,12,13
*  11  lx = length(rstplt)
*      ibo = ib
*      ib = ib + 1
*      if (ib .gt. 2) ib = 1
*endif
*if -def,bufr
   10  continue
         read (rstplt,end=12,err=10) lx,iwrd
*if def,fourbyt
         if (iwrd .eq. 4) then
           read (rstplt,end=12,err=10) (ha(in+inx4-1),in=1,lx)
         else
           read (rstplt,end=12,err=10) (fa(in+inx(1)-1),in=1,lx)
         endif
*endif
*if -def,fourbyt
*        read (rstplt,end=12,err=10) (fa(in+inx(1)-1),in=1,lx)
*endif
*endif
       write (chwrd,'(a8)',err=10) fa(inx(ib))
       if (chwrd .eq. 'plotinf') go to 20
cblh $if def,bufr,1
*if def,bufr
*      buffer in (rstplt,1) (fa(inx(ibo)),fa(inx(ibo)+le))
*endif
       if (chwrd .ne. 'plotrec') go to 10
       if (ler .ne. 0) then
         if (lx .ne. ler) go to 102
         call unsqoz (fa(inx(ib)+1),li)
       else
         if (lx .ne. le) go to 102
       endif
       io = io + 1
       if (io .gt. 2) io = 1
       j = outx(io)
       fa(j+1) = fa(inx(ib)+1)
cdir$ ivdep
       do 15 k = kb,ke
         j = j + 1
         fa(j+1) = fa(inx(ib)+ia(2,k))
         if (ia(2,k) .eq. 0) fa(j+1) = 0.0d0
   15  continue
cblh---------------------------------------------
cblh       if (iand(print,15) .ne. 0) then
       if ((isrestartenabled0).or.
     &     (ismajoreditenabled1).or.
     &     (isminoreditenabled2).or.
     &     (isplotenabled3)) then
cblh---------------------------------------------
         write (stripf,2009,iostat=nerr) (fa(ii),ii=outx(io),j+1)
 2009  format (a10,5x,1p,4e15.6,:/(5e15.6))
         if (nerr .ne. 0) go to 101
       else
cblh $if def,bufr,2
*if def,bufr
*      if (unit(stripf) .ge. 0.0) go to 101
*      buffer out (stripf,1) (ia(outx(io)),ia(j+1))
*endif
cblh $if -def,bufr,1
*if -def,bufr
       write (stripf) filsz4,(fa(in),in=outx(io),j+1)
*endif
       endif
       go to 10
c  End of rstplt file reached.
cblh $if def,bufr,3
*if def,bufr
*blh---------------------------------------------
*blh   12  if (iand(print,15) .eq. 0) then
*  12  if (.not.(isrestartenabled0).and.
*    &     .not.(ismajoreditenabled1).and.
*    &     .not.(isminoreditenabled2).and.
*    &     .not.(isplotenabled3)) then
*blh---------------------------------------------
*        if (unit(stripf) .ge. 0.0) go to 101
*      endif
*endif
cblh $if -def,bufr,1
*if -def,bufr
   12  continue
*endif
       close (unit=stripf)
       hit1 = .false.
       hit2 = .false.
       kb = filndx(2)
       ke = kb + filsiz(2) - 1
       do 30 k = kb,ke,3
         hit = iand(ia(2,k),ishft(1,24)) .ne. 0
         hit1 = hit1 .or. hit
         hit2 = hit2 .or. .not.hit
   30  continue
       if (hit1) go to 31
       write (output,2031)
 2031  format ('0$$$$$$$$ None of the strip request variables were found
     * on the rstplt file.')
       go to 40
   31  if (.not.hit2) go to 40
       write (output,2032)
 2032  format ('0The following strip request variables were not found on
     * the rstplt file.'/
     * ' req.num.    variable code    parameter')
       do 32 k = kb,ke,3
         if (iand(ia(2,k),ishft(1,24)) .ne. 0) go to 32
         i = iand(ia(2,k),not(ishft(1,24)))
         write (output,2033) i,fa(k+1),ia(2,k+2)
 2033  format (i9,4x,a10,i16)
   32  continue
   40  write (output,2005)
 2005  format ('0Successful end of strip processing.')
       go to 200
c
  100  write (output,2001)
 2001  format ('0******** Insufficient space for writing strip file.')
       go to 201
  101  write (output,2002)
 2002  format ('0******** Write error on stripf file.')
       go to 201
cblh $if def,bufr,3
*if def,bufr
*  13  write (output,2003)
*2003  format ('0******** Read error on rstplt file.')
*      go to 201
*endif
  102  write (output,2004)
 2004  format ('0******** A plotalf, plotnum, or plotrec record on rstpl
     *t has the wrong length.')
       go to 201
cblh $if def,bufr,1
*if def,bufr
*  61  if (lx .eq. 0) go to 74
*endif
   62  write (output,2022)
 2022  format ('0******** Parity error on file rstplt reading plotalf or
     * plotnum record.')
       go to 201
   74  write (output,2026)
 2026  format ('0******** Eof on file rstplt before reading plotalf or p
     *lotnum record.')
       go to 201
  202  write (output,2024)
 2024  format ('0******** Record label should be plotalf or plotnum but
     *is not.')
       go to 201
  204  write (output,2015)
 2015  format ('0******** Plotinf record not read when it should have be
     *en read.')
       go to 201
  205  write (output,2016)
 2016  format ('0******** Plotinf record is wrong length.')
  201  fail = .true.
c
cblh $if def,bufr,1
*if def,bufr
* 200  call ftbexp (0,maxscm,maxlcm)
*endif
cblh $if -def,bufr,1
*if -def,bufr
  200  continue
*endif
       return
       end
c***********************************************************************
c
c Data dictionary for local variables
c
c Number of local variables =   28
c
c i=integer r=real l=logical c=character
c***********************************************************************
c Type    Name                              Definition
c-----------------------------------------------------------------------
c  c      chwrd*8                          =
c  i      filsz4                           =
c  r      ftbnid                           =
c  l      hit                              =
c  l      hit1                             =
c  l      hit2                             =
c  i      i                                =
c  i      ib                               =
c  i      ibo                              =
c  i      ii                               =
c  i      in                               =
c  i      inx(2)                           =
c  i      inx4                             =
c  i      io                               =
c  i      iwrd                             =
c  i      j                                =
c  i      k                                =
c  i      kb                               =
c  i      ke                               =
c  i      l                                =
c  i      lcntgs                           =
c  i      lcontg                           =
c  i      le                               =
c  i      ler                              =
c  i      li                               =
c  i      lx                               =
c  i      nerr                             =
c  i      outx(2)                          =
c***********************************************************************
