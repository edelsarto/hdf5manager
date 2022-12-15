*define linuxlit
*define linux
*define parcs
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
*deck rrestf
       subroutine rrestf (nogof)
c
c  $Id: selap.s,v 2.104 1999/06/11 16:27:15 randyt Exp randyt $
c
c  rrestf reads restart input file.
c
c  Cognizant engineer: rjw.
c
*in32 irsno
*in32 iw
*in32 ihlpsv
*in32end
c
cblh $if def,impnon,1
*if def,impnon
       implicit none
*endif
c
       logical nogof
      include 'comctl.h'
      include 'contrl.h'
      include 'fast.h'
cblh $if def,fourbyt,2
*if def,fourbyt
       integer*4 ha(2*lfsiz)
       equivalence (ha(1),fa(1))
*endif
      include 'genrl.h'
      include 'machas.h'
      include 'machls.h'
      include 'machns.h'
      include 'machos.h'
      include 'machss.h'
      include 'maxmem.h'
      include 'ufiles.h'
      include 'ufilef.h'
c
cgam       integer l3a(7),l3b(10),iw(11),iroutx,ncasx,
       integer l3a(7),l3b(10),iw(2,11),ncasx,
     &         iccm(ncoms),iccs(ncoms),filszo(nfiles),inx(nfiles)
cgam     * iccm(ncoms),iccs(ncoms),iccf(ncoms),filszo(nfiles),inx(nfiles
cgam  savprob... replaces iroutx
       integer savprobtype05, savprobopt611
cgam  savwritecommonblck2(ncoms) replaces iccf(ncoms)
       logical savwritecommonblck2(ncoms)
c
       integer i,ias,in,inxx,irsno(2,6),isiz,iu,iud,ix,ixb,ixx,l1,
     &         lcntgs,lcontg,len,lenb,lenc,lx
       integer localrstblcknumber1531
c
       logical islocalrestartenabled0  ,
     &         islocalmajoreditenabled1,
     &         islocalminoreditenabled2,
     &         islocalplotenabled3     ,
     &         islocalcompleterestart4 ,
     &         localinteractiveflag5   ,
     &         islocalimplicithttrnsfr6,
     &         islocaltwostepflag7     ,
     &         localstdystatetermflg8  ,
     &         localintegrationflag9   ,
     &         islocalathenaopt10      ,
     &         islocalscdapopt11       ,
     &         islocalplotsqzflg12     ,
     &         localtransrotatflag14
cblh---------------------------------------------
cblh       integer ihlpsv(2)
cgam      integer blhihlpsv(2)
c replacement variables for ihlpsv
      logical
     &         islocalprntaccum0         ,
     &         islocalprntbrntrn1        ,
     &         islocalprntccfl2          ,
     &         islocalprntchfcal3        ,
     &         islocalprntconden4        ,
     &         islocalprntdittus5        ,
     &         islocalprnteqfinl6        ,
     &         islocalprntfwdrag7        ,
     &         islocalprntht2tdp9        ,
     &         islocalprnthtfinl12       ,
     &         islocalprnthtrc113        ,
     &         islocalprnthydro15        ,
     &         islocalprntistate17       ,
     &         islocalprntjchoke18       ,
     &         islocalprntjprop19        ,
     &         islocalprntnoncnd20       ,
     &         islocalprntphantj21       ,
     &         islocalprntphantv22       ,
     &         islocalprntpimplt23
      logical
     &         islocalprntpintfc24       ,
     &         islocalprntprednb25       ,
     &         islocalprntpreseq26       ,
     &         islocalprntpstdnb27       ,
     &         islocalprntqfmove28       ,
     &         islocalprntsimplt29       ,
     &         islocalprntstacc0         ,
     &         islocalprntstate1         ,
     &         islocalprntstatep2        ,
     &         islocalprntsuboil3        ,
     &         islocalprntsysitr4        ,
     &         islocalprnttstate6        ,
     &         islocalprntvalve7         ,
     &         islocalprntvexplt8        ,
     &         islocalprntvfinl9         ,
     &         islocalprntvimplt10       ,
     &         islocalprntvlvela11       ,
     &         islocalprntvolvel12       ,
     &         islocalprnttrip13
      logical
     &         islocalprntpower14        ,
     &         islocalprntvolume15       ,
     &         islocalprntjunction16     ,
     &         islocalprntheatstr17      ,
     &         islocalprntradht18        ,
     &         islocalprntreflood19      ,
     &         islocalprntfsnprdtr20     ,
     &         islocalprntcontrol21      ,
     &         islocalprntinput22        ,
     &         islocalprntmiedit23
cblh---------------------------------------------
       real*8 rsno(6),rw(11),fil(nfiles),iv
       equivalence (rsno(1),irsno(1,1)),(rw(1),iw(1,1))
       real*8 fid,fsav,ftbnid
       logical tfail,fl,frst,lperr,rdc,hit,rdebug
       character ihldc*108,ihldp*64,crstrc*24,labl1(2)*9,chwrd*8
       character*8 blnk,dummy,rwch2,rwch3
cblh $if -def,bufr,1
*if -def,bufr
       integer iwrd
*endif
       integer j,niscm
       parameter (niscm=11)
       integer iscm(niscm)
       external aatl,fabend,fmvlwr,ftbchk,ftbdel,ftbexp,ftbnid,ftbout,
     & ftbrdc,ftbrsv,ftbsft,inp2,lcntgs,lcontg
c
       data l3a /104,0,0,1,0,1,-1/
       data l3b /103,0,1,6,0,1,0,2,-1,-1/
       data iscm /7,9,15,17,18,19,22,23,24,26,27/
       data iu /3/, iud /1/
       data crstrc /'restart-plot file'/
       data labl1 /'steady st','transient'/
       data blnk /'        '/
      include 'machaf.h'
      include 'machlf.h'
      include 'machnf.h'
      include 'machof.h'
      include 'machsf.h'
c
       rdebug = .false.
       rdc = .false.
c  Process input control card.
       do 6 i = 1,nfiles
          fil(i) = 0.0d0
          inx(i) = 0
    6  continue
       nogof = .false.
c
c  Read in the 103 card, restart number in word 1
c  and restart-plot file name in words 2-6
c
       l3b(6) = 1
       call inp2 (fa(filndx(1)),rsno,l3b)
       if (l3b(6) .le. 0) then
          nogof = .true.
          go to 41
       endif
   30  if (isprntinput22) write (output,2024) irsno(2,1)
 2024  format ('0Desired restart number is',i9,'.')
c
       if (l3b(6) .gt. 1) then
          if (isprntinput22) write (output,2102)
 2102  format ('0Restart filename specified on 103 card.')
          if (l3b(6) .lt. 7) then
             do 13 j = l3b(6)+1,6
                read (blnk,'(a8)') rsno(j)
   13        continue
             if (isprntinput22) then
                write (filsch(5)(1:40),'(5a8)') (rsno(j),j=2,6)
                write (output,2107) filsch(5)
 2107  format ('0Restart-plot file name changed to: ',a)
             endif
          else
             nogof = .true.
             go to 41
          endif
       endif
c
       len = 0
*if -def,nopen
*if -def,apollo
       open (unit=rstplt,file=filsch(5),status='old',
     &       form='unformatted',iostat=len)
*endif
*if def,apollo
*      open (unit=rstplt,file=filsch(5),status='old',
*    &       form='unformatted',iostat=len,recl=200000)
*endif
*endif
       if (len .ne. 0) then
         write (tty,2013) len,filsch(5)
 2013  format ('Open failure number',i8,' on file ',a,
     &         ', perhaps file is  not available.')
         write (output,2014) len,filsch(5)
 2014  format ('0******** Open failure number',i8,' on file ',a,
     &         ' perhaps file is not available.')
         nogof = .true.
         go to 41
       endif
   21  continue
       rewind rstplt
cblh $if def,bufr,3
*if def,bufr
*      buffer in (rstplt,1) (rw(1),rw(11))
*      if (unit(rstplt)) 38,27,28
*  38  if (length(rstplt) .eq. 10) go to 39
*endif
*if -def,bufr
       read (rstplt,end=27,err=28) len,iwrd
       read (rstplt,end=27,err=28) (rw(i),i=1,10)
*endif
       if (len .eq. 10) go to 39
       write (output,2016)
 2016  format ('0******** Wrong length on first record of restart',
     &         ' input file.')
       nogof = .true.
       go to 41
   27  write (output,2010)
 2010  format ('0******** Eof encountered on first read of restart',
     &         ' input file, no file available.')
       nogof = .true.
       go to 41
   28  write (output,2015)
 2015  format ('0******** Parity error on first record.')
       nogof = .true.
       go to 41
   39  write (ihldc(1:24),'(3a8)') (rw(i),i=4,6)
c
c  Check against the string "restart-plot file"
c  which has to be in the first record of a restart-plot file
c
       if (ihldc(1:24) .eq. crstrc) go to 40
       write (output,2017)
 2017  format ('0******** Label in first record of restart input',
     &         ' file is incorrect.')
       write (output,2003) ihldc(1:24), crstrc
 2003  format (' First record is: ',a,/,
     &         ' It should be   : ',a)
       nogof = .true.
       go to 41
cblh---------------------------------------------
cblh   40  if( iand(ihlppr(2),ishft(1,22)).ne.0 )
cblh     & write (output,2018) (rw(i),i=1,3),(rw(i),i=7,9),labl1(iw(10))
   40  if (isprntinput22)
     &   write (output,2018) (rw(i),i=1,3),(rw(i),i=7,9),labl1(iw(2,10))
cblh---------------------------------------------
 2018  format ('0Restart input file was written by program ',3a8,' on ',
     &         3a8,', ',a9,' run.')
       write (rwch2,'(a8)') rw(1)
       write (rwch3,'(a8)') rw(3)
       call aatl(3, dummy, rwch2, rwch3)
c  Process restart output control card.
   41  if (.not.rdebug) then
         l3a(6) = 1
c
c  Read in the 104 card,
c
         call inp2 (fa(filndx(1)),iv,l3a)
         if (l3a(6)) 10,11,12
   12    write (chwrd,'(a8)') iv
         if (chwrd .eq. 'none') then
            if (isprntinput22) write (output,2005)
 2005  format ('0No restart-plot file is to be written.')
            go to 19
         else
            write (output,2001)
 2001 format ('0Incorrect option on card 104.')
            go to 10
         endif
   11    if (isprntinput22) write (output,2002)
 2002  format ('0Restart-plot file will be written.')
         isrestartenabled0 = .true.
         go to 19
   10    fail = .true.
c
c  Save information from common that will be overlaid by restart reads.
c
   19    newrst = problemopt611 .ne. iw(2,10)
         if (nogof) then
            fail = .true.
            return
         endif
         islocalprntaccum0   = isprntaccum0
         islocalprntbrntrn1  = isprntbrntrn1
         islocalprntccfl2    = isprntccfl2
         islocalprntchfcal3  = isprntchfcal3
         islocalprntconden4  = isprntconden4
         islocalprntdittus5  = isprntdittus5
         islocalprnteqfinl6  = isprnteqfinl6
         islocalprntfwdrag7  = isprntfwdrag7
         islocalprntht2tdp9  = isprntht2tdp9
         islocalprnthtfinl12 = isprnthtfinl12
         islocalprnthtrc113  = isprnthtrc113
         islocalprnthydro15  = isprnthydro15
         islocalprntistate17 = isprntistate17
         islocalprntjchoke18 = isprntjchoke18
         islocalprntjprop19  = isprntjprop19
         islocalprntnoncnd20 = isprntnoncnd20
         islocalprntphantj21 = isprntphantj21
         islocalprntphantv22 = isprntphantv22
         islocalprntpimplt23 = isprntpimplt23
         islocalprntpintfc24 = isprntpintfc24
         islocalprntprednb25 = isprntprednb25
         islocalprntpreseq26 = isprntpreseq26
         islocalprntpstdnb27 = isprntpstdnb27
         islocalprntqfmove28 = isprntqfmove28
         islocalprntsimplt29 = isprntsimplt29
cblh---------------------------------------------
cblh         iroutx = iroute
cblh---------------------------------------------
         islocalprntstacc0     = isprntstacc0
         islocalprntstate1     = isprntstate1
         islocalprntstatep2    = isprntstatep2
         islocalprntsuboil3    = isprntsuboil3
         islocalprntsysitr4    = isprntsysitr4
         islocalprnttstate6    = isprnttstate6
         islocalprntvalve7     = isprntvalve7
         islocalprntvexplt8    = isprntvexplt8
         islocalprntvfinl9     = isprntvfinl9
         islocalprntvimplt10   = isprntvimplt10
         islocalprntvlvela11   = isprntvlvela11
         islocalprntvolvel12   = isprntvolvel12
         islocalprnttrip13     = isprnttrip13
         islocalprntpower14    = isprntpower14
         islocalprntvolume15   = isprntvolume15
         islocalprntjunction16 = isprntjunction16
         islocalprntheatstr17  = isprntheatstr17
         islocalprntradht18    = isprntradht18
         islocalprntreflood19  = isprntreflood19
         islocalprntfsnprdtr20 = isprntfsnprdtr20
         islocalprntcontrol21  = isprntcontrol21
         islocalprntinput22    = isprntinput22
         islocalprntmiedit23   = isprntmiedit23
cblh---------------------------------------------
         islocalrestartenabled0   = isrestartenabled0
         islocalmajoreditenabled1 = ismajoreditenabled1
         islocalminoreditenabled2 = isminoreditenabled2
         islocalplotenabled3      = isplotenabled3
         islocalcompleterestart4  = iscompleterestart4
         localinteractiveflag5    = interactiveflag5
         islocalimplicithttrnsfr6 = isimplicithttrnsfr6
         islocaltwostepflag7      = istwostepflag7
         localstdystatetermflg8   = stdystatetermflg8
         localintegrationflag9    = integrationflag9
         islocalathenaopt10       = isathenaopt10
         islocalscdapopt11        = isscdapopt11
         islocalplotsqzflg12      = isplotsqzflg12
         localtransrotatflag14    = transrotatflag14
         localrstblcknumber1531   = rstblcknumber1531
cblh---------------------------------------------
cblh         iroutx = iand(problemtype05,63) +
cblh     &      ishft(iand(problemopt611,63),6)
cblh---------------------------------------------
cgam         iroutx = problemtype05 + problemopt611*(2**(6))
         savprobtype05 = problemtype05
         savprobopt611 = problemopt611
cblh---------------------------------------------
         ncasx = ncase
         tfail = fail
         ihldc = ctitle
         ihldp = ptitle
         do 23 i = 1,ncoms
           iccm(i) = comdat(i)
           iccs(i) = comdln(i)
cblh---------------------------------------------
cblh           iccf(i) = filflg(i)
cgam  only need to save this bit since it is only one checked below
cga   and iccf is a local variable
           savwritecommonblck2(i) = writecommonblck2(i)
cblh---------------------------------------------
   23    continue
         fsav = filid(1)
         inxx = filndx(1)
         isiz = filsiz(1)
c  Get space for first restart record or plot record.
         fid = ftbnid(1)
       else
         do 22 i = 1,nfiles
           if (fil(i) .ne. 0.0d0) then
             call ftbdel (fil(i))
             fil(i) = 0.0d0
             inx(i) = 0
           endif
   22    continue
         call ftbdel (fid)
       endif
       l1 = lcontg(1)
       call ftbrsv (fid,l1,-1,ix)
       ixx = ix + l1 - 1
       ias = locf4(fa(1)) - 1
       frst = .true.
       lenb = 0
       lperr = .false.
c  Read restart record or plot record.
cblh $if def,bufr,4
*if def,bufr
*  63  buffer in (rstplt,1) (ia(ix),ia(ixx))
*      lx = unit(rstplt)
*      if (lx .ge. 0) go to 61
*      lx = length(rstplt)
*endif
*if -def,bufr
   63  continue
       read (rstplt,end=74,err=62) lx,iwrd
       read (rstplt,end=74,err=62) (fa(in+ix-1),in=1,lx)
*endif
       if (rdebug) write (output,3001) lx,fa(ix),ia(2,ix+1),ia(2,ix+2)
 3001  format (' ',i10,a10,2i10)
       write (chwrd,'(a8)') fa(ix)
       if (chwrd .ne. 'plotrec') go to 70
       if (lx .eq. lenc) go to 63
       go to 56
   70  if (chwrd .ne. 'plotinf') go to 57
       if (lx .ne. 3) go to 56
       do 79 i = 1,nfiles
         if (fil(i) .eq. 0.0d0) go to 79
         call ftbdel (fil(i))
         fil(i) = 0.0d0
         inx(i) = 0
   79  continue
       if (.not.rdc) go to 60
       call ftbexp (0,maxscm,maxlcm)
       rdc = .false.
       call ftbsft (2.0d0,isiz,1,inxx)
   60  lenb  = ia(2,ix+1)
       l1 = lenb + 2
       lenc = ia(2,ix+2)
       if (lcntgs(fid,1) .lt. l1) go to 78
       call ftbsft (fid,l1,2,ix)
       ixx = ix + l1 - 1
       l1 = lcontg(1)
       go to 63
   57  if (chwrd.ne.'plotalf' .and. chwrd.ne.'plotnum') go to 58
       if (lx .eq. lenb) go to 63
   56  if (lperr) go to 63
       write (output,2041)
 2041  format ('0******** Wrong length record in plotinf, plotalf,',
     &         ' plotnum, or plotrec record of rstplt file.')
       lperr = .true.
       tfail = .true.
       go to 63
   58  if (chwrd .ne. 'restart') go to 65
       if (lx .ne. 3) go to 59
c  Read restart information and copy to new restart file.
       fl = iand(ia(2,ix+1),16) .ne. 0
       if (fl) then
cblh $if def,bufr,4
*if def,bufr
*        buffer in (rstplt,1) (fa(ix+3),fa(ix+23))
*        lx = unit(rstplt)
*        if (lx .ge. 0) go to 61
*        lx = length(rstplt)
*endif
*if -def,bufr
         read (rstplt,end=74,err=62) lx,iwrd
         read (rstplt,end=74,err=62) (fa(i+ix+2),i=1,lx)
*endif
         if (rdebug) write (output,3002) lx,fa(ix+3)
 3002  format ('         20',i10,a10)
         if (lx .ne. 20) go to 66
         write (ptitle,'(8a8)') (fa(i+ix+2),i=1,8)
         write (ctitle,'(12a8)') (fa(i+ix+2),i=9,20)
       endif
       do 42 i = 1,nfiles
         filszo(i) = filsiz(i)
   42  continue
       if (frst .and. .not.fl) call fabend
       do 67 i = 1,ncoms
         if (frst) then
           if (i .eq. 6) then
             if (comdat(6) .eq. 0) iccm(6) = 0
           endif
         endif
         if (iccm(i) .eq. 0) go to 71
         if (.not.fl) then
cblh---------------------------------------------
cblh           if (iand(iccf(i),4) .eq. 0) go to 67
           if (.not.savwritecommonblck2(i)) goto 67
cblh---------------------------------------------
         endif
         ixb = iccm(i) - ias
cblh $if def,bufr,4
*if def,bufr
*        buffer in (rstplt,1) (fa(ixb),fa(ixb+iccs(i)))
*        lx = unit(rstplt)
*        if (lx .ge. 0) go to 61
*        lx = length(rstplt)
*endif
*if -def,bufr
*if -def,fourbyt
*        read (rstplt,end=74,err=62) lx,iwrd
*        read (rstplt,end=74,err=62) (fa(in+ixb-1),in=1,lx)
*endif
*if def,fourbyt
         read (rstplt,end=74,err=62) lx,iwrd
         read (rstplt,end=74,err=62) (ha(in+ixb-1),in=1,lx)
*endif
*endif
         if (rdebug) write (output,3003) iccs(i),lx
 3003  format (' cb',i8,i10)
         if (lx .ne. iccs(i)) go to 66
   67  continue
   71  hit = .false.
       do 43 i = 1,nfiles
         hit = hit .or. filsiz(i) .ne. filszo(i)
   43  continue
       if (rdc .and. hit) then
         call ftbexp(0,maxscm,maxlcm)
         rdc = .false.
         do 45 i = 1,nfiles
           if (fil(i) .ne. 0.0d0) then
             call ftbdel (fil(i))
             fil(i) = 0.0d0
             inx(i) = 0
           endif
   45    continue
         call ftbsft (2.0d0,isiz,1,inxx)
         l1 = lcontg(1)
       endif
       do 80 i = 1,nfiles
         if (filid(i)) 83,80,82
cblh---------------------------------------------
cblh   83    if (.not.fl .or. iand(filflg(i),1).eq.0) go to 80
   83    if (.not.fl .or. .not.writedynamicfile0(i)) go to 80
cblh---------------------------------------------
   82    if (inx(i) .ne. 0) go to 81
         l1 = l1 - filsiz(i)
         if (l1 .lt. 0) go to 64
         fil(i) = ftbnid(1)
cblh---------------------------------------------
cblh         if (iand(filflg(i),1) .ne. 0) fil(i) = -fil(i)
         if (writedynamicfile0(i)) fil(i) = -fil(i)
cblh---------------------------------------------
         call ftbrsv (fil(i),filsiz(i),-1,inx(i))
cblh $if def,bufr,4
*if def,bufr
*  81    buffer in (rstplt,1) (fa(inx(i)),fa(inx(i)+filsiz(i)))
*        lx = unit(rstplt)
*        if (lx .ge. 0) go to 61
*        lx = length(rstplt)
*endif
*if -def,bufr
   81  continue
         read (rstplt,end=74,err=62) lx,iwrd
         read (rstplt,end=74,err=62) (fa(in+inx(i)-1),in=1,lx)
*endif
         if (rdebug) write (output,3004) filsiz(i),lx
 3004  format (' db',i8,i10)
         if (lx .ne. filsiz(i)) go to 66
   80  continue
       if (frst) frst = .false.
       if (.not.rdc) then
         call ftbsft (2.0d0,isiz,2,inxx)
         call ftbrdc
         rdc = .true.
       endif
c  Determine if desired restart record has been read.
       if (irsno(2,1) - ia(2,ix+2)) 69,90,63
c
cblh $if def,bufr,1
*if def,bufr
*  61  if (lx .eq. 0) go to 74
*endif
   62  write (output,2022)
 2022  format ('0******** Parity error on file rstplt occurred',
     &         ' before reading requested restart data.')
       go to 76
   74  write (output,2023)
 2023  format ('0******** End of file on file rstplt occurred',
     &         ' before reading requested restart data.')
   76  tfail = .true.
       if (frst) go to 77
       if (.not.rdc) go to 97
       write (output,2034)
 2034  format ('0******** Data previously read will be used, data',
     &         ' may be a combination of indicated restart record',
     &         ' and previous record.')
       go to 90
   77  write (output,2038)
 2038  format ('0******** Error occurred before first restart',
     &         ' record was read.')
       go to 68
   97  write (output,2028)
 2028  format ('0******** Error occurred at restart record',
     &         ' following a renodaliztion.')
       go to 68
   78  write (output,2026)
 2026  format ('0******** Insufficient space to handle restart,',
     &         ' problem will be terminated.')
       tfail = .true.
       go to 68
   64  write (output,2026)
       tfail = .true.
   68  nogof = .true.
       go to 89
c
   65  write (output,2036)
 2036  format ('0******** Wrong information in header of',
     &         ' restart record.')
       go to 76
c
   66  write (output,2042)
 2042  format ('0******** Wrong length record in common or',
     &         ' dynamic restart record.')
       if (rdebug) go to 76
       rdebug = .true.
       write (output,3005)
 3005  format ('0******** Rereading rstplt with diagnostics enabled.')
       go to 21
   59  write (output,2037)
 2037  format ('0******** Wrong length record in header of',
     &         ' restart record.')
       go to 76
c
   69  write (output,2039)
 2039  format ('0******** Desired restart record is not on file,',
     &         ' using record just read.')
       tfail = .true.
c  Return temporary file.
   90  continue
c  restore print control flags
cblh---------------------------------------------
cblh       ihlppr(1) = ihlpsv(1)
       isprntaccum0     = islocalprntaccum0
       isprntbrntrn1    = islocalprntbrntrn1
       isprntccfl2      = islocalprntccfl2
       isprntchfcal3    = islocalprntchfcal3
       isprntconden4    = islocalprntconden4
       isprntdittus5    = islocalprntdittus5
       isprnteqfinl6    = islocalprnteqfinl6
       isprntfwdrag7    = islocalprntfwdrag7
       isprntht2tdp9    = islocalprntht2tdp9
       isprnthtfinl12   = islocalprnthtfinl12
       isprnthtrc113    = islocalprnthtrc113
       isprnthydro15    = islocalprnthydro15
       isprntistate17   = islocalprntistate17
       isprntjchoke18   = islocalprntjchoke18
       isprntjprop19    = islocalprntjprop19
       isprntnoncnd20   = islocalprntnoncnd20
       isprntphantj21   = islocalprntphantj21
       isprntphantv22   = islocalprntphantv22
       isprntpimplt23   = islocalprntpimplt23
       isprntpintfc24   = islocalprntpintfc24
       isprntprednb25   = islocalprntprednb25
       isprntpreseq26   = islocalprntpreseq26
       isprntpstdnb27   = islocalprntpstdnb27
       isprntqfmove28   = islocalprntqfmove28
cblh---------------------------------------------
cblh       ihlppr(2) = ihlpsv(2)
       isprntsimplt29   = islocalprntsimplt29
       isprntstacc0     = islocalprntstacc0
       isprntstate1     = islocalprntstate1
       isprntstatep2    = islocalprntstatep2
       isprntsuboil3    = islocalprntsuboil3
       isprntsysitr4    = islocalprntsysitr4
       isprnttstate6    = islocalprnttstate6
       isprntvalve7     = islocalprntvalve7
       isprntvexplt8    = islocalprntvexplt8
       isprntvfinl9     = islocalprntvfinl9
       isprntvimplt10   = islocalprntvimplt10
       isprntvlvela11   = islocalprntvlvela11
       isprntvolvel12   = islocalprntvolvel12
       isprnttrip13     = islocalprnttrip13
       isprntpower14    = islocalprntpower14
       isprntvolume15   = islocalprntvolume15
       isprntjunction16 = islocalprntjunction16
       isprntheatstr17  = islocalprntheatstr17
       isprntradht18    = islocalprntradht18
       isprntreflood19  = islocalprntreflood19
       isprntfsnprdtr20 = islocalprntfsnprdtr20
       isprntcontrol21  = islocalprntcontrol21
       isprntinput22    = islocalprntinput22
cblh---------------------------------------------
cblh       isprntmiedit23   = iand(ihlpsv(2),2**(23)).ne.0
       isprntmiedit23   = islocalprntmiedit23
cblh---------------------------------------------
cblh       if( iand(ihlppr(2),ishft(1,22)).ne.0 )
cblh     & write (output,2040) ia(ix+2)
       if (isprntinput22) write (output,2040) ia(2,ix+2)
cblh---------------------------------------------
 2040  format ('0Restart record',i8,' found on restart file.')
   89  if (.not.rdc) go to 92
       call ftbexp (0,maxscm,maxlcm)
       call ftbsft (2.0d0,isiz,1,inxx)
   92  call ftbdel (fid)
c  Restore saved information to common.
       isrestartenabled0   = islocalrestartenabled0
       ismajoreditenabled1 = ismajoreditenabled1 .or.
     &                       islocalmajoreditenabled1
       isminoreditenabled2 = isminoreditenabled2 .or.
     &                       islocalminoreditenabled2
       isplotenabled3      = isplotenabled3      .or.
     &                       islocalplotenabled3
       iscompleterestart4  = islocalcompleterestart4
       interactiveflag5    = interactiveflag5   .or.
     &                       localinteractiveflag5
       isimplicithttrnsfr6 = isimplicithttrnsfr6 .or.
     &                       islocalimplicithttrnsfr6
       istwostepflag7      = istwostepflag7      .or.
     &                       islocaltwostepflag7
       stdystatetermflg8   = stdystatetermflg8   .or.
     &                       localstdystatetermflg8
       integrationflag9    = integrationflag9    .or.
     &                       localintegrationflag9
       isathenaopt10       = isathenaopt10       .or.
     &                       islocalathenaopt10
       isscdapopt11        = isscdapopt11        .or.
     &                       islocalscdapopt11
       isplotsqzflg12      = isplotsqzflg12      .or.
     &                       islocalplotsqzflg12
       transrotatflag14    = transrotatflag14    .or.
     &                       localtransrotatflag14
       rstblcknumber1531   = localrstblcknumber1531
cblh---------------------------------------------
cblh       if( iand(ihlppr(2),ishft(1,22)).ne.0 )
cblh     & write (output,2021) ctitle(1:80)
       if (isprntinput22) write (output,2021) ctitle(1:80)
cblh---------------------------------------------
 2021  format ('0Problem title card of restart record being',
     &         ' used is:',a)
cblh---------------------------------------------
cblh       iroute = iroutx
cblh---------------------------------------------
         problemtype05 = savprobtype05
         problemopt611 = savprobopt611
cblh---------------------------------------------
       ncase = ncasx
       ctitle = ihldc
       ptitle = ihldp
       do 24 i = 1,ncoms
         comdat(i) = iccm(i)
         comdln(i) = iccs(i)
   24  continue
       fail = tfail
       filid(1) = fsav
       filndx(1) = inxx
       filsiz(1) = isiz
       do 91 i = 2,nfiles
         filid(i) = fil(i)
         filndx(i) = inx(i)
   91  continue
c
c  Write selected files to disk.
       do 101 j = 1,niscm
         i = iscm(j)
         if (filid(i) .ne. 0.0d0) then
           call ftbrsv (-filid(i),filsiz(i),iu,filndx(i))
           call ftbout (iud,fa(inx(i)),filsiz(i),filndx(i))
           call ftbdel (filid(i))
         endif
  101  continue
       call ftbchk (iud)
       call fmvlwr
c
c  Return if restart has failed and leave restart-plot file alone
c
       if (fail) return
c
c  Rewind rstplt and read over first record if switching between
c  steady state and transient.
c
       if (.not.newrst) return
       rewind rstplt
       read (ptitle(1:24),'(3a8)')   (rw(i),i=1,3)
       read (crstrc,'(3a8)')         (rw(i),i=4,6)
       read (ctitle(82:105),'(3a8)') (rw(i),i=7,9)
c
c  This rewrite of the first record on the restart file
c  changes word 10 from a 1 to a 2 which changes the type of the
c  restart file from a steady-state to a transient run, as per
c  labl1 above in the data statement
c
cblh---------------------------------------------
cblh       iw(10) = ishft(iroute,-6)
       iw(2,10) = problemopt611
cblh---------------------------------------------
cblh $if def,bufr,2
*if def,bufr
*      buffer out (rstplt,1) (rw(1),rw(10))
*      if (unit(rstplt) .ge. 0.0) call fabend
*endif
*if -def,bufr
       lx = 10
       write (rstplt) lx,iwrd
       write (rstplt) (rw(i),i=1,10)
*endif
c
c  So the restart-plot file has to records up front,
c  the first has the stedy state flag set and
c  the second has the transient flag set.
c  The file is not positioned to add more plot and/or
c  restart records to it.
c
       return
       end
c***********************************************************************
c
c Data dictionary for local variables
c
c Number of local variables =  120
c
c i=integer r=real l=logical c=character
c***********************************************************************
c Type    Name                              Definition
c-----------------------------------------------------------------------
c  c*8    blnk                       =
c  c*8    chwrd                      =
c  c*24   crstrc                     =
c  c*8    dummy                      =
c  r      fid                        =
c  r      fil(nfiles)                =
c  i      filszo(nfiles)             =
c  l      fl                         =
c  l      frst                       =
c  r      fsav                       =
c  r      ftbnid                     =
c  l      hit                        =
c  i      i                          =
c  i      ias                        =
c  i      iccm(ncoms)                =
c  i      iccs(ncoms)                =
c  c*108  ihldc                      =
c  c*64   ihldp                      =
c  i      in                         =
c  i      inx(nfiles)                =
c  i      inxx                       =
c  i      irsno(7)                   =
c  i      iscm(niscm)                =
c  i      isiz                       =
c  l      islocalathenaopt10         =
c  l      islocalcompleterestart4    =
c  l      islocalimplicithttrnsfr6   =
c  l      islocalmajoreditenabled1   =
c  l      islocalminoreditenabled2   =
c  l      islocalplotenabled3        =
c  l      islocalplotsqzflg12        =
c  l      islocalprntaccum0          =
c  l      islocalprntbrntrn1         =
c  l      islocalprntccfl2           =
c  l      islocalprntchfcal3         =
c  l      islocalprntconden4         =
c  l      islocalprntcontrol21       =
c  l      islocalprntdittus5         =
c  l      islocalprnteqfinl6         =
c  l      islocalprntfsnprdtr20      =
c  l      islocalprntfwdrag7         =
c  l      islocalprntheatstr17       =
c  l      islocalprntht2tdp9         =
c  l      islocalprnthtfinl12        =
c  l      islocalprnthtrc113         =
c  l      islocalprnthydro15         =
c  l      islocalprntinput22         =
c  l      islocalprntistate17        =
c  l      islocalprntjchoke18        =
c  l      islocalprntjprop19         =
c  l      islocalprntjunction16      =
c  l      islocalprntmiedit23        =
c  l      islocalprntnoncnd20        =
c  l      islocalprntphantj21        =
c  l      islocalprntphantv22        =
c  l      islocalprntpimplt23        =
c  l      islocalprntpintfc24        =
c  l      islocalprntpower14         =
c  l      islocalprntprednb25        =
c  l      islocalprntpreseq26        =
c  l      islocalprntpstdnb27        =
c  l      islocalprntqfmove28        =
c  l      islocalprntradht18         =
c  l      islocalprntreflood19       =
c  l      islocalprntsimplt29        =
c  l      islocalprntstacc0          =
c  l      islocalprntstate1          =
c  l      islocalprntstatep2         =
c  l      islocalprntsuboil3         =
c  l      islocalprntsysitr4         =
c  l      islocalprnttrip13          =
c  l      islocalprnttstate6         =
c  l      islocalprntvalve7          =
c  l      islocalprntvexplt8         =
c  l      islocalprntvfinl9          =
c  l      islocalprntvimplt10        =
c  l      islocalprntvlvela11        =
c  l      islocalprntvolume15        =
c  l      islocalprntvolvel12        =
c  l      islocalrestartenabled0     =
c  l      islocalscdapopt11          =
c  l      islocaltwostepflag7        =
c  i      iu                         =
c  i      iud                        =
c  r      iv                         =
c  i      iw(11)                     =
c  i      iwrd                       =
c  i      ix                         =
c  i      ixb                        =
c  i      ixx                        =
c  i      j                          =
c  i      l1                         =
c  i      l3a(7)                     =
c  i      l3b(10)                    =
c  c*9    labl1(2)                   =
c  i      lcntgs                     =
c  i      lcontg                     =
c  i      len                        =
c  i      lenb                       =
c  i      lenc                       =
c  l      localintegrationflag9      =
c  l      localinteractiveflag5      =
c  i      localrstblcknumber1531     =
c  l      localstdystatetermflg8     =
c  l      localtransrotatflag14      =
c  l      lperr                      =
c  i      lx                         =
c  i      ncasx                      =
c  i      niscm                      =
c  l      nogof                      =
c  l      rdc                        =
c  l      rdebug                     =
c  r      rsno(7)                    =
c  r      rw(11)                     =
c  c*8    rwch2                      =
c  c*8    rwch3                      =
c  i      savprobopt611              =
c  i      savprobtype05              =
c  l      savwritecommonblck2(ncoms) =
c  l      tfail                      =
c***********************************************************************
