C> @file
C> @brief Dynamically allocate Fortran language arrays within internal
C> memory.
	
C> This subroutine is called internally during the first call to
C> subroutine openbf() from an application program, in order to
C> dynamically allocate internal Fortran language arrays based on
C> parameter values set during one or more previous calls to function
C> isetprm().
C>
C> <p>This subroutine isn't normally called directly from an application
C> program, since it's automatically called internally during the first
C> call to subroutine openbf() from an application program.
C>
C> @author J. Ator
C> @date 2014-12-04
C>
C> @remarks
C> - All memory allocated within this subroutine can be freed via a
C> subsequent call to subroutine exitbufr() from within the
C> application program, or else it will be freed automatically by the
C> operating system once the application program terminates.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2014-12-04 | J. Ator | Original author |
C> | 2016-05-24 | J. Ator | Added allocations for MODA_BITMAPS and MODA_NRV203 |
C> | 2017-05-22 | J. Ator | Added allocations for MODA_RLCCMN |
C> | 2019-05-09 | J. Ator | Modified allocations for MODA_BUFRMG |
C> | 2021-01-08 | J. Ator | Modified mstabs array declarations for GNUv10 portability |
C> | 2021-05-17 | J. Ator | Allow up to 24 characters in cbunit |
C>
	SUBROUTINE ARALLOCF

	USE MODA_USRINT
	USE MODA_USRBIT
	USE MODA_IVAL
	USE MODA_MSGCWD
	USE MODA_STBFR
	USE MODA_UFBCPL
	USE MODA_SC3BFR
	USE MODA_UNPTYP
	USE MODA_LUSHR
	USE MODA_NULBFR
	USE MODA_STCODE
	USE MODA_IDRDM
	USE MODA_XTAB
	USE MODA_MSGLIM
	USE MODA_BITBUF
	USE MODA_MGWA
	USE MODA_MGWB
	USE MODA_BUFRMG
	USE MODA_BUFRSR
	USE MODA_MSGMEM
	USE MODA_TABABD
	USE MODA_TABLES
	USE MODA_USRTMP
	USE MODA_IVTTMP
	USE MODA_COMPRX
	USE MODA_COMPRS
	USE MODA_MSTABS
	USE MODA_RDMTB
	USE MODA_NMIKRP
	USE MODA_S01CM
	USE MODA_BITMAPS
	USE MODA_NRV203
	USE MODA_RLCCMN

	CHARACTER*80 ERRSTR
	CHARACTER*36  BRTSTR

	COMMON /QUIET/ IPRT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	IF ( IPRT .GE. 1 ) THEN
	    CALL ERRWRT
     .		('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
	    ERRSTR = 'BUFRLIB: ARRAYS WILL BE DYNAMICALLY ALLOCATED' //
     .		' USING THE FOLLOWING VALUES:'
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I7)' ) '    MAXSS = ', MAXSS
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I4)' ) '   NFILES = ', NFILES
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I7)' ) '   MXMSGL = ', MXMSGL
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I5)' ) '   MXDXTS = ', MXDXTS
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I7)' ) '   MAXMSG = ', MAXMSG
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I9)' ) '   MAXMEM = ', MAXMEM
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I5)' ) '   MAXTBA = ', MAXTBA
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I5)' ) '   MAXTBB = ', MAXTBB
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I5)' ) '   MAXTBD = ', MAXTBD
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I7)' ) '    MAXJL = ', MAXJL
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I6)' ) '    MXCDV = ', MXCDV
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I4)' ) '    MXLCC = ', MXLCC
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I6)' ) '    MXCSB = ', MXCSB
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I5)' ) '   MXMTBB = ', MXMTBB
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I5)' ) '   MXMTBD = ', MXMTBD
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I4)' ) '    MAXCD = ', MAXCD
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I4)' ) '    MXNRV = ', MXNRV
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I4)' ) '   MXS01V = ', MXS01V
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I4)' ) '   MXTAMC = ', MXTAMC
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I4)' ) '    MXTCO = ', MXTCO
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I4)' ) '    MXBTM = ', MXBTM
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I4)' ) '  MXBTMSE = ', MXBTMSE
	    CALL ERRWRT(ERRSTR)
	    WRITE ( ERRSTR, '(A,I4)' ) '    MXRST = ', MXRST
	    CALL ERRWRT(ERRSTR)
	    CALL ERRWRT
     .		('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
	END IF

	BRTSTR = 'BUFRLIB: ARALLOCF FAILED ALLOCATING '

C	MODA_USRINT arrays.

	ALLOCATE( NVAL(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NVAL' )

	ALLOCATE( INV(MAXSS,NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'INV' )

	ALLOCATE( NRFELM(MAXSS,NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NRFELM' )

	ALLOCATE( VAL(MAXSS,NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'VAL' )

C	MODA_USRBIT arrays.

	ALLOCATE( NBIT(MAXSS), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NBIT' )

	ALLOCATE( MBIT(MAXSS), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MBIT' )

C	MODA_IVAL arrays.

	ALLOCATE( IVAL(MAXSS), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IVAL' )

C	MODA_MSGCWD arrays.

	ALLOCATE( NMSG(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NMSG' )

	ALLOCATE( NSUB(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NSUB' )

	ALLOCATE( MSUB(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MSUB' )

	ALLOCATE( INODE(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'INODE' )

	ALLOCATE( IDATE(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IDATE' )

C	MODA_STBFR arrays.

	ALLOCATE( IOLUN(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IOLUN' )

	ALLOCATE( IOMSG(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IOMSG' )

C	MODA_UFBCPL arrays.

	ALLOCATE( LUNCPY(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'LUNCPY' )

C	MODA_SC3BFR arrays.

	ALLOCATE( ISC3(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'ISC3' )

	ALLOCATE( TAMNEM(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'TAMNEM' )

C	MODA_UNPTYP arrays.

	ALLOCATE( MSGUNP(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MSGUNP' )

C	MODA_LUSHR arrays.

	ALLOCATE( LUS(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'LUS' )

C	MODA_NULBFR arrays.

	ALLOCATE( NULL(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NULL' )

C	MODA_STCODE arrays.

	ALLOCATE( ISCODES(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'ISCODES' )

C	MODA_IDRDM arrays.

	ALLOCATE( IDRDM(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IDRDM' )

C	MODA_XTAB arrays.

	ALLOCATE( XTAB(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'XTAB' )

C	MODA_MSGLIM arrays.

	ALLOCATE( MSGLIM(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MSGLIM' )

C	Calculate MXMSGLD4 from MXMSGL.

	IF ( MOD(MXMSGL,4) .eq. 0 ) THEN
	    MXMSGLD4 = MXMSGL/4
	ELSE
	    MXMSGLD4 = MXMSGL/4 + 1
	END IF

C	MODA_BITBUF arrays.

	ALLOCATE( IBAY(MXMSGLD4), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IBAY' )

	ALLOCATE( MBYT(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MBYT' )

	ALLOCATE( MBAY(MXMSGLD4,NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MBAY' )

C	MODA_MGWA arrays.

	ALLOCATE( MGWA(MXMSGLD4), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MGWA' )

C	MODA_MGWB arrays.

	ALLOCATE( MGWB(MXMSGLD4), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MGWB' )

C	MODA_BUFRMG arrays.

	ALLOCATE( MSGLEN(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MSGLEN' )
	ALLOCATE( MSGTXT(MXMSGLD4,NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MSGTXT' )

C	MODA_BUFRSR arrays.

	ALLOCATE( JSR(NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'JSR' )

	ALLOCATE( JBAY(MXMSGLD4), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'JBAY' )

C	Calculate MXDXM and MXDXW from MXDXTS and MXMSGLD4.

        MXDXM = MXDXTS*3
        MXDXW = MXDXM*MXMSGLD4

C	MODA_MSGMEM arrays.

	ALLOCATE( MSGP(0:MAXMSG), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MSGP' )

	ALLOCATE( MSGS(MAXMEM), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MSGS' )

	ALLOCATE( MDX(MXDXW), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MDX' )

	ALLOCATE( IPDXM(MXDXM), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IPDXM' )

	ALLOCATE( IFDXTS(MXDXTS), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IFDXTS' )

	ALLOCATE( ICDXTS(MXDXTS), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'ICDXTS' )

	ALLOCATE( IPMSGS(MXDXTS), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IPMSGS' )

C	MODA_TABABD arrays.

	ALLOCATE( NTBA(0:NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NTBA' )

	ALLOCATE( NTBB(0:NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NTBB' )

	ALLOCATE( NTBD(0:NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NTBD' )

	ALLOCATE( MTAB(MAXTBA,NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MTAB' )

	ALLOCATE( IDNA(MAXTBA,NFILES,2), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IDNA' )

	ALLOCATE( IDNB(MAXTBB,NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IDNB' )

	ALLOCATE( IDND(MAXTBD,NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IDND' )

	ALLOCATE( TABA(MAXTBA,NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'TABA' )

	ALLOCATE( TABB(MAXTBB,NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'TABB' )

	ALLOCATE( TABD(MAXTBD,NFILES), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'TABD' )

C	MODA_TABLES arrays.

	ALLOCATE( TAG(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'TAG' )

	ALLOCATE( TYP(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'TYP' )

	ALLOCATE( KNT(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'KNT' )

	ALLOCATE( JUMP(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'JUMP' )

	ALLOCATE( LINK(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'LINK' )

	ALLOCATE( JMPB(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'JMPB' )

	ALLOCATE( IBT(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IBT' )

	ALLOCATE( IRF(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IRF' )

	ALLOCATE( ISC(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'ISC' )

	ALLOCATE( ITP(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'ITP' )

	ALLOCATE( VALI(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'VALI' )

	ALLOCATE( KNTI(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'KNTI' )

	ALLOCATE( ISEQ(MAXJL,2), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'ISEQ' )

	ALLOCATE( JSEQ(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'JSEQ' )

C	MODA_USRTMP arrays.

	ALLOCATE( IUTMP(MAXJL,MAXRCR), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IUTMP' )

	ALLOCATE( VUTMP(MAXJL,MAXRCR), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'VUTMP' )

C	MODA_IVTTMP arrays.

	ALLOCATE( TTMP(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'TTMP' )

	ALLOCATE( ITMP(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'ITMP' )

	ALLOCATE( VTMP(MAXJL), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'VTMP' )

C	MODA_COMPRX arrays.

	ALLOCATE( KMIN(MXCDV), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'KMIN' )

	ALLOCATE( KMAX(MXCDV), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'KMAX' )

	ALLOCATE( KMIS(MXCDV), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'KMIS' )

	ALLOCATE( KBIT(MXCDV), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'KBIT' )

	ALLOCATE( ITYP(MXCDV), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'ITYP' )

	ALLOCATE( IWID(MXCDV), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IWID' )

	ALLOCATE( CHARACTER*(MXLCC) :: CSTR(MXCDV), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CSTR' )

C	MODA_COMPRS arrays.

	ALLOCATE( MATX(MXCDV,MXCSB), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'MATX' )

	ALLOCATE( CHARACTER*(MXLCC) :: CATX(MXCDV,MXCSB), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CATX' )

C	MODA_MSTABS arrays.

	ALLOCATE( IBFXYN(MXMTBB), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IBFXYN' )

	ALLOCATE( CBSCL(4,MXMTBB), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CBSCL' )

	ALLOCATE( CBSREF(12,MXMTBB), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CBSREF' )

	ALLOCATE( CBBW(4,MXMTBB), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CBBW' )

	ALLOCATE( CBUNIT(24,MXMTBB), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CBUNIT' )

	ALLOCATE( CBMNEM(8,MXMTBB), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CBMNEM' )

	ALLOCATE( CBELEM(120,MXMTBB), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CBELEM' )

	ALLOCATE( IDFXYN(MXMTBD), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IDFXYN' )

	ALLOCATE( CDSEQ(120,MXMTBD), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CDSEQ' )

	ALLOCATE( CDMNEM(8,MXMTBD), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CDMNEM' )

	ALLOCATE( NDELEM(MXMTBD), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NDELEM' )

	ALLOCATE( IDEFXY(MXMTBD*MAXCD), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IDEFXY' )

C	MODA_RDMTB arrays.

	ALLOCATE( IEFXYN(MXMTBD,MAXCD), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IEFXYN' )

	ALLOCATE( CMDSCB(MXMTBB), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CMDSCB' )

	ALLOCATE( CMDSCD(MXMTBD), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CMDSCD' )

	ALLOCATE( CEELEM(MXMTBD,MAXCD), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CEELEM' )

C	MODA_NMIKRP arrays.

	ALLOCATE( NEM(MAXCD,10), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NEM' )

	ALLOCATE( IRP(MAXCD,10), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IRP' )

	ALLOCATE( KRP(MAXCD,10), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'KRP' )

C	MODA_S01CM arrays.

	ALLOCATE( IVMNEM(MXS01V), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IVMNEM' )

	ALLOCATE( CMNEM(MXS01V), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CMNEM' )

C	MODA_BITMAPS arrays.

	ALLOCATE( INODTAMC(MXTAMC), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'INODTAMC' )

	ALLOCATE( NTCO(MXTAMC), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NTCO' )

	ALLOCATE( CTCO(MXTAMC,MXTCO), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CTCO' )

	ALLOCATE( INODTCO(MXTAMC,MXTCO), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'INODTCO' )

	ALLOCATE( NBTMSE(MXBTM), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NBTMSE' )

	ALLOCATE( ISTBTM(MXBTM), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'ISTBTM' )

	ALLOCATE( ISZBTM(MXBTM), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'ISZBTM' )

	ALLOCATE( IBTMSE(MXBTM,MXBTMSE), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IBTMSE' )

C	MODA_NRV203 arrays.

	ALLOCATE( TAGNRV(MXNRV), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'TAGNRV' )

	ALLOCATE( INODNRV(MXNRV), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'INODNRV' )

	ALLOCATE( NRV(MXNRV), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'NRV' )

	ALLOCATE( ISNRV(MXNRV), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'ISNRV' )

	ALLOCATE( IENRV(MXNRV), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IENRV' )

C	MODA_RLCCMN arrays.

	ALLOCATE( IRNCH(MXRST), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IRNCH' )

	ALLOCATE( IRBIT(MXRST), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'IRBIT' )

	ALLOCATE( CRTAG(MXRST), STAT=iost )
	IF ( iost .ne. 0 ) CALL BORT( BRTSTR // 'CRTAG' )

	RETURN
	END
