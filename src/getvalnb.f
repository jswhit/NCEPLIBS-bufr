C> @file
C> @brief Read one data value from a data subset.

C> This function can be used to read a data value corresponding to
C> a specific occurrence of a mnemonic within a data subset, based on
C> its position relative to a different mnemonic within the subset.
C>
C> <p>The function first searches for a specific occurrence of a pivot
C> mnemonic, counting from the beginning of the subset.  From there,
C> it then searches in either a forward or backward direction for a
C> specific occurrence of a nearby mnemonic, and if found
C> returns the data value from the corresponding location
C> within the subset.
C>
C> @author J. Ator
C> @date 2012-09-12
C>
C> @param[in] LUNIT  -- integer: Fortran logical unit number for
C>                      BUFR file
C> @param[in] TAGPV  -- character*(*): Pivot mnemonic; the subroutine
C>                      will first search for the (NTAGPV)th occurrence
C>                      of this mnemonic, counting from the beginning
C>                      of the overall subset definition
C> @param[in] NTAGPV -- integer: Ordinal occurrence of TAGPV to search for,
C>                      counting from the beginning of the overall
C>                      subset definition
C> @param[in] TAGNB  -- character*(*): Nearby mnemonic; assuming TAGPV is
C>                      successfully found, the subroutine will then search
C>                      nearby for the (NTAGNB)th occurrence of TAGNB and
C>                      return the corresponding value
C> @param[in] NTAGNB -- integer: Ordinal occurrence of TAGNB to search for,
C>                      counting from the location of TAGPV within the
C>                      overall subset definition.  If NTAGNB is positive,
C>                      the subroutine will search in a forward direction
C>                      from the location of TAGPV; otherwise, if NTAGNB is
C>                      negative, it will instead search in a backwards
C>                      direction from the location of TAGPV.
C> @returns getvalnb -- real*8: Value corresponding to (NTAGNB)th occurrence
C>                      of TAGNB.  If for any reason this value cannot be
C>                      located, then the current placeholder value for
C>                      "missing" data will be returned instead.
C>
C> <p>The current placeholder value for "missing" data can be determined
C> via a separate call to function getbmiss().
C>
C> <p>Before calling this function, a BUFR data subset should already be
C> open for reading via a previous call to one of the BUFRLIB
C> [subset-reading subroutines](@ref hierarchy).
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2012-09-12 | J. Ator | Original author |
C> | 2014-10-02 | J. Ator | Modified to use fstag() |
C> | 2014-12-10 | J. Ator | Use modules instead of COMMON blocks |
C>
	REAL*8 FUNCTION GETVALNB ( LUNIT, TAGPV, NTAGPV, TAGNB, NTAGNB )

        USE MODV_BMISS

	USE MODA_USRINT
	USE MODA_MSGCWD
	USE MODA_TABLES

	CHARACTER*(*) TAGPV, TAGNB

C----------------------------------------------------------------------
C----------------------------------------------------------------------

	GETVALNB = BMISS

C	Get LUN from LUNIT.

	CALL STATUS (LUNIT, LUN, IL, IM )
	IF ( IL .GE. 0 ) RETURN
	IF ( INODE(LUN) .NE. INV(1,LUN) ) RETURN

C	Starting from the beginning of the subset, locate the (NTAGPV)th
C	occurrence of TAGPV.

	CALL FSTAG( LUN, TAGPV, NTAGPV, 1, NPV, IRET )
	IF ( IRET .NE. 0 ) RETURN

C	Now, starting from the (NTAGPV)th occurrence of TAGPV, search
C	forward or backward for the (NTAGNB)th occurrence of TAGNB.

	CALL FSTAG( LUN, TAGNB, NTAGNB, NPV, NNB, IRET )
	IF ( IRET .NE. 0 ) RETURN

	GETVALNB = VAL(NNB,LUN)
	    
	RETURN
	END
