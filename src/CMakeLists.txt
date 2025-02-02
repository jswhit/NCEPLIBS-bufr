set(CMAKE_INCLUDE_CURRENT_DIR ON)

list(APPEND fortran_src
  modv_BMISS.f90 modv_IFOPBF.f90 modv_MAXCD.f90 modv_MAXJL.f modv_MAXMEM.f modv_MAXMSG.f modv_MAXNC.f90
  modv_MAXSS.f modv_MAXTBA.f modv_MAXTBB.f modv_MAXTBD.f modv_MXBTM.f modv_MXBTMSE.f modv_MXCDV.f
  modv_MXCNEM.f90 modv_MXCSB.f modv_MXDXTS.f modv_MXH4WLC.f90 modv_MXLCC.f modv_MXMSGL.f modv_MXMTBB.f
  modv_MXMTBD.f modv_MXMTBF.f modv_MXNAF.f90 modv_MXNRV.f modv_MXRST.f modv_MXS.f90 modv_MXS01V.f
  modv_MXTAMC.f modv_MXTCO.f modv_NFILES.f90
  moda_bitbuf.F moda_bitmaps.F moda_bufrmg.F moda_bufrsr.F moda_comprs.F moda_comprx.F moda_dscach.F
  moda_h4wlc.F moda_idrdm.F moda_ival.F moda_ivttmp.F moda_lushr.F moda_mgwa.F moda_mgwb.F moda_msgcwd.F
  moda_msglim.F moda_msgmem.F moda_mstabs.F moda_nmikrp.F moda_nrv203.F moda_nulbfr.F moda_rdmtb.F
  moda_rlccmn.F moda_s01cm.F moda_sc3bfr.F moda_stbfr.F moda_stcode.F moda_tababd.F moda_tables.F
  moda_ufbcpl.F moda_unptyp.F moda_usrbit.F moda_usrint.F moda_usrtmp.F moda_xtab.F
  adn30.f atrcpt.f blocks.f bort.f bort2.f cadn30.f capit.f chekstab.f chrtrna.f cktaba.f closmg.f
  cmpmsg.f cmsgini.f cnved4.f codflg.f conwin.f copybf.f copymg.f copysb.f cpbfdx.f cpdxmm.f cpymem.f
  cpyupd.f datebf.f datelen.f digit.f drfini.f drstpl.f dumpbf.f dxdump.f dxinit.f dxmini.f elemdx.f
  errwrt.f exitbufr.f fstag.f getabdb.f getbmiss.f getcfmng.f getlens.f getntbe.f gets1loc.f gettagpr.f
  gettagre.f gettbh.f getvalnb.f getwin.f hold4wlc.f i4dy.f ibfms.f icbfms.f ichkstr.f icmpdx.f icopysb.f
  idn30.f idxmsg.f ifbget.f ifxy.f igetdate.f igetfxy.f igetmxby.f igetntbi.f igetntbl.f igetprm.f
  igetrfel.f igetsc.f igettdi.f imrkopr.f inctab.f invcon.f invmrg.f invtag.f invwin.f iok2cpy.f
  iokoper.f ipkm.f ipks.f ireadmg.f ireadmm.f ireadns.f ireadsb.f ishrdx.f isize.f istdesc.f iupb.f
  iupbs01.f iupbs3.f iupm.f iupvs01.f jstnum.f lcmgdf.f lmsg.f lstjpb.f makestab.f maxout.f
  mesgbc.f mesgbf.f minimg.f mrginv.f msgfull.f msgini.f msgupd.f msgwrt.f mtfnam.f mtinfo.f mvb.f
  nemdefs.f nemock.f nemspecs.f nemtab.f nemtba.f nemtbax.f nemtbb.f nemtbd.f nenubd.f nevn.f newwin.f
  nmsub.f nmwrd.f numbck.f numtab.f numtbd.f nvnwin.f nwords.f nxtwin.f openbt.f openmb.f openmg.f pad.f
  padmsg.f parstr.f parusr.f parutg.f pkb.f pkbs1.f pkc.f pkftbv.f pktdd.f posapx.f rcstpl.f rdbfdx.f
  rdcmps.f rdmemm.f rdmems.f rdmgsb.f rdmsgw.f rdmtbb.f rdmtbd.f rdmtbf.f rdtree.f rdusdx.f readdx.f
  readerme.f readlc.f readmg.f readmm.f readns.f reads3.f readsb.f rewnbf.f rsvfvm.f rtrcpt.f
  rtrcptb.f seqsdx.f setblock.f setbmiss.f setvalnb.f sntbbe.f sntbde.f sntbfe.f status.f stbfdx.f
  stdmsg.f stndrd.f stntbi.f stntbia.f strbtm.f strcln.f strcpt.f string.f strnum.f strsuc.f tabent.f
  tabsub.f trybump.f ufbcnt.f ufbcpy.f ufbcup.f ufbdmp.f ufbevn.f ufbget.f ufbin3.f ufbint.f ufbinx.f
  ufbmem.f ufbmex.f ufbmms.f ufbmns.f ufbovr.f ufbpos.f ufbqcd.f ufbqcp.f ufbrep.f ufbrms.f ufbrp.f
  ufbrw.f ufbseq.f ufbsp.f ufbstp.f ufbtab.f ufbtam.f ufdump.f upb.f upbb.f upc.f upds3.f upftbv.f
  ups.f uptdd.f usrtpl.f valx.f wrcmps.f wrdxtb.f writcp.f writdx.f writlc.f writsa.f writsb.f
  wrtree.f wtstat.f arallocf.f ardllocf.f closbf.f ireadmt.f irev.F isetprm.f openbf.f pkvs01.f
  wrdlen.F fortran_open.f90 fortran_close.f90 bufr_interface.f90)

list(APPEND c_src
  arallocc.c ardllocc.c bort_exit.c ccbfl.c cmpia.c cmpstia1.c cmpstia2.c cobfl.c cpmstabs.c
  crbmg.c cread.c cwbmg.c dlloctbf.c icvidx.c inittbf.c nummtb.c rbytes.c restd.c sorttbf.c
  srchtbf.c strtbfe.c stseq.c wrdesc.c)

list(APPEND c_hdr
  cfe.h cobfl.h cread.h mstabs.h bufr_interface.h)

# Create the bvers.f file and bfrini.f90 files
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/bvers.f.in ${CMAKE_CURRENT_BINARY_DIR}/bvers.f @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/bfrini.f90.in ${CMAKE_CURRENT_BINARY_DIR}/bfrini.f90 @ONLY)
list(APPEND fortran_src ${CMAKE_CURRENT_BINARY_DIR}/bvers.f ${CMAKE_CURRENT_BINARY_DIR}/bfrini.f90)

# Create the bufrlib.h file
foreach(_var IN ITEMS MAXNC MXNAF MAXCD NFILES)
  file(STRINGS modv_${_var}.f90 _${_var}_tempstr REGEX "integer,")
  if(_${_var}_tempstr MATCHES "${_var} = ([0-9]+)")
    set(${_var} ${CMAKE_MATCH_1})
  else()
    message(FATAL_ERROR "Unable to parse variable ${_var} value from file: modv_${_var}.f90")
  endif()
endforeach()
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/bufrlib.h.in ${CMAKE_CURRENT_BINARY_DIR}/bufrlib.h @ONLY)
list(APPEND c_hdr ${CMAKE_CURRENT_BINARY_DIR}/bufrlib.h)

include(TestBigEndian)
test_big_endian(IS_BIG_ENDIAN)
if(IS_BIG_ENDIAN)
  list(APPEND endian_def BIG_ENDIAN)
else()
  list(APPEND endian_def LITTLE_ENDIAN)
endif()

foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/bufr_${kind}")

  add_library(${lib_name}_f OBJECT ${fortran_src})
  set_target_properties(${lib_name}_f PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
  target_compile_definitions(${lib_name}_f PRIVATE "${endian_def}")

  add_library(${lib_name}_c OBJECT ${c_src})
  set_target_properties(${lib_name}_c PROPERTIES COMPILE_FLAGS
                                                 "${c_${kind}_flags}")
  target_compile_definitions(${lib_name}_c PUBLIC "${underscore_def}")
  target_compile_definitions(${lib_name}_c PRIVATE "${c_${kind}_defs}")

  set_target_properties(${lib_name}_f PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")

  add_library(${lib_name} $<TARGET_OBJECTS:${lib_name}_f>
                          $<TARGET_OBJECTS:${lib_name}_c>)
  add_library(${PROJECT_NAME}::${lib_name} ALIAS ${lib_name})

  target_include_directories(${lib_name} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

  target_include_directories(${lib_name} INTERFACE
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:include/bufr_${kind}>)

  target_compile_definitions(${lib_name} PUBLIC "${underscore_def}")

  list(APPEND LIB_TARGETS ${lib_name})
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
  install(FILES ${c_hdr} DESTINATION ${CMAKE_INSTALL_PREFIX}/include/bufr_${kind})
endforeach()

install(
  TARGETS ${LIB_TARGETS}
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

export(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake)

configure_package_config_file(
  ${PROJECT_SOURCE_DIR}/cmake/PackageConfig.cmake.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

write_basic_package_version_file(
  ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

install(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})
