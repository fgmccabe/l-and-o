'#pkg'("n7o7'()7'n2o2'pkg's'lo.io'e'*'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'private'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'private'n2o2'pkg's'lo.list'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'s\"I40'openInFile'FT2St'lo.io*ioEncoding't'lo.io*inChannel''openOutFile'FT2St'lo.io*ioEncoding't'lo.io*outChannel''rawEncoding't'lo.io*ioEncoding''utf8Encoding't'lo.io*ioEncoding''unknownEncoding't'lo.io*ioEncoding''cwd'FT0S'cd'PT1S'ls'FT1SLS'fileType'FT1St'lo.io*fileType''setUid't'lo.io*filePerm''setGid't'lo.io*filePerm''stIcky't'lo.io*filePerm''rUsr't'lo.io*filePerm''wUsr't'lo.io*filePerm''xUsr't'lo.io*filePerm''rGrp't'lo.io*filePerm''wGrp't'lo.io*filePerm''xGrp't'lo.io*filePerm''rOth't'lo.io*filePerm''wOth't'lo.io*filePerm''xOth't'lo.io*filePerm''fmodes'FT1SLt'lo.io*filePerm''fileList'FT1SLT3St'lo.io*fileType'Lt'lo.io*filePerm''fifoSpecial't'lo.io*fileType''directory't'lo.io*fileType''charSpecial't'lo.io*fileType''blockSpecial't'lo.io*fileType''plainFile't'lo.io*fileType''symlink't'lo.io*fileType''socket't'lo.io*fileType''chmode'PT2SLt'lo.io*filePerm''rm'PT1S'mv'PT2SS'filePresent'PT1S'isDir'PT1S'fileSize'FT1Si'newerFile'PT2SS'getFile'FT1SS'putFile'PT2SS'logMsg'PT1S\"s\"I5'ioEncoding'Yt'lo.io*ioEncoding'I0'inChannel'Yt'lo.io*inChannel'I12'ready'PT0'close'PT0'eof'PT0'seek'PT1i'pos'FT0i'inText'FT1SS'inLine'FT0S'inChars'FT1iS'inCh'FT0i'inB'FT0i'inBytes'FT1iLi'name'S'outChannel'Yt'lo.io*outChannel'I9'ready'PT0'close'PT0'flush'PT0'outLine'PT1S'outStr'PT1S'outBytes'PT1Li'outB'PT1i'outCh'PT1i'name'S'fileType'Yt'lo.io*fileType'I0'filePerm'Yt'lo.io*filePerm'I0\"n22o22'()22's'rawEncoding's'utf8Encoding's'unknownEncoding's'setUid's'setGid's'stIcky's'rUsr's'wUsr's'xUsr's'rGrp's'wGrp's'xGrp's'rOth's'wOth's'xOth's'fifoSpecial's'directory's'charSpecial's'blockSpecial's'plainFile's'symlink's'socket'n0o0'()0'n0o0'()0'").
'lo.io@init'() :- !.
'lo.io#inChannel'('inChannel%1'('lo.io@inChannel'())) :- !.
'lo.io#inChannel'('name%1'(XV1), XLbl1, XThis1) :- !,
    'lo.io#inChannel@name'(XV1, XLbl1, XThis1).
'lo.io#inChannel'('inBytes%2'(XV4, XV5), XLbl2, XThis2) :- !,
    'lo.io#inChannel@inBytes'(XV4, XV5, XLbl2, XThis2).
'lo.io#inChannel'('inBytes%1'('lo.io#inChannel^inBytes'(XLbl3, XThis3)), XLbl3, XThis3).
'lo.io#inChannel'('inB%1'(XV9), XLbl4, XThis4) :- !,
    'lo.io#inChannel@inB'(XV9, XLbl4, XThis4).
'lo.io#inChannel'('inB%1'('lo.io#inChannel^inB'(XLbl5, XThis5)), XLbl5, XThis5).
'lo.io#inChannel'('inCh%1'(XV12), XLbl6, XThis6) :- !,
    'lo.io#inChannel@inCh'(XV12, XLbl6, XThis6).
'lo.io#inChannel'('inCh%1'('lo.io#inChannel^inCh'(XLbl7, XThis7)), XLbl7, XThis7).
'lo.io#inChannel'('inChars%2'(XV16, XV17), XLbl8, XThis8) :- !,
    'lo.io#inChannel@inChars'(XV16, XV17, XLbl8, XThis8).
'lo.io#inChannel'('inChars%1'('lo.io#inChannel^inChars'(XLbl9, XThis9)), XLbl9, XThis9).
'lo.io#inChannel'('inLine%1'(XV21), XLbl10, XThis10) :- !,
    'lo.io#inChannel@inLine'(XV21, XLbl10, XThis10).
'lo.io#inChannel'('inLine%1'('lo.io#inChannel^inLine'(XLbl11, XThis11)), XLbl11, XThis11).
'lo.io#inChannel'('inText%2'(XV25, XV26), XLbl12, XThis12) :- !,
    'lo.io#inChannel@inText'(XV25, XV26, XLbl12, XThis12).
'lo.io#inChannel'('inText%1'('lo.io#inChannel^inText'(XLbl13, XThis13)), XLbl13, XThis13).
'lo.io#inChannel'('pos%1'(XV30), XLbl14, XThis14) :- !,
    'lo.io#inChannel@pos'(XV30, XLbl14, XThis14).
'lo.io#inChannel'('pos%1'('lo.io#inChannel^pos'(XLbl15, XThis15)), XLbl15, XThis15).
'lo.io#inChannel'('seek%1'(XV33), XLbl16, XThis16) :- !,
    'lo.io#inChannel@seek'(XV33, XLbl16, XThis16).
'lo.io#inChannel'('seek%1'('lo.io#inChannel^seek'(XLbl17, XThis17)), XLbl17, XThis17).
'lo.io#inChannel'('eof%0'(), XLbl18, XThis18) :- !,
    'lo.io#inChannel@eof'(XLbl18, XThis18).
'lo.io#inChannel'('eof%1'('lo.io#inChannel^eof'(XLbl19, XThis19)), XLbl19, XThis19).
'lo.io#inChannel'('close%0'(), XLbl20, XThis20) :- !,
    'lo.io#inChannel@close'(XLbl20, XThis20).
'lo.io#inChannel'('close%1'('lo.io#inChannel^close'(XLbl21, XThis21)), XLbl21, XThis21).
'lo.io#inChannel'('ready%0'(), XLbl22, XThis22) :- !,
    'lo.io#inChannel@ready'(XLbl22, XThis22).
'lo.io#inChannel'('ready%1'('lo.io#inChannel^ready'(XLbl23, XThis23)), XLbl23, XThis23).
'lo.io#inChannel@name'(XN, XLbV1, XThV1) :- XLbV1 = 'lo.io#inChannel'(XN, XH),
    !.
'lo.io#inChannel@inBytes'(XCx, XX8, XLbV1, XThV1) :- XLbV1 = 'lo.io#inChannel'(XN, XH),
    !,
    '_inbytes'(XH, XCx, XX8).
'lo.io#inChannel@inBytes'(_, _, _, _) :- raise_exception('error'("inBytes", 49, 5, 29)).
'lo.io#inChannel@inB'(XX10, XLbV1, XThV1) :- XLbV1 = 'lo.io#inChannel'(XN, XH),
    !,
    '_inbyte'(XH, XX10).
'lo.io#inChannel@inB'(_, _, _) :- raise_exception('error'("inB", 50, 5, 19)).
'lo.io#inChannel@inCh'(XX12, XLbV1, XThV1) :- XLbV1 = 'lo.io#inChannel'(XN, XH),
    !,
    '_inchar'(XH, XX12).
'lo.io#inChannel@inCh'(_, _, _) :- raise_exception('error'("inCh", 51, 5, 20)).
'lo.io#inChannel@inChars'(XCx, XX16, XLbV1, XThV1) :- XLbV1 = 'lo.io#inChannel'(XN, XH),
    !,
    '_inchars'(XH, XCx, XX16).
'lo.io#inChannel@inChars'(_, _, _, _) :- raise_exception('error'("inChars", 52, 5, 29)).
'lo.io#inChannel@inLine'(XX18, XLbV1, XThV1) :- XLbV1 = 'lo.io#inChannel'(XN, XH),
    !,
    '_inline'(XH, XX18).
'lo.io#inChannel@inLine'(_, _, _) :- raise_exception('error'("inLine", 53, 5, 22)).
'lo.io#inChannel@inText'(Xterm, XX22, XLbV1, XThV1) :- XLbV1 = 'lo.io#inChannel'(XN, XH),
    !,
    '_intext'(XH, Xterm, XX22).
'lo.io#inChannel@inText'(_, _, _, _) :- raise_exception('error'("inText", 54, 5, 31)).
'lo.io#inChannel@pos'(XX24, XLbV1, XThV1) :- XLbV1 = 'lo.io#inChannel'(XN, XH),
    !,
    '_fposition'(XH, XX24).
'lo.io#inChannel@pos'(_, _, _) :- raise_exception('error'("pos", 55, 5, 22)).
'lo.io#inChannel@seek'(XPx, XLbV1, XThV1) :- XLbV1 = 'lo.io#inChannel'(XN, XH),
    '_fseek'(XH, XPx).
'lo.io#inChannel@eof'(XLbV1, XThV1) :- XLbV1 = 'lo.io#inChannel'(XN, XH),
    '_end_of_file'(XH).
'lo.io#inChannel@close'(XLbV1, XThV1) :- XLbV1 = 'lo.io#inChannel'(XN, XH),
    '_close'(XH).
'lo.io#inChannel@ready'(XLbV1, XThV1) :- XLbV1 = 'lo.io#inChannel'(XN, XH),
    '_ready'(XH).
'lo.io@openInFile'(XFn, XEnc, 'lo.io#inChannel'(XFn, XX38)) :- !,
    ocall('_coerce%2'(XEnc, XX36),'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer','lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer'),
    '_openInFile'(XFn, XX36, XX38).
'lo.io@openInFile'(_, _, _) :- raise_exception('error'("openInFile", 24, 3, 64)).
'lo.io#outChannel'('outChannel%1'('lo.io@outChannel'())) :- !.
'lo.io#outChannel'('name%1'(XV38), XLbl24, XThis24) :- !,
    'lo.io#outChannel@name'(XV38, XLbl24, XThis24).
'lo.io#outChannel'('outCh%1'(XV40), XLbl25, XThis25) :- !,
    'lo.io#outChannel@outCh'(XV40, XLbl25, XThis25).
'lo.io#outChannel'('outCh%1'('lo.io#outChannel^outCh'(XLbl26, XThis26)), XLbl26, XThis26).
'lo.io#outChannel'('outB%1'(XV43), XLbl27, XThis27) :- !,
    'lo.io#outChannel@outB'(XV43, XLbl27, XThis27).
'lo.io#outChannel'('outB%1'('lo.io#outChannel^outB'(XLbl28, XThis28)), XLbl28, XThis28).
'lo.io#outChannel'('outBytes%1'(XV46), XLbl29, XThis29) :- !,
    'lo.io#outChannel@outBytes'(XV46, XLbl29, XThis29).
'lo.io#outChannel'('outBytes%1'('lo.io#outChannel^outBytes'(XLbl30, XThis30)), XLbl30, XThis30).
'lo.io#outChannel'('outStr%1'(XV49), XLbl31, XThis31) :- !,
    'lo.io#outChannel@outStr'(XV49, XLbl31, XThis31).
'lo.io#outChannel'('outStr%1'('lo.io#outChannel^outStr'(XLbl32, XThis32)), XLbl32, XThis32).
'lo.io#outChannel'('outLine%1'(XV52), XLbl33, XThis33) :- !,
    'lo.io#outChannel@outLine'(XV52, XLbl33, XThis33).
'lo.io#outChannel'('outLine%1'('lo.io#outChannel^outLine'(XLbl34, XThis34)), XLbl34, XThis34).
'lo.io#outChannel'('flush%0'(), XLbl35, XThis35) :- !,
    'lo.io#outChannel@flush'(XLbl35, XThis35).
'lo.io#outChannel'('flush%1'('lo.io#outChannel^flush'(XLbl36, XThis36)), XLbl36, XThis36).
'lo.io#outChannel'('close%0'(), XLbl37, XThis37) :- !,
    'lo.io#outChannel@close'(XLbl37, XThis37).
'lo.io#outChannel'('close%1'('lo.io#outChannel^close'(XLbl38, XThis38)), XLbl38, XThis38).
'lo.io#outChannel'('ready%0'(), XLbl39, XThis39) :- !,
    'lo.io#outChannel@ready'(XLbl39, XThis39).
'lo.io#outChannel'('ready%1'('lo.io#outChannel^ready'(XLbl40, XThis40)), XLbl40, XThis40).
'lo.io#outChannel@name'(XN, XLbV2, XThV2) :- XLbV2 = 'lo.io#outChannel'(XN, XH),
    !.
'lo.io#outChannel@outCh'(XCh, XLbV2, XThV2) :- XLbV2 = 'lo.io#outChannel'(XN, XH),
    '_outch'(XH, XCh).
'lo.io#outChannel@outB'(XB, XLbV2, XThV2) :- XLbV2 = 'lo.io#outChannel'(XN, XH),
    '_outbyte'(XH, XB).
'lo.io#outChannel@outBytes'(XL, XLbV2, XThV2) :- XLbV2 = 'lo.io#outChannel'(XN, XH),
    '_outbytes'(XH, XL).
'lo.io#outChannel@outStr'(XS, XLbV2, XThV2) :- XLbV2 = 'lo.io#outChannel'(XN, XH),
    '_outtext'(XH, XS).
'lo.io#outChannel@outLine'(XS, XLbV2, XThV2) :- XLbV2 = 'lo.io#outChannel'(XN, XH),
    '_outtext'(XH, XS),
    '_outtext'(XH, "
").
'lo.io#outChannel@flush'(XLbV2, XThV2) :- XLbV2 = 'lo.io#outChannel'(XN, XH),
    '_flush'(XH).
'lo.io#outChannel@close'(XLbV2, XThV2) :- XLbV2 = 'lo.io#outChannel'(XN, XH),
    '_close'(XH).
'lo.io#outChannel@ready'(XLbV2, XThV2) :- XLbV2 = 'lo.io#outChannel'(XN, XH),
    '_ready'(XH).
'lo.io@openOutFile'(XFn, XEnc, 'lo.io#outChannel'(XFn, XX70)) :- !,
    ocall('_coerce%2'(XEnc, XX68),'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer','lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer'),
    '_openOutFile'(XFn, XX68, XX70).
'lo.io@openOutFile'(_, _, _) :- raise_exception('error'("openOutFile", 27, 3, 67)).
'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer'('lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer%1'('lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer')) :- !.
'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer'('_coerce%2'(XV59, XV60), XLbl41, XThis41) :- !,
    'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer@_coerce'(XV59, XV60, XLbl41, XThis41).
'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer'('_coerce%1'('lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer^_coerce'(XLbl42, XThis42)), XLbl42, XThis42).
'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer@_coerce'('lo.io#rawEncoding', 0, XLbV6, XThV6) :- !.
'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer@_coerce'('lo.io#utf8Encoding', 3, XLbV6, XThV6) :- !.
'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer@_coerce'('lo.io#unknownEncoding', 2, XLbV6, XThV6) :- !.
'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 93, 5, 25)).
'lo.io@openAppendFile'(Xfle, Xencoding, 'lo.io#outChannel'(Xfle, XX82)) :- !,
    ocall('_coerce%2'(Xencoding, XX80),'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer','lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer'),
    '_openAppendFile'(Xfle, XX80, XX82).
'lo.io@openAppendFile'(_, _, _) :- raise_exception('error'("openAppendFile", 100, 3, 86)).
'lo.io@pipeConnect'(XCmd, XArgs, XEnv, Xencoding, ('lo.io#outChannel'(XX95, XinF), 'lo.io#inChannel'(XX100, XoutF), 'lo.io#inChannel'(XX105, XerrF))) :- '_popen'(XCmd, XArgs, XEnv, XinF, XoutF, XerrF),
    !,
    ocall('+%3'(XCmd, ":stdin", XX95),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%3'(XCmd, ":stdout", XX100),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%3'(XCmd, ":stderr", XX105),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string').
'lo.io@pipeConnect'(_, _, _, _, _) :- raise_exception('error'("pipeConnect", 114, 3, 180)).
'lo.io@cwd'(XX109) :- !,
    '_cwd'(XX109).
'lo.io@cwd'(_) :- raise_exception('error'("cwd", 120, 3, 15)).
'lo.io@cd'(XD) :- '_cd'(XD).
'lo.io@ls'(XD, XX114) :- !,
    '_ls'(XD, XX114).
'lo.io@ls'(_, _) :- raise_exception('error'("ls", 128, 3, 15)).
'lo.io@fileType'(XF, XX118) :- !,
    '_file_type'(XF, XX117),
    ocall('_coerce%2'(XX117, XX118),'lo.coerce$coercion$lo.core*integer$lo.io*fileType','lo.coerce$coercion$lo.core*integer$lo.io*fileType').
'lo.io@fileType'(_, _) :- raise_exception('error'("fileType", 138, 3, 38)).
'lo.io@fileMode'(0, 'lo.core#[]') :- !.
'lo.io@fileMode'(XX, 'lo.core#,..'('lo.io#setUid', XX128)) :- 'lo.bits@.&.'(XX, 2048, XX123),
    ocall('==%2'(XX123, 2048),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer'),
    !,
    'lo.bits@.&.'(XX, 2047, XX127),
    'lo.io@fileMode'(XX127, XX128).
'lo.io@fileMode'(XX, 'lo.core#,..'('lo.io#setGid', XX137)) :- 'lo.bits@.&.'(XX, 1024, XX132),
    ocall('==%2'(XX132, 1024),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer'),
    !,
    'lo.bits@.&.'(XX, 3071, XX136),
    'lo.io@fileMode'(XX136, XX137).
'lo.io@fileMode'(XX, 'lo.core#,..'('lo.io#stIcky', XX146)) :- 'lo.bits@.&.'(XX, 512, XX141),
    ocall('==%2'(XX141, 512),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer'),
    !,
    'lo.bits@.&.'(XX, 3583, XX145),
    'lo.io@fileMode'(XX145, XX146).
'lo.io@fileMode'(XX, 'lo.core#,..'('lo.io#rUsr', XX155)) :- 'lo.bits@.&.'(XX, 256, XX150),
    ocall('==%2'(XX150, 256),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer'),
    !,
    'lo.bits@.&.'(XX, 3839, XX154),
    'lo.io@fileMode'(XX154, XX155).
'lo.io@fileMode'(XX, 'lo.core#,..'('lo.io#wUsr', XX164)) :- 'lo.bits@.&.'(XX, 128, XX159),
    ocall('==%2'(XX159, 128),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer'),
    !,
    'lo.bits@.&.'(XX, 3967, XX163),
    'lo.io@fileMode'(XX163, XX164).
'lo.io@fileMode'(XX, 'lo.core#,..'('lo.io#xUsr', XX173)) :- 'lo.bits@.&.'(XX, 64, XX168),
    ocall('==%2'(XX168, 64),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer'),
    !,
    'lo.bits@.&.'(XX, 4031, XX172),
    'lo.io@fileMode'(XX172, XX173).
'lo.io@fileMode'(XX, 'lo.core#,..'('lo.io#rGrp', XX182)) :- 'lo.bits@.&.'(XX, 32, XX177),
    ocall('==%2'(XX177, 32),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer'),
    !,
    'lo.bits@.&.'(XX, 4063, XX181),
    'lo.io@fileMode'(XX181, XX182).
'lo.io@fileMode'(XX, 'lo.core#,..'('lo.io#wGrp', XX191)) :- 'lo.bits@.&.'(XX, 16, XX186),
    ocall('==%2'(XX186, 16),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer'),
    !,
    'lo.bits@.&.'(XX, 4079, XX190),
    'lo.io@fileMode'(XX190, XX191).
'lo.io@fileMode'(XX, 'lo.core#,..'('lo.io#xGrp', XX200)) :- 'lo.bits@.&.'(XX, 8, XX195),
    ocall('==%2'(XX195, 8),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer'),
    !,
    'lo.bits@.&.'(XX, 4087, XX199),
    'lo.io@fileMode'(XX199, XX200).
'lo.io@fileMode'(XX, 'lo.core#,..'('lo.io#rOth', XX209)) :- 'lo.bits@.&.'(XX, 4, XX204),
    ocall('==%2'(XX204, 4),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer'),
    !,
    'lo.bits@.&.'(XX, 4091, XX208),
    'lo.io@fileMode'(XX208, XX209).
'lo.io@fileMode'(XX, 'lo.core#,..'('lo.io#wOth', XX218)) :- 'lo.bits@.&.'(XX, 2, XX213),
    ocall('==%2'(XX213, 2),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer'),
    !,
    'lo.bits@.&.'(XX, 4093, XX217),
    'lo.io@fileMode'(XX217, XX218).
'lo.io@fileMode'(XX, 'lo.core#,..'('lo.io#xOth', XX227)) :- 'lo.bits@.&.'(XX, 1, XX222),
    ocall('==%2'(XX222, 1),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer'),
    !,
    'lo.bits@.&.'(XX, 4094, XX226),
    'lo.io@fileMode'(XX226, XX227).
'lo.io@fileMode'(_, _) :- raise_exception('error'("fileMode", 157, 3, 17)).
'lo.io@fmodes'(XF, XX232) :- !,
    '_file_mode'(XF, XX231),
    'lo.io@fileMode'(XX231, XX232).
'lo.io@fmodes'(_, _) :- raise_exception('error'("fmodes", 154, 3, 36)).
'lo.io@fileList'(XD, XX242) :- !,
    '_ls'(XD, XX235),
    ocall('//%3'(XX235, 'lo.io@$1', XX242),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list').
'lo.io@fileList'(_, _) :- raise_exception('error'("fileList", 131, 3, 55)).
'lo.coerce$coercion$lo.core*integer$lo.io*fileType'('lo.coerce$coercion$lo.core*integer$lo.io*fileType%1'('lo.coerce$coercion$lo.core*integer$lo.io*fileType')) :- !.
'lo.coerce$coercion$lo.core*integer$lo.io*fileType'('_coerce%2'(XV85, XV86), XLbl43, XThis43) :- !,
    'lo.coerce$coercion$lo.core*integer$lo.io*fileType@_coerce'(XV85, XV86, XLbl43, XThis43).
'lo.coerce$coercion$lo.core*integer$lo.io*fileType'('_coerce%1'('lo.coerce$coercion$lo.core*integer$lo.io*fileType^_coerce'(XLbl44, XThis44)), XLbl44, XThis44).
'lo.coerce$coercion$lo.core*integer$lo.io*fileType@_coerce'(0, 'lo.io#fifoSpecial', XLbV26, XThV26) :- !.
'lo.coerce$coercion$lo.core*integer$lo.io*fileType@_coerce'(1, 'lo.io#directory', XLbV26, XThV26) :- !.
'lo.coerce$coercion$lo.core*integer$lo.io*fileType@_coerce'(2, 'lo.io#charSpecial', XLbV26, XThV26) :- !.
'lo.coerce$coercion$lo.core*integer$lo.io*fileType@_coerce'(3, 'lo.io#blockSpecial', XLbV26, XThV26) :- !.
'lo.coerce$coercion$lo.core*integer$lo.io*fileType@_coerce'(4, 'lo.io#plainFile', XLbV26, XThV26) :- !.
'lo.coerce$coercion$lo.core*integer$lo.io*fileType@_coerce'(5, 'lo.io#symlink', XLbV26, XThV26) :- !.
'lo.coerce$coercion$lo.core*integer$lo.io*fileType@_coerce'(6, 'lo.io#socket', XLbV26, XThV26) :- !.
'lo.coerce$coercion$lo.core*integer$lo.io*fileType@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 141, 5, 25)).
'lo.io@encodeModes'('lo.core#[]', XP, XP) :- !.
'lo.io@encodeModes'('lo.core#,..'('lo.io#setUid', XL), XP, XX261) :- !,
    'lo.bits@.|.'(XP, 2048, XX260),
    'lo.io@encodeModes'(XL, XX260, XX261).
'lo.io@encodeModes'('lo.core#,..'('lo.io#setGid', XL), XP, XX269) :- !,
    'lo.bits@.|.'(XP, 1024, XX268),
    'lo.io@encodeModes'(XL, XX268, XX269).
'lo.io@encodeModes'('lo.core#,..'('lo.io#stIcky', XL), XP, XX277) :- !,
    'lo.bits@.|.'(XP, 512, XX276),
    'lo.io@encodeModes'(XL, XX276, XX277).
'lo.io@encodeModes'('lo.core#,..'('lo.io#rUsr', XL), XP, XX285) :- !,
    'lo.bits@.|.'(XP, 256, XX284),
    'lo.io@encodeModes'(XL, XX284, XX285).
'lo.io@encodeModes'('lo.core#,..'('lo.io#wUsr', XL), XP, XX293) :- !,
    'lo.bits@.|.'(XP, 128, XX292),
    'lo.io@encodeModes'(XL, XX292, XX293).
'lo.io@encodeModes'('lo.core#,..'('lo.io#xUsr', XL), XP, XX301) :- !,
    'lo.bits@.|.'(XP, 64, XX300),
    'lo.io@encodeModes'(XL, XX300, XX301).
'lo.io@encodeModes'('lo.core#,..'('lo.io#rGrp', XL), XP, XX309) :- !,
    'lo.bits@.|.'(XP, 32, XX308),
    'lo.io@encodeModes'(XL, XX308, XX309).
'lo.io@encodeModes'('lo.core#,..'('lo.io#wGrp', XL), XP, XX317) :- !,
    'lo.bits@.|.'(XP, 16, XX316),
    'lo.io@encodeModes'(XL, XX316, XX317).
'lo.io@encodeModes'('lo.core#,..'('lo.io#xGrp', XL), XP, XX325) :- !,
    'lo.bits@.|.'(XP, 8, XX324),
    'lo.io@encodeModes'(XL, XX324, XX325).
'lo.io@encodeModes'('lo.core#,..'('lo.io#rOth', XL), XP, XX333) :- !,
    'lo.bits@.|.'(XP, 4, XX332),
    'lo.io@encodeModes'(XL, XX332, XX333).
'lo.io@encodeModes'('lo.core#,..'('lo.io#wOth', XL), XP, XX341) :- !,
    'lo.bits@.|.'(XP, 2, XX340),
    'lo.io@encodeModes'(XL, XX340, XX341).
'lo.io@encodeModes'('lo.core#,..'('lo.io#xOth', XL), XP, XX349) :- !,
    'lo.bits@.|.'(XP, 1, XX348),
    'lo.io@encodeModes'(XL, XX348, XX349).
'lo.io@encodeModes'(_, _, _) :- raise_exception('error'("encodeModes", 176, 3, 20)).
'lo.io@chmode'(XF, XP) :- 'lo.io@encodeModes'(XP, 0, XX354),
    '_chmod'(XF, XX354).
'lo.io@rm'(XF) :- '_rm'(XF).
'lo.io@mv'(XO, XN) :- '_mv'(XO, XN).
'lo.io@filePresent'(XF) :- '_file_present'(XF).
'lo.io@isDir'(XD) :- '_isdir'(XD).
'lo.io@fileSize'(XF, XX367) :- !,
    '_file_size'(XF, XX367).
'lo.io@fileSize'(_, _) :- raise_exception('error'("fileSize", 207, 3, 28)).
'lo.io@newerFile'(XU1, XU2) :- '_file_modified'(XU1, XX372),
    '_file_modified'(XU2, XX374),
    'lo.core@>'('lo.core$comp$lo.core*float', XX372, XX374).
'lo.io@getFile'(XFn, XX377) :- !,
    '_get_file'(XFn, XX377).
'lo.io@getFile'(_, _) :- raise_exception('error'("getFile", 214, 3, 28)).
'lo.io@putFile'(XFn, XText) :- '_put_file'(XFn, XText).
'lo.io@logMsg'(XMsg) :- '_logmsg'(XMsg).
'lo.io#inChannel^inBytes'('_call%2'(XV2, XV3), 'lo.io#inChannel^inBytes'(XLbV1, XThV1), _) :- 'lo.io#inChannel@inBytes'(XV2, XV3, XLbV1, XThV1).
'lo.io#inChannel^inBytes'('_call%2'(XV6, XV7), 'lo.io#inChannel^inBytes'(XLbV1, XThV1), _) :- 'lo.io#inChannel@inBytes'(XV6, XV7, XLbV1, XThV1).
'lo.io#inChannel^inB'('_call%1'(XV8), 'lo.io#inChannel^inB'(XLbV1, XThV1), _) :- 'lo.io#inChannel@inB'(XV8, XLbV1, XThV1).
'lo.io#inChannel^inB'('_call%1'(XV10), 'lo.io#inChannel^inB'(XLbV1, XThV1), _) :- 'lo.io#inChannel@inB'(XV10, XLbV1, XThV1).
'lo.io#inChannel^inCh'('_call%1'(XV11), 'lo.io#inChannel^inCh'(XLbV1, XThV1), _) :- 'lo.io#inChannel@inCh'(XV11, XLbV1, XThV1).
'lo.io#inChannel^inCh'('_call%1'(XV13), 'lo.io#inChannel^inCh'(XLbV1, XThV1), _) :- 'lo.io#inChannel@inCh'(XV13, XLbV1, XThV1).
'lo.io#inChannel^inChars'('_call%2'(XV14, XV15), 'lo.io#inChannel^inChars'(XLbV1, XThV1), _) :- 'lo.io#inChannel@inChars'(XV14, XV15, XLbV1, XThV1).
'lo.io#inChannel^inChars'('_call%2'(XV18, XV19), 'lo.io#inChannel^inChars'(XLbV1, XThV1), _) :- 'lo.io#inChannel@inChars'(XV18, XV19, XLbV1, XThV1).
'lo.io#inChannel^inLine'('_call%1'(XV20), 'lo.io#inChannel^inLine'(XLbV1, XThV1), _) :- 'lo.io#inChannel@inLine'(XV20, XLbV1, XThV1).
'lo.io#inChannel^inLine'('_call%1'(XV22), 'lo.io#inChannel^inLine'(XLbV1, XThV1), _) :- 'lo.io#inChannel@inLine'(XV22, XLbV1, XThV1).
'lo.io#inChannel^inText'('_call%2'(XV23, XV24), 'lo.io#inChannel^inText'(XLbV1, XThV1), _) :- 'lo.io#inChannel@inText'(XV23, XV24, XLbV1, XThV1).
'lo.io#inChannel^inText'('_call%2'(XV27, XV28), 'lo.io#inChannel^inText'(XLbV1, XThV1), _) :- 'lo.io#inChannel@inText'(XV27, XV28, XLbV1, XThV1).
'lo.io#inChannel^pos'('_call%1'(XV29), 'lo.io#inChannel^pos'(XLbV1, XThV1), _) :- 'lo.io#inChannel@pos'(XV29, XLbV1, XThV1).
'lo.io#inChannel^pos'('_call%1'(XV31), 'lo.io#inChannel^pos'(XLbV1, XThV1), _) :- 'lo.io#inChannel@pos'(XV31, XLbV1, XThV1).
'lo.io#inChannel^seek'('_call%1'(XV32), 'lo.io#inChannel^seek'(XLbV1, XThV1), _) :- 'lo.io#inChannel@seek'(XV32, XLbV1, XThV1).
'lo.io#inChannel^seek'('_call%1'(XV34), 'lo.io#inChannel^seek'(XLbV1, XThV1), _) :- 'lo.io#inChannel@seek'(XV34, XLbV1, XThV1).
'lo.io#inChannel^eof'('_call%0'(), 'lo.io#inChannel^eof'(XLbV1, XThV1), _) :- 'lo.io#inChannel@eof'(XLbV1, XThV1).
'lo.io#inChannel^eof'('_call%0'(), 'lo.io#inChannel^eof'(XLbV1, XThV1), _) :- 'lo.io#inChannel@eof'(XLbV1, XThV1).
'lo.io#inChannel^close'('_call%0'(), 'lo.io#inChannel^close'(XLbV1, XThV1), _) :- 'lo.io#inChannel@close'(XLbV1, XThV1).
'lo.io#inChannel^close'('_call%0'(), 'lo.io#inChannel^close'(XLbV1, XThV1), _) :- 'lo.io#inChannel@close'(XLbV1, XThV1).
'lo.io#inChannel^ready'('_call%0'(), 'lo.io#inChannel^ready'(XLbV1, XThV1), _) :- 'lo.io#inChannel@ready'(XLbV1, XThV1).
'lo.io#inChannel^ready'('_call%0'(), 'lo.io#inChannel^ready'(XLbV1, XThV1), _) :- 'lo.io#inChannel@ready'(XLbV1, XThV1).
'lo.io^openInFile'('_call%3'(XV35, XV36, XV37), 'lo.io^openInFile', _) :- 'lo.io@openInFile'(XV35, XV36, XV37).
'lo.io#outChannel^outCh'('_call%1'(XV39), 'lo.io#outChannel^outCh'(XLbV2, XThV2), _) :- 'lo.io#outChannel@outCh'(XV39, XLbV2, XThV2).
'lo.io#outChannel^outCh'('_call%1'(XV41), 'lo.io#outChannel^outCh'(XLbV2, XThV2), _) :- 'lo.io#outChannel@outCh'(XV41, XLbV2, XThV2).
'lo.io#outChannel^outB'('_call%1'(XV42), 'lo.io#outChannel^outB'(XLbV2, XThV2), _) :- 'lo.io#outChannel@outB'(XV42, XLbV2, XThV2).
'lo.io#outChannel^outB'('_call%1'(XV44), 'lo.io#outChannel^outB'(XLbV2, XThV2), _) :- 'lo.io#outChannel@outB'(XV44, XLbV2, XThV2).
'lo.io#outChannel^outBytes'('_call%1'(XV45), 'lo.io#outChannel^outBytes'(XLbV2, XThV2), _) :- 'lo.io#outChannel@outBytes'(XV45, XLbV2, XThV2).
'lo.io#outChannel^outBytes'('_call%1'(XV47), 'lo.io#outChannel^outBytes'(XLbV2, XThV2), _) :- 'lo.io#outChannel@outBytes'(XV47, XLbV2, XThV2).
'lo.io#outChannel^outStr'('_call%1'(XV48), 'lo.io#outChannel^outStr'(XLbV2, XThV2), _) :- 'lo.io#outChannel@outStr'(XV48, XLbV2, XThV2).
'lo.io#outChannel^outStr'('_call%1'(XV50), 'lo.io#outChannel^outStr'(XLbV2, XThV2), _) :- 'lo.io#outChannel@outStr'(XV50, XLbV2, XThV2).
'lo.io#outChannel^outLine'('_call%1'(XV51), 'lo.io#outChannel^outLine'(XLbV2, XThV2), _) :- 'lo.io#outChannel@outLine'(XV51, XLbV2, XThV2).
'lo.io#outChannel^outLine'('_call%1'(XV53), 'lo.io#outChannel^outLine'(XLbV2, XThV2), _) :- 'lo.io#outChannel@outLine'(XV53, XLbV2, XThV2).
'lo.io#outChannel^flush'('_call%0'(), 'lo.io#outChannel^flush'(XLbV2, XThV2), _) :- 'lo.io#outChannel@flush'(XLbV2, XThV2).
'lo.io#outChannel^flush'('_call%0'(), 'lo.io#outChannel^flush'(XLbV2, XThV2), _) :- 'lo.io#outChannel@flush'(XLbV2, XThV2).
'lo.io#outChannel^close'('_call%0'(), 'lo.io#outChannel^close'(XLbV2, XThV2), _) :- 'lo.io#outChannel@close'(XLbV2, XThV2).
'lo.io#outChannel^close'('_call%0'(), 'lo.io#outChannel^close'(XLbV2, XThV2), _) :- 'lo.io#outChannel@close'(XLbV2, XThV2).
'lo.io#outChannel^ready'('_call%0'(), 'lo.io#outChannel^ready'(XLbV2, XThV2), _) :- 'lo.io#outChannel@ready'(XLbV2, XThV2).
'lo.io#outChannel^ready'('_call%0'(), 'lo.io#outChannel^ready'(XLbV2, XThV2), _) :- 'lo.io#outChannel@ready'(XLbV2, XThV2).
'lo.io^openOutFile'('_call%3'(XV54, XV55, XV56), 'lo.io^openOutFile', _) :- 'lo.io@openOutFile'(XV54, XV55, XV56).
'lo.io@rawEncoding'('lo.io#rawEncoding') :- !.
'lo.io@utf8Encoding'('lo.io#utf8Encoding') :- !.
'lo.io@unknownEncoding'('lo.io#unknownEncoding') :- !.
'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer^_coerce'('_call%2'(XV57, XV58), 'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer^_coerce'(XLbV6, XThV6), _) :- 'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer@_coerce'(XV57, XV58, XLbV6, XThV6).
'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer^_coerce'('_call%2'(XV61, XV62), 'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer^_coerce'(XLbV6, XThV6), _) :- 'lo.coerce$coercion$lo.io*ioEncoding$lo.core*integer@_coerce'(XV61, XV62, XLbV6, XThV6).
'lo.io^openAppendFile'('_call%3'(XV63, XV64, XV65), 'lo.io^openAppendFile', _) :- 'lo.io@openAppendFile'(XV63, XV64, XV65).
'lo.io^pipeConnect'('_call%5'(XV66, XV67, XV68, XV69, XV70), 'lo.io^pipeConnect', _) :- 'lo.io@pipeConnect'(XV66, XV67, XV68, XV69, XV70).
'lo.io^cwd'('_call%1'(XV71), 'lo.io^cwd', _) :- 'lo.io@cwd'(XV71).
'lo.io^cd'('_call%1'(XV72), 'lo.io^cd', _) :- 'lo.io@cd'(XV72).
'lo.io^ls'('_call%2'(XV73, XV74), 'lo.io^ls', _) :- 'lo.io@ls'(XV73, XV74).
'lo.io^fileType'('_call%2'(XV75, XV76), 'lo.io^fileType', _) :- 'lo.io@fileType'(XV75, XV76).
'lo.io@setUid'('lo.io#setUid') :- !.
'lo.io@setGid'('lo.io#setGid') :- !.
'lo.io@stIcky'('lo.io#stIcky') :- !.
'lo.io@rUsr'('lo.io#rUsr') :- !.
'lo.io@wUsr'('lo.io#wUsr') :- !.
'lo.io@xUsr'('lo.io#xUsr') :- !.
'lo.io@rGrp'('lo.io#rGrp') :- !.
'lo.io@wGrp'('lo.io#wGrp') :- !.
'lo.io@xGrp'('lo.io#xGrp') :- !.
'lo.io@rOth'('lo.io#rOth') :- !.
'lo.io@wOth'('lo.io#wOth') :- !.
'lo.io@xOth'('lo.io#xOth') :- !.
'lo.io^fileMode'('_call%2'(XV77, XV78), 'lo.io^fileMode', _) :- 'lo.io@fileMode'(XV77, XV78).
'lo.io^fmodes'('_call%2'(XV79, XV80), 'lo.io^fmodes', _) :- 'lo.io@fmodes'(XV79, XV80).
'lo.io@$1'('_call%2'(XF, (XF, XX239, XX241)), 'lo.io@$1', _) :- !,
    'lo.io@fileType'(XF, XX239),
    'lo.io@fmodes'(XF, XX241).
'lo.io@$1'(_, _, _) :- raise_exception('error'("lambda", 131, 27, 30)).
'lo.io^fileList'('_call%2'(XV81, XV82), 'lo.io^fileList', _) :- 'lo.io@fileList'(XV81, XV82).
'lo.io@fifoSpecial'('lo.io#fifoSpecial') :- !.
'lo.io@directory'('lo.io#directory') :- !.
'lo.io@charSpecial'('lo.io#charSpecial') :- !.
'lo.io@blockSpecial'('lo.io#blockSpecial') :- !.
'lo.io@plainFile'('lo.io#plainFile') :- !.
'lo.io@symlink'('lo.io#symlink') :- !.
'lo.io@socket'('lo.io#socket') :- !.
'lo.coerce$coercion$lo.core*integer$lo.io*fileType^_coerce'('_call%2'(XV83, XV84), 'lo.coerce$coercion$lo.core*integer$lo.io*fileType^_coerce'(XLbV26, XThV26), _) :- 'lo.coerce$coercion$lo.core*integer$lo.io*fileType@_coerce'(XV83, XV84, XLbV26, XThV26).
'lo.coerce$coercion$lo.core*integer$lo.io*fileType^_coerce'('_call%2'(XV87, XV88), 'lo.coerce$coercion$lo.core*integer$lo.io*fileType^_coerce'(XLbV26, XThV26), _) :- 'lo.coerce$coercion$lo.core*integer$lo.io*fileType@_coerce'(XV87, XV88, XLbV26, XThV26).
'lo.io^encodeModes'('_call%3'(XV89, XV90, XV91), 'lo.io^encodeModes', _) :- 'lo.io@encodeModes'(XV89, XV90, XV91).
'lo.io^chmode'('_call%2'(XV92, XV93), 'lo.io^chmode', _) :- 'lo.io@chmode'(XV92, XV93).
'lo.io^rm'('_call%1'(XV94), 'lo.io^rm', _) :- 'lo.io@rm'(XV94).
'lo.io^mv'('_call%2'(XV95, XV96), 'lo.io^mv', _) :- 'lo.io@mv'(XV95, XV96).
'lo.io^filePresent'('_call%1'(XV97), 'lo.io^filePresent', _) :- 'lo.io@filePresent'(XV97).
'lo.io^isDir'('_call%1'(XV98), 'lo.io^isDir', _) :- 'lo.io@isDir'(XV98).
'lo.io^fileSize'('_call%2'(XV99, XV100), 'lo.io^fileSize', _) :- 'lo.io@fileSize'(XV99, XV100).
'lo.io^newerFile'('_call%2'(XV101, XV102), 'lo.io^newerFile', _) :- 'lo.io@newerFile'(XV101, XV102).
'lo.io^getFile'('_call%2'(XV103, XV104), 'lo.io^getFile', _) :- 'lo.io@getFile'(XV103, XV104).
'lo.io^putFile'('_call%2'(XV105, XV106), 'lo.io^putFile', _) :- 'lo.io@putFile'(XV105, XV106).
'lo.io^logMsg'('_call%1'(XV107), 'lo.io^logMsg', _) :- 'lo.io@logMsg'(XV107).
