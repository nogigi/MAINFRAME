       01  ARIM112I.
           02  FILLER PIC X(12).
           02  MDATEL    COMP  PIC  S9(4).
           02  MDATEF    PICTURE X.
           02  FILLER REDEFINES MDATEF.
             03 MDATEA    PICTURE X.
           02  MDATEI  PIC X(10).
           02  MTERML    COMP  PIC  S9(4).
           02  MTERMF    PICTURE X.
           02  FILLER REDEFINES MTERMF.
             03 MTERMA    PICTURE X.
           02  MTERMI  PIC X(4).
           02  MTASKL    COMP  PIC  S9(4).
           02  MTASKF    PICTURE X.
           02  FILLER REDEFINES MTASKF.
             03 MTASKA    PICTURE X.
           02  MTASKI  PIC X(7).
           02  MTRANL    COMP  PIC  S9(4).
           02  MTRANF    PICTURE X.
           02  FILLER REDEFINES MTRANF.
             03 MTRANA    PICTURE X.
           02  MTRANI  PIC X(4).
           02  MCODEL    COMP  PIC  S9(4).
           02  MCODEF    PICTURE X.
           02  FILLER REDEFINES MCODEF.
             03 MCODEA    PICTURE X.
           02  MCODEI  PIC X(5).
           02  MLIBELL    COMP  PIC  S9(4).
           02  MLIBELF    PICTURE X.
           02  FILLER REDEFINES MLIBELF.
             03 MLIBELA    PICTURE X.
           02  MLIBELI  PIC X(20).
           02  MCATEGL    COMP  PIC  S9(4).
           02  MCATEGF    PICTURE X.
           02  FILLER REDEFINES MCATEGF.
             03 MCATEGA    PICTURE X.
           02  MCATEGI  PIC X(5).
           02  MFOURL    COMP  PIC  S9(4).
           02  MFOURF    PICTURE X.
           02  FILLER REDEFINES MFOURF.
             03 MFOURA    PICTURE X.
           02  MFOURI  PIC X(5).
           02  MAPPROL    COMP  PIC  S9(4).
           02  MAPPROF    PICTURE X.
           02  FILLER REDEFINES MAPPROF.
             03 MAPPROA    PICTURE X.
           02  MAPPROI  PIC X(2).
           02  MNLOTL    COMP  PIC  S9(4).
           02  MNLOTF    PICTURE X.
           02  FILLER REDEFINES MNLOTF.
             03 MNLOTA    PICTURE X.
           02  MNLOTI  PIC X(1).
           02  MQTSTKL    COMP  PIC  S9(4).
           02  MQTSTKF    PICTURE X.
           02  FILLER REDEFINES MQTSTKF.
             03 MQTSTKA    PICTURE X.
           02  MQTSTKI  PIC X(6).
           02  MQTALEL    COMP  PIC  S9(4).
           02  MQTALEF    PICTURE X.
           02  FILLER REDEFINES MQTALEF.
             03 MQTALEA    PICTURE X.
           02  MQTALEI  PIC X(5).
           02  MLOTD OCCURS 5 TIMES.
             03  MLOTL    COMP  PIC  S9(4).
             03  MLOTF    PICTURE X.
             03  MLOTI  PIC X(79).
           02  MMSGL    COMP  PIC  S9(4).
           02  MMSGF    PICTURE X.
           02  FILLER REDEFINES MMSGF.
             03 MMSGA    PICTURE X.
           02  MMSGI  PIC X(79).
       01  ARIM112O REDEFINES ARIM112I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  MDATEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  MTERMO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  MTASKO  PIC X(7).
           02  FILLER PICTURE X(3).
           02  MTRANO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  MCODEO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  MLIBELO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  MCATEGO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  MFOURO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  MAPPROO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  MNLOTO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  MQTSTKO PIC ZZZZZ9 blank when zero.
           02  FILLER PICTURE X(3).
           02  MQTALEO PIC ZZZZ9 blank when zero.
           02  DFHMS1 OCCURS 5 TIMES.
             03  FILLER PICTURE X(2).
             03  MLOTA    PICTURE X.
             03  MLOTO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  MMSGO  PIC X(79).
       01  ARIMHP2I REDEFINES ARIM112I.
           02  FILLER PIC X(12).
       01  ARIMHP2O REDEFINES ARIMHP2I.
           02  FILLER PIC X(12).
