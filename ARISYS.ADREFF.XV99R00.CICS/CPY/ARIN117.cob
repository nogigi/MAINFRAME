       01  ARIM117I.
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
           02  MMSGL    COMP  PIC  S9(4).
           02  MMSGF    PICTURE X.
           02  FILLER REDEFINES MMSGF.
             03 MMSGA    PICTURE X.
           02  MMSGI  PIC X(79).
       01  ARIM117O REDEFINES ARIM117I.
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
           02  MMSGO  PIC X(79).
       01  ARIMHP7I REDEFINES ARIM117I.
           02  FILLER PIC X(12).
       01  ARIMHP7O REDEFINES ARIMHP7I.
           02  FILLER PIC X(12).
