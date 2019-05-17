# Note



# Process
- init protected mode(LABEL_BEGIN)
    - get memory size
    - init descriptor
    - set gdtr and idtr
    - test GDT and Call Gate LDT(LABEL_SEG_CODE32)
        - set segment register 
        - test Call Gate(call SelectorCallGateTest:0 ->LABEL_SEG_CODE_DEST)
            - print 'C' at row 12 column 1 
            - retf to ring3 test code (LABEL_CODE_RING3)
                -  print '3' at row 12 column 3
