# Note



# Process
- init protected mode(LABEL_BEGIN)
    - get memory size
    - init descriptor
    - set gdtr and idtr
    - test GDT\LDT\ Call Gate with TSS(LABEL_SEG_CODE32)
        - set segment register
        - load ldt and tr 
        - using retf transfer to ring3 code(LABEL_CODE_RING3)
            - print '3' at row 12 column 4 
            - using call gate transfer to ring0 code (LABEL_SEG_CODE_DEST)
              - print '3' at row 12 column 1
              - jmp to local code (LABEL_CODE_A)
                - print 'L' at row 12 column 0
                - jmp to 16 bit code(LABEL_SEG_CODE16)
                  - go back to real mode