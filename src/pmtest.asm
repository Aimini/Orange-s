;================================
;pmtest1.asm
;保护模式
;================================


%include "pm.inc"   ;常量,宏和一些说明

;当输入一个外部命令或通过EXEC子功能（系统功能调用INT21h,子功能号为4Bh）加载一个程序时
;COMMAND确定当前可用内存的最低端作为程序段的起点,也就是程序被加载到内存空间的起点;
;在程序所占用内存的前256（0100h）个字节中,DOS会为程序创建前缀（PSP）数据区。加载程序后 即 cs:ip = psp:0100h
org     0100h
	jmp     LABEL_BEGIN

[SECTION .gdt]
PageDirBase0		equ	200000h	; 页目录开始地址:	2M
PageTblBase0		equ	201000h	; 页表开始地址:		2M +  4K
PageDirBase1		equ	210000h	; 页目录开始地址:	2M + 64K
PageTblBase1		equ	211000h	; 页表开始地址:		2M + 64K + 4K


LinearAddrDemo	equ	00401000h
ProcFoo		equ	00401000h
ProcBar		equ	00501000h
ProcPagingDemo	equ	00301000h

;GDT 段表
;                                段基址,            段界限,         属性;
;空描述符                              ,                 ,             ;
LABEL_GDT:          Descriptor       0,                0,            0;
;Normal描述符
LABEL_DESC_NORMAL:  Descriptor       0,           0ffffh,       DA_DRW;
;32位代码段描述符 非一致代码段          ,                 ,             ;
LABEL_DESC_CODE32:  Descriptor       0, SegCode32Len - 1, DA_CR + DA_32;
;16位代码段描述符 非一致代码段                                          ;
LABEL_DESC_CODE16:  Descriptor       0,           0ffffh, DA_C        ;
; 32 bit code segment RING3
LABEL_DESC_CODE_RING3: Descriptor    0,   CodeRing3Len-1,  DA_CR + DA_32 + DA_DPL3; 
;
LABEL_DESC_DATA:    Descriptor       0,      DataLen - 1, DA_DRW        ;
;                                     ,                 ,             ;
LABEL_DESC_STACK:   Descriptor       0,       TopOfStack, DA_DRWA + DA_32;
;
LABEL_DESC_STACK_RING3:   Descriptor 0,      TopOfStack3, DA_DRWA + DA_32 + DA_DPL3;
LABEL_DESC_TEST:    Descriptor 0500000h,          0ffffh, DA_DRW       ;
;显存描述符 显存首址 , set it to DPL3 for test                    ,                ,            ;
LABEL_DESC_VIDEO:   Descriptor  0B8000h,          0ffffh, DA_DRW + DA_DPL3     ;
;LDT 描述符
LABEL_DESC_LDT:     Descriptor        0,      LDTLen - 1, DA_LDT;
;调用们描述符
LABEL_DESC_CODE_DEST:Descriptor       0,SegCodeDestLen -1,DA_C | DA_32;
LABEL_DESC_FLAT_C:  Descriptor        0,         0fffffh, DA_CR  | DA_32     |DA_LIMIT_4K
LABEL_DESC_FLAT_RW: Descriptor        0,         0fffffh, DA_DRW |DA_LIMIT_4K
;                                    目标选择子，偏移，DCount,属性
LABEL_CALL_GATE_TEST:   Gate   SelectorCodeDest,   0,     0,DA_386CGate + DA_DPL3

LABEL_DESC_TSS:     Descriptor        0,      TSSLen - 1,     DA_386TSS
;GDT END

GdtLen  equ  $ - LABEL_GDT      ;GDT长度
GdtPtr	dw      GdtLen - 1      ;GDT界限
	    dd      0               ;GDT基址(此处还未设置)
;GDT 选择子
SelectorNormal          equ     LABEL_DESC_NORMAL - LABEL_GDT
SelectorCode32          equ     LABEL_DESC_CODE32 - LABEL_GDT
SelectorCode16          equ     LABEL_DESC_CODE16 - LABEL_GDT
SelectorCodeRing3       equ     LABEL_DESC_CODE_RING3 - LABEL_GDT + SA_RPL3
SelectorData            equ     LABEL_DESC_DATA   - LABEL_GDT
SelectorStack           equ     LABEL_DESC_STACK  - LABEL_GDT
SelectorStackRing3      equ     LABEL_DESC_STACK_RING3- LABEL_GDT + SA_RPL3
SelectorTest            equ     LABEL_DESC_TEST   - LABEL_GDT
SelectorVideo           equ     LABEL_DESC_VIDEO  - LABEL_GDT
SelectorLDT             equ     LABEL_DESC_LDT    - LABEL_GDT
SelectorCodeDest        equ     LABEL_DESC_CODE_DEST - LABEL_GDT
SelectorFlatC		equ	LABEL_DESC_FLAT_C	- LABEL_GDT
SelectorFlatRW		equ	LABEL_DESC_FLAT_RW	- LABEL_GDT


SelectorCallGateTest    equ LABEL_CALL_GATE_TEST - LABEL_GDT

SelectorTSS equ LABEL_DESC_TSS - LABEL_GDT
;END of section gdt
[SECTION .ldt]
ALIGN 32
LABEL_LDT:
LABEL_LDT_DESC_CODEA: Descriptor 0,CodeALen - 1,DA_C  + DA_32;
LDTLen equ $ - LABEL_LDT  
SelectorLDTCodeA  equ LABEL_LDT_DESC_CODEA - LABEL_LDT + SA_TIL
;END of [SECTION .ldt]
;-----------------------------------------------------------------------
;数据段
[SECTION .data1]
ALIGN   32
[BITS 32]
LABEL_DATA:
; 实模式下使用这些符号
; 字符串
_szPMMessage:			db	"In Protect Mode now. ^-^", 0Ah, 0Ah, 0	; 进入保护模式后显示此字符串
_szMemChkTitle:			db	"BaseAddrL BaseAddrH LengthLow LengthHigh   Type", 0Ah, 0	; 进入保护模式后显示此字符串
_szRAMSize			db	"RAM size:", 0
_szReturn			db	0Ah, 0
; 变量
_wSPValueInRealMode		dw	0
_dwMCRNumber:			dd	0	; Memory Check Result
_dwDispPos:			dd	(80 * 6 + 0) * 2	; 屏幕第 6 行, 第 0 列。
_dwMemSize:			dd	0
_ARDStruct:			; Address Range Descriptor Structure
_dwBaseAddrLow:		dd	0
_dwBaseAddrHigh:	dd	0
_dwLengthLow:		dd	0
_dwLengthHigh:		dd	0
_dwType:		    dd	0
_PageTableNumber		dd	0
_MemChkBuf:	times	256	db	0

_SavedIDTR:			dd	0	; 用于保存 IDTR
				    dd	0
_SavedIMREG:		db	0	; 中断屏蔽寄存器值

; 保护模式下使用这些符号
szPMMessage		equ	_szPMMessage	- $$
szMemChkTitle	equ	_szMemChkTitle	- $$
szRAMSize		equ	_szRAMSize	- $$
szReturn		equ	_szReturn	- $$
dwDispPos		equ	_dwDispPos	- $$
dwMemSize		equ	_dwMemSize	- $$
dwMCRNumber		equ	_dwMCRNumber	- $$
ARDStruct		equ	_ARDStruct	- $$
dwBaseAddrLow	equ	_dwBaseAddrLow	- $$
dwBaseAddrHigh	equ	_dwBaseAddrHigh	- $$
dwLengthLow	    equ	_dwLengthLow	- $$
dwLengthHigh	equ	_dwLengthHigh	- $$
dwType		    equ	_dwType		- $$
MemChkBuf		equ	_MemChkBuf	- $$
PageTableNumber		equ	_PageTableNumber- $$
SavedIDTR       equ _SavedIDTR - $$
SavedIMREG      equ _SavedIMREG - $$
DataLen			equ	$ - LABEL_DATA


[SECTION .idt]
ALIGN 32
[BITS 32]
LABEL_IDT:
    ;重复255次

    %rep    255      
                 ; 目标段选择子,           偏移; DCount,  属性
    Gate        SelectorCode32, SuperHandler,  0   , DA_386IGate
    %endrep
IdtLen      equ     $ - LABEL_IDT
IdtPtr      dw      IdtLen - 1  ;段界限
            dd      0           ;段基址
;---------stack----------------------stack-------------------stack---------------------
;------------ 栈段
[SECTION .gs]
ALIGN 32
[BITS 32]
LABEL_STACK:
    times 512 db 0
TopOfStack equ $ - LABEL_STACK - 1
;-------- ring3 栈段
[SECTION .s3]
ALIGN 32
[BITS 32]
LABEL_STACK3:
    times 512 db 0
TopOfStack3 equ $ - LABEL_STACK3 - 1
;End of stack segment


;---------tss----------------------tss-------------------tss---------------------
[SECTION .tss]
ALIGN 32
[BITS 32]
LABEL_TSS:
dd  0               ;上一任务链接
dd  TopOfStack      ;0级堆栈
dd  SelectorStack
dd  0               ;1级堆栈
dd  0
dd  0               ;2级堆栈
dd  0
dd  0               ;CR3
dd  0               ;EIP
dd  0               ;EFLAGS
dd  0               ;eax
dd  0               ;ecx
dd  0               ;edx
dd  0               ;ebx
dd  0               ;esp
dd  0               ;ebp
dd  0               ;esi
dd  0               ;edi
dd  0               ;es
dd  0               ;cs
dd  0               ;ss
dd  0               ;ds
dd  0               ;fs
dd  0               ;gs
dd  0               ;LDT Selector
dw  0               ;调试陷阱标志
dw  $ - LABEL_TSS + 2;I/O位图基址
db	0ffh	        ;I/O位图结束标志
TSSLen equ $ -LABEL_TSS

;End of TSS



[SECTION .s16]
[BITS   16]
LABEL_BEGIN:
	mov     ax, cs
	mov     ds, ax
	mov     es, ax
	mov     ss, ax ;CS = DS = ES = SS
	mov     sp, 0100h; PSP区作为栈，作者可能想破坏PSP区的信息吧

    mov [LABEL_GO_BACK_TO_REAL + 3], ax ;<---这里把cs存储到了LABEL_GO_BACK_TO_REAL: jmp指令的基址
    mov [_wSPValueInRealMode], sp


    ; 得到内存数
    mov     ebx, 0
    mov     di, _MemChkBuf ;内存范围信息将被存入 [es:di]中
.loop:
    mov     eax, 0E820H ;获取内存信息的magic number
    mov     ecx, 20
    mov     edx, 0534D4150h ;
    int     15h
    jc      LABEL_MEM_CHK_FAIL;CF = 0表示没有错误
    add     di, 20  ;每个内存范围信息大小为20字节。
    inc     dword [_dwMCRNumber]
    cmp      ebx, 0
    jne     .loop
    jmp     LABEL_MEM_CHK_OK
LABEL_MEM_CHK_FAIL:
    mov dword [_dwMCRNumber],0
LABEL_MEM_CHK_OK:
    ;初始化 16 位代码段描述符
    mov     ax, cs
    movzx   eax, ax
    shl     eax, 4 ;eax = cs * 16
    add     eax, LABEL_SEG_CODE16 ; eax = cs * 16 + offset
    mov     word [LABEL_DESC_CODE16 + 2],ax
    shr     eax, 16
	mov     byte [LABEL_DESC_CODE16 + 4], al
	mov     byte [LABEL_DESC_CODE16 + 7], ah
	;初始化32位代码段描述符 
	xor     eax, eax
	mov     ax, cs
	shl     eax, 4
	add     eax, LABEL_SEG_CODE32;将段基址复制,段界限和段属性已在17行声明
	mov     word [LABEL_DESC_CODE32 + 2], ax;就是这里,忘了写 +2 结果又浪费了半天时间,今天就到这吧 2017年3月14日 21:11:24
	shr     eax, 16
	mov     byte [LABEL_DESC_CODE32 + 4], al
	mov     byte [LABEL_DESC_CODE32 + 7], ah

    ; 初始化数据段描述符
    xor     eax, eax
    mov     ax, ds
    shl     eax, 4
    add     eax, LABEL_DATA
    mov     word [LABEL_DESC_DATA + 2], ax
    shr     eax, 16
    mov     byte [LABEL_DESC_DATA + 4], al
    mov     byte [LABEL_DESC_DATA + 7], ah
	;初始化32位Ring3代码段描述符 
	xor     eax, eax
	mov     ax, cs      
	shl     eax, 4
	add     eax, LABEL_CODE_RING3;将段基址复制,段界限和段属性已在17行声明
	mov     word [LABEL_DESC_CODE_RING3 + 2], ax;就是这里,忘了写 +2 结果又浪费了半天时间,今天就到这吧 2017年3月14日 21:11:24
	shr     eax, 16
	mov     byte [LABEL_DESC_CODE_RING3 + 4], al
	mov     byte [LABEL_DESC_CODE_RING3 + 7], ah
    ; 初始化栈描述符
    xor     eax, eax
    mov     ax, ds
    shl     eax, 4
    add     eax, LABEL_STACK
    mov     word [LABEL_DESC_STACK + 2], ax
    shr     eax, 16
    mov     byte [LABEL_DESC_STACK + 4], al
    mov     byte [LABEL_DESC_STACK + 7], ah
   ; 初始化RING3栈描述符
    xor     eax, eax
    mov     ax, ds
    shl     eax, 4
    add     eax, LABEL_STACK3
    mov     word [LABEL_DESC_STACK_RING3 + 2], ax
    shr     eax, 16
    mov     byte [LABEL_DESC_STACK_RING3 + 4], al
    mov     byte [LABEL_DESC_STACK_RING3 + 7], ah

    ; 初始化LDT描述符
    xor     eax, eax
    mov     ax, ds
    shl     eax, 4
    add     eax, LABEL_LDT
    mov     word [LABEL_DESC_LDT + 2], ax
    shr     eax, 16
    mov     byte [LABEL_DESC_LDT + 4], al
    mov     byte [LABEL_DESC_LDT + 7], ah

    ; 初始化LDT中CodeA描述符
    xor     eax, eax
    mov     ax, ds
    shl     eax, 4
    add     eax, LABEL_CODE_A
    mov     word [LABEL_LDT_DESC_CODEA + 2], ax
        shr     eax, 16
    mov     byte [LABEL_LDT_DESC_CODEA + 4], al
    mov     byte [LABEL_LDT_DESC_CODEA + 7], ah

        ; 初始化GDT中SEGCODE调用门描述符
    xor     eax, eax
    mov     ax, cs
    shl     eax, 4
    add     eax, LABEL_SEG_CODE_DEST
    mov     word [LABEL_DESC_CODE_DEST + 2], ax
    shr     eax, 16
    mov     byte [LABEL_DESC_CODE_DEST + 4], al
    mov     byte [LABEL_DESC_CODE_DEST + 7], ah
    ; 初始化TSS描述符
    xor     eax, eax
    mov     ax, cs
    shl     eax, 4
    add     eax, LABEL_TSS
    mov     word [LABEL_DESC_TSS + 2], ax
    shr     eax, 16
    mov     byte [LABEL_DESC_TSS + 4], al
    mov     byte [LABEL_DESC_TSS + 7], ah


	;为加载 GDTR 做准备
	xor     eax, eax
	mov     ax, ds
	shl     eax, 4
	add     eax, LABEL_GDT          ; eax <- gdt 基址
	mov     dword [GdtPtr + 2], eax ; [GdtPtr + 2] <- eax 这样GdtPtr处信息就完整了（为什么不直接写在GdtPtr处）

	;加载GDTR
	lgdt    [GdtPtr]

	; 保存 IDTR
	sidt	[_SavedIDTR]

	; 保存中断屏蔽寄存器(IMREG)值
	in	al, 21h
	mov	[_SavedIMREG], al

    ;加载IDT
    xor     eax, eax
    mov     ax, ds
    shl     eax, 4
    add     eax, LABEL_IDT
    mov     dword [IdtPtr +2] , eax

	; 关中断
	;cli;不需要了?
    lidt    [IdtPtr]
	;打开地址线A20
	in      al, 92h
	or      al, 00000010b
	out     92h,al

	;准备切换到保护模式
	mov     eax, cr0
	or      eax, 1
	mov     cr0, eax

	;进入保护模式
	jmp     dword SelectorCode32:0  ;执行这一句会把SelectorCode32装入cs并跳转到Code32Selector：0处
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


LABEL_REAL_ENTRY:
    mov     ax, cs
    mov     ds, ax
    mov     es, ax
    mov     ss, ax
    mov     sp, [_wSPValueInRealMode]
    
    lidt	[_SavedIDTR]	; 恢复 IDTR 的原值

	mov	al, [_SavedIMREG]	; ┓恢复中断屏蔽寄存器(IMREG)的原值
	out	21h, al			    ; ┛

    

    in      al, 92h
    and     al, 11111101b ;| 关闭 A20 地址线
    out     92h, al

    sti         ;开中断

    mov     ax, 4c00h
    int     21h     ;回到dos
;END of [SECTION .s16]
  



[SECTION .s32]; 32位代码段. 由实模式跳入.
[BITS	32]
LABEL_SEG_CODE32:
    ;各种段寄存器初始化
	mov     ax, SelectorData
    mov     ds, ax
    mov     es, ax
    mov     ax, SelectorVideo
	mov     gs, ax                  ;视屏段选择子
    mov     ax, SelectorStack
    mov     ss, ax

    mov     esp, TopOfStack
    mov     ax, SelectorLDT
    lldt    ax

    mov     ax, SelectorTSS
    ltr     ax
    push    szMemChkTitle;非cpu设定代码,放在cpu设定代码后
    call    DispStr
    add     esp, 4;因为前面push了一个szMemChkTitle所以这里要让esp往回
    call    DispMemSize
    
    call    PagingDemo

    push SelectorStackRing3 ;vb 0x58:0x13
	push TopOfStack3
	push SelectorCodeRing3
	push 0
    retf

    call SelectorCallGateTest:0
    call Init8259A


    push    szPMMessage
    call    DispStr
    add     esp, 4;




 


	call	SetRealmode8259A
    ;跳回到16位代码段，Q:为什么不从此处关保护模式后直接跳回实模式下的代码段？
    ;A:保护模式下cs中的选择子对应的描述符的属性在实模式下仍然生效，并且可能会与实模式冲突
    ;因此我们需要一个“实模式描述符”，其他段寄存器（ds,es等等）中的选择子可以通过mov指令加这个描述符
    ;的选择子从而刷新属性，cs只能通过jmp指令
	jmp     SelectorCode16:0



Init8259A:
    mov     al, 011h        ; 000|1 0001 对于PC系统高三位必须为0，ICW1的第4位必须为1 ,edge triggered 模式 , 8字节中断向量， 级联 8259，需要 ICW4
    out     020h, al        ;主 8259 ICW1
    call    io_delay

    out     0A0h, al        ;从 8259 ICW1
    call    io_delay        
    mov     al, 020h        ;0010 0|000 低三位为0表示为 80x86系统
    out     021h, al        ;主  8259 ICW2
    call    io_delay

    mov     al, 028h        ;0010 1|000 同上
    out     0A1h, al        ;从 8259 ICW2
    call    io_delay

    mov     al, 004h        ;00000100 IRQ2 上接了从片
    out     021h, al        ;主 8259 ICW3
    call    io_delay

    mov     al, 002h        ;2h表示 从片接在主片的 IRQ2上
    out     0A1h, al        ;从8259 ICW3
    call    io_delay

    mov     al, 001h        ;0-\;
                            ;0  \;
                            ;0-----未使用
                            ;0----- 1=SFNM模式 0=sequential模式
                            ;0-\;
                            ;0-----主从缓冲模式
                            ;0----- 1 = 自动EOI 0=正常EOI
                            ;1----- 1 = 8086模式 0 = MCS 80/85
                                            
    out     021h, al        ;主8259 ICW4
    call    io_delay

    out     0A1h, al         ;从 8259 ICW4
    call    io_delay

    ;写 OCW (Operation Control Word)
    mov     al, 11111110b ;1表示关中断
    ;mov    al, 11111111b ;屏蔽主8259所有中断
    out     021h, al
    call	io_delay

    mov     al, 11111111b ;屏蔽从8259所有中断
    out     0A1h, al 
    call    io_delay
    ret

io_delay:  ;io等待延时函数
    nop
    nop
    nop
    nop
    ret

SetRealmode8259A:
    mov	ax, SelectorData
    mov	fs, ax

    mov	al, 015h    ;多看模拟器提示，该模拟器不支持单片模式，把级联位设为1，然后把ICW全置0应该就好了
    out	020h, al	; 主8259, ICW1.
    call	io_delay

    mov	al, 008h	; IRQ0 对应中断向量 0x8
    out	021h, al	; 主8259, ICW2.
    call	io_delay

    mov	    al, 00h
    out	    021h, al	; 主8259, ICW3.
    call	io_delay

    mov	al, 001h
    out	021h, al	; 主8259, ICW4.
    call	io_delay

    mov	al, [fs:SavedIMREG]	; ┓恢复中断屏蔽寄存器(IMREG)的原值
    out	021h, al		; ┛
    call	io_delay

    ret
; SetRealmode8259A 

_SuperHandler:
    SuperHandler    equ  _SuperHandler - $$
    mov     ah, 0Ch
    mov     al, '1'

    mov     [gs:((80 * 0 + 75) * 2)], ax
    jmp     $;妈的智障。被自己蠢哭
    iretd
;===========================分页机制初始化===================================
SetupPaging:
    ;根据内存上限计算应该初始化多少页表
    xor     edx, edx
    mov     eax, [dwMemSize]
    mov     ebx, 0400000h    ;一个PDE指向1k个PTE,一个页PTE指向一块4k字节的内存->一个PTE的内存大小为 1k * 4k = 4M
    div     ebx             ;默认的被除数为EAX, 一般除以EBX(ECX), 结果是:商放在EAX中,余数放在EDX中
    mov     ecx, eax    ;页表数
    test	edx, edx    ;edx余数
    jz      .no_remainder;余数不为零,需要加一个页目录 (ceil(MemSize/4M))
    inc     ecx
.no_remainder:
    mov     [PageTableNumber], ecx

    mov	    ax, SelectorFlatRW
    mov     es, ax
    mov     edi, PageDirBase0
    xor     eax, eax
    mov     eax, PageTblBase0 | PG_P | PG_USU | PG_RWW
.1:
    stosd       ;mov [es:edi],eax 若设置了EFLAGS中的方向位置位(即在STOSL指令前使用STD指令)则EDI自减4,否则(使用CLD指令)EDI自增4
    add     eax, 4096;每个PDE管理1k个页表,即每个PDE指向的页表首地址相隔4B * 1k = 4K
    loop    .1

    ;在初始化所有页表
    mov     eax, [PageTableNumber]
    mov     ebx, 1024
    mul     ebx         ;计算出页表数
    mov     ecx, eax
    mov     edi, PageTblBase0
    xor     eax, eax
    mov     eax, PG_P | PG_USU | PG_RWW
.2:
    stosd
    add     eax, 4096
    loop    .2

    mov     eax,    PageDirBase0
    mov     cr3, eax
    mov     eax, cr0
   	or	    eax, 80000000h
    mov     cr0, eax
    jmp     short .3

.3:
    nop
    ret
;分页机制启动完毕------------------------------------------------------------

;测试分页机制---------------------------------------------------------------
PagingDemo:
    mov	    ax, cs;这里需要从代码段复制所以将cs赋值到ds
    mov     ds, ax
    mov     ax, SelectorFlatRW
    mov     es, ax

    push    LenFoo
    push    OffsetFoo
    push    ProcFoo
    call    MemCpy
    add     esp, 12

    push	LenBar
	push	OffsetBar
	push	ProcBar
	call	MemCpy
	add	esp, 12

    push	LenPagingDemoAll ;复制PagingDemoProc的代码到指定地址 (ProcPagingDemo)
	push	OffsetPagingDemoProc
	push	ProcPagingDemo
	call	MemCpy
	add	esp, 12

    mov	ax, SelectorData
	mov	ds, ax			; 数据段选择子
	mov	es, ax

	call	SetupPaging		; 启动分页

	call	SelectorFlatC:ProcPagingDemo
	call	PSwitch			; 切换页目录,改变地址映射关系
	call	SelectorFlatC:ProcPagingDemo
    mov     eax,PageDirBase0
    mov     cr3,eax
	ret

;切换页表-----------------------------------------------------
PSwitch:
    mov	    ax, SelectorFlatRW
    mov	    es, ax
    mov     edi, PageDirBase1
    xor     eax, eax
    mov     eax, PageTblBase1 |PG_P |PG_USU |PG_RWW
    mov     ecx, [PageTableNumber] 

.1:
    stosd   ;初始化第二个页目录
    add     eax, 4096
    loop    .1

    mov     eax,  [PageTableNumber]
    mov     ebx, 1024
    mul     ebx ;计算总共多少个页表项
    mov     ecx, eax
    mov     edi, PageTblBase1
    xor     eax, eax
    mov     eax, PG_P |PG_USU |PG_RWW
.2:
    stosd
    add     eax, 4096
    loop    .2

    
    mov     eax, LinearAddrDemo ;--                     ;                                   

    shr     eax, 22             ;   |;获得该地址所在的页目录(第n项)
    mov     ebx, 4096           ;   +
    mul     ebx                 ;__/;获得该地址所在的页表项（包括页目录，即页目录*102）
    mov     ecx, eax
    mov     eax, LinearAddrDemo ;;获取该地址所在页表的表项的地址
    shr     eax, 12 ;除以2^12，即4k，获取其所在页表项
    and     eax, 03FFh;只留下低10位，该地址在页表中的位置
    mov     ebx, 4;每个页表表项大小为4b
    mul     ebx   ;算出目标页表项相对于该表中第一项的偏移
    add     eax, ecx
    add     eax, PageTblBase1;d所得到LinearAddrDemo页表表项的地址
    mov     dword [es:eax], ProcBar | PG_P | PG_USU | PG_RWW

    mov     eax, PageDirBase1
    mov     cr3, eax
    jmp     short .3
.3:
    nop
    ret


PagingDemoProc:
    OffsetPagingDemoProc    equ  PagingDemoProc - $$
    mov     eax, LinearAddrDemo
    call    eax
    retf

LenPagingDemoAll      equ $ - PagingDemoProc

foo:
    OffsetFoo       equ  foo - $$
    mov     ah, 0Ch
    mov     al, 'F'
    mov     [gs:((80*0 + 0)*2)], ax
    mov     al, 'o'
    mov     [gs:((80*0 + 1)*2)], ax
    mov     [gs:((80*0 + 2)*2)], ax
    ret
    LenFoo      equ $ - foo

bar:
    OffsetBar       equ bar - $$
    mov     ah, 0Ch
    mov     al, 'B'
    mov     [gs:((80*1 + 0)*2)], ax
    mov     al, 'a'
    mov     [gs:((80*1 + 1)*2)], ax
    mov     al, 'r'
    mov     [gs:((80*1 + 2)*2)], ax
    ret
    LenBar      equ $ - bar


DispMemSize:
    push    esi
    push    edi
    push    ecx

    mov     esi, MemChkBuf  ;目标为内存检查的缓存位置
    mov     ecx, [dwMCRNumber];得到关于内存数据块的数; 
                            ;for(ecx = dwMCRNumber; ecx > 0; --ecx)
.loop:                      ;{
    mov     edx, 5          ;ARDS  5x4byte
    mov     edi, ARDStruct  ;   uint32_t * MemChkBuf(esi),*ARDStruct(edi)
.1:                         ;                       
    push    dword [esi]     ;   for(esi = MemChkBuf, edx = 5; edx > 0 ; -- edx){
    call    DispInt         ;       DispInt(*MenChkBuf)
    pop     eax             ;  
    stosd                   ;       *ARDStruct = *MenChkBuf , ARDStruct += 1;
    add     esi, 4          ;       ++MemChkBuf;    
    dec     edx             ;   }
    cmp     edx, 0          ;
    jnz     .1              
    call    DispReturn
    cmp     dword [dwType], 1
    jne     .2
    mov     eax, [dwBaseAddrLow]
    add     eax, [dwLengthLow]
    cmp     eax, [dwMemSize]
    jb      .2
    mov     [dwMemSize], eax
.2:
    loop    .loop           ;}

    call    DispReturn
    push    szRAMSize
    call    DispStr
    add     esp, 4

    push    dword [dwMemSize]
    call    DispInt
    add     esp, 4

    pop     ecx
    pop     edi
    pop     esi
    ret

    %include "lib.inc"
SegCode32Len    equ $ - LABEL_SEG_CODE32
[SECTION .ring3]
ALIGN 32
[BITS 32];0x20 
LABEL_CODE_RING3:
    mov ax, SelectorVideo
    mov gs,ax
    mov edi,(80*12 + 4) *2 ;第12行第4列
    mov ah, 0Ch
    mov al, '3'
    mov [gs:edi],ax

    call SelectorCallGateTest:0

    jmp $
CodeRing3Len    equ $ - LABEL_CODE_RING3
;-------------LDT测试用代码段
[SECTION .la]
ALIGN 32
[BITS 32]
LABEL_CODE_A:
    mov ax, SelectorVideo
    mov gs,ax
    mov edi,(80*12 + 0) *2 ;第12行第0列
    mov ah, 0Ch
    mov al, 'L'
    mov [gs:edi],ax

    jmp SelectorCode16:0
CodeALen    equ $ - LABEL_CODE_A
;---调用门测试用代码段
[section .sdest]
ALIGN 32
[bits 32]
LABEL_SEG_CODE_DEST:
    ;打印字符 C
    mov ax, SelectorVideo
    mov gs,ax
    mov edi,(80*12 + 1) *2 ;第12行第0列
    mov ah,0Ch
    mov al,'C'
    mov [gs:edi],ax
    
    jmp  SelectorLDTCodeA:0
    retf    

SegCodeDestLen  equ $ - LABEL_SEG_CODE_DEST
[SECTION .s16code]
ALIGN   32
[BITS   16]
LABEL_SEG_CODE16:
    ;跳回实模式
    mov ax, SelectorNormal
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax

    mov eax, cr0
    and eax, 7FFFFFFEh  ; PE=0, PG=0
    mov cr0, eax
LABEL_GO_BACK_TO_REAL:
    jmp 0:LABEL_REAL_ENTRY

Code16Len       equ $ - LABEL_SEG_CODE16


