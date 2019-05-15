#有BUG？？
TOOLPATH = ../tools/
TEMPPATH = ../temp/
SRCPATH = ./src/

MAKE = $(TOOLPATH)minGW/bin/mingw32-make.exe -r
CC = $(TOOLPATH)minGW/bin/mingw32-gcc.exe


BXIMAGE = $(TOOLPATH)bochs/bximage.exe
BOCHS = $(TOOLPATH)bochs/bochs.exe
BOCHSDBG = $(TOOLPATH)bochs/bochsdbg.exe
DD = $(TOOLPATH)dd/dd.exe
NASM = $(TOOLPATH)nasm/nasm.exe
EDIMG   = $(TOOLPATH)edimg.exe

#生成pmtest.com
$(TEMPPATH)pmtest.com: $(SRCPATH)pmtest.asm
	$(NASM) $(SRCPATH)pmtest.asm -I $(SRCPATH) -o $(TEMPPATH)pmtest.com


$(TEMPPATH)pm.img:$(TEMPPATH)pmtest.com
	-del pm.img
	$(EDIMG)   imgin:$(TOOLPATH)fdimg0at.tek \
	copy from:$(TEMPPATH)pmtest.com to:@: \
	imgout:$(TEMPPATH)pm.img
#将pmtest.bin写入空白磁盘
#pm.img : blank.img pmtest.com
#$(MAKE) blank.img
#$(MAKE) pmtest.com
#$(DD) if=pmtest.com of=pm.img bs=512 count=1

#编译+执行
run:
	$(MAKE) $(TEMPPATH)pm.img
	copy $(subst /,\,$(TEMPPATH)pm.img) ..\pm.img /Y
	$(BOCHS) -q -f ..\bochsrc.bxrc

rundbg:
	$(MAKE) $(TEMPPATH)pm.img
	copy $(subst /,\,$(TEMPPATH)pm.img) ..\pm.img /Y
	$(BOCHSDBG) -q -f ..\bochsrc.bxrc


bochs:
	$(BOCHS) -q -f ..\bochsrc.bxrc

bochsdbg:
	$(BOCHSDBG) -q -f ..\bochsrc.bxrc

clean:
	-del *.com
	-del *.bin
	-del *.img