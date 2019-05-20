#有BUG？？
TEMPPATH = ../temp/
SRCPATH = ./src/

BOCHS = bochs
BOCHSDBG = bochs
DD = dd
NASM = nasm

mount_disk:
	sudo losetup /dev/loop23 $(TEMPPATH)pm.img
	sudo mount -o  uid=1000,gid=1000 /dev/loop23 $(TEMPPATH)tt

umount_disk:
	sudo umount $(TEMPPATH)tt
	sudo losetup -d /dev/loop23
#生成pmtest.com
$(TEMPPATH)pmtest.com: $(SRCPATH)pmtest.asm
	$(NASM) $(SRCPATH)pmtest.asm -I $(SRCPATH) -o $(TEMPPATH)pmtest.com


$(TEMPPATH)pm.img:$(TEMPPATH)pmtest.com
	#dd if=$(TEMPPATH)pmtest.com of=$(TEMPPATH)pm.img bs=512 count=1 conv=notrunc
	sudo losetup /dev/loop23 $(TEMPPATH)pm.img
	sudo mount -o  uid=1000,gid=1000 /dev/loop23 $(TEMPPATH)tt
	cp	$(TEMPPATH)pmtest.com $(TEMPPATH)tt
	sudo umount $(TEMPPATH)tt
	sudo losetup -d /dev/loop23
#将pmtest.bin写入空白磁盘
#pm.img : blank.img pmtest.com
#$(MAKE) blank.img
#$(MAKE) pmtest.com
#$(DD) if=pmtest.com of=pm.img bs=512 count=1

#编译+执行
run:$(TEMPPATH)pm.img
	$(BOCHS) -q -f ../bochsrc-linux.bxrc -rc ../excute.bochs

rundbg:$(TEMPPATH)pm.img
	$(BOCHSDBG) -q -f ../bochsrc-linux.bxrc

bochs:
	$(BOCHS) -q -f ../bochsrc-linux.bxrc -rc ../excute.bochs

bochsdbg:
	$(BOCHSDBG) -q -f ../bochsrc-linux.bxrc

clean:
	rm -f $(TEMPPATH)*.com
	rm -f $(TEMPPATH)*.bin
	rm -f $(TEMPPATH)*.img