OS := $(shell uname -s)
IS_LINUX := $(shell echo $(OS)|grep -i linux)
IS_WIN := $(shell echo $(OS)|grep -i w32)
ifdef IS_LINUX
TMP = ~/
BAK = ~/po0l/
DOALL = mv $(OFILE) $(TOFILE)
else
ifdef IS_WIN
TMP = D:/
BAK = E:/baks/
DOALL = cp $(OFILE) $(BOFILE) ; mv $(OFILE) $(TOFILE)
endif
endif

A = 7z a $(OFILE) * .gitignore .git/config
WG = 7z a $(OFILE) * *.git*
X = $(A) -xr@$(XFILE)
P = tar zcvf $(ZOFILE) * .git --exclude-from=$(XFILE)
SUFFIX = .7z
XFILE = EXclude
PWD = $(shell pwd)
DIR = $(notdir $(PWD))
OFILE = $(addsuffix $(SUFFIX) , $(DIR))
TOFILE = $(join $(TMP) , $(OFILE))
BOFILE = $(join $(BAK) , $(OFILE))
ZOFILE = $(addsuffix .zip , $(DIR))
PUBOFILE = $(join $(TMP) , $(ZOFILE))

all:
	$(A)
	$(DOALL)

git:
	$(WG)
	$(DOALL)

pub:
	$(P)
	mv $(ZOFILE) $(PUBOFILE)

clean :
	rm $(TOFILE)

clear :
	rm $(BOFILE)

echo:
	@echo pwd   $(PWD)
	@echo dir   $(DIR)
	@echo ofile $(OFILE)
	@echo tofile $(TOFILE)
	@echo bofile $(BOFILE)
	@echo zofile $(ZOFILE)
	@echo pubofile $(PUBOFILE)
