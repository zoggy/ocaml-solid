include ../../master.Makefile

SUBS=patch user_creation solid-conf gitfs
all: $(SUBS)

patch:
	$(MAKE) -C $@

user_creation:
	$(MAKE) -C $@

solid-conf:
	$(MAKE) -C $@

gitfs:
	$(MAKE) -C $@

.PHONY: patch put post get options user_creation solid-conf gitfs

clean:
	for i in $(SUBS); do $(MAKE) -C $$i clean; done

