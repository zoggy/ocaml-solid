include ../../master.Makefile

SUBS=patch user_creation solid-conf
all: $(SUBS)

patch:
	$(MAKE) -C $@

user_creation:
	$(MAKE) -C $@

solid-conf:
	$(MAKE) -C $@

.PHONY: patch put post get options user_creation solid-conf

clean:
	for i in $(SUBS); do $(MAKE) -C $$i clean; done

