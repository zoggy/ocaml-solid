include ../Makefile.common

test: main

client: client.ml
	@$(OCAMLFIND) ocamlopt -o $@ -package solid.tls,lwt.ppx -rectypes -linkpkg $<

main:
	@$(RM) -r www ; \
	$(MKDIR) www/repo ; \
	(cd www/repo ; git init . ; \
	$(MKDIR) dir1/dir2 ; \
	$(CP) ../../,acl . ; \
	/bin/echo -e "file1\nv1" > dir1/file1.txt ; \
	/bin/echo -e "file2\nv1" > dir1/dir2/file2.txt ; \
	git add ,acl dir1/file1.txt dir1/dir2/file2.txt ; \
	git commit -am"v1" ; \
	/bin/echo -e "file1\nv2" > dir1/file1.txt ; \
	/bin/echo -e "file2\nv2" > dir1/dir2/file2.txt ; \
	git commit -am"v2" ; \
  git log --pretty="%H" | tail -n 1 | cut -d" " -f 1 > ../../commit-id.txt ; \
  git show `cat ../../commit-id.txt`:dir1/file1.txt > ../../git1.txt ; \
  cat dir1/file1.txt >> ../../git1.txt ; \
  git show `cat ../../commit-id.txt`:dir1/dir2/file2.txt > ../../git2.txt ; \
  cat dir1/dir2/file2.txt >> ../../git2.txt ) ; \
	$(LAUNCH_SERVER) & sleep 1 ; \
	export CID=`cat commit-id.txt` ; \
	curl -k -s $(URL)v/$${CID}/dir1/file1.txt > file1v1.txt ; \
	curl -k -s $(URL)v/$${CID}/dir1/dir2/file2.txt > file2v1.txt ; \
	curl -k -s $(URL)v/master/dir1/file1.txt > file1v2.txt ; \
	curl -k -s $(URL)v/master/dir1/dir2/file2.txt > file2v2.txt ; \
	cat file1v1.txt file1v2.txt > all1.txt ; \
	cat file2v1.txt file2v2.txt > all2.txt ; \
	diff git1.txt all1.txt && diff git2.txt all2.txt ; \
	export RES=$$? ; \
	kill $$! ; \
	exit $${RES}

clean:
	$(RM) -r *.txt www
