checksumrecurse: fsrecurse ChecksumRecurse.ml
	ocamlfind ocamlopt -c \
-linkpkg -package \
nocrypto \
FsRecurse.ml FsRecurse.mli ChecksumRecurse.mli ChecksumRecurse.ml 

fsrecurse: FsRecurse.mli FsRecurse.ml
	ocamlfind ocamlopt -c \
-linkpkg -package \
cstruct.unix,cstruct \
FsRecurse.mli

clean:
	rm *.cmxa *.cmi *.cmx *.cmo *.native *.o 2>/dev/null | true

