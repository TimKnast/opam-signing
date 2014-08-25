ochecksum: fsrecurse checksumrecurse ochecksum.ml
	ocamlfind ocamlopt -o \
ochecksum.native \
-linkpkg -package \
nocrypto,cstruct,cstruct.unix \
FsRecurse.mli FsRecurse.ml ChecksumRecurse.mli ChecksumRecurse.ml \
ochecksum.ml

checksumrecurse: fsrecurse ChecksumRecurse.ml ChecksumRecurse.mli
	ocamlfind ocamlopt -c \
FsRecurse.ml FsRecurse.mli ChecksumRecurse.mli ChecksumRecurse.ml 

fsrecurse: FsRecurse.mli FsRecurse.ml
	ocamlfind ocamlopt -c \
-linkpkg -package \
cstruct.unix,cstruct \
FsRecurse.mli

clean:
	rm *.cmxa *.cmi *.cmx *.cmo *.native *.o 2>/dev/null | true

