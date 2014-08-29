fsi=FsRecurse.mli FsRecurse.ml
csi=ChecksumRecurse.mli ChecksumRecurse.ml
ocl=ochecksum_lib.mli ochecksum_lib.ml

ochecksum: fsrecurse checksumrecurse ochecksumlib
	ocamlfind ocamlopt -o \
ochecksum.native \
-linkpkg -package \
cstruct.unix,nocrypto,cmdliner \
$(fsi) $(csi) $(ocl) ochecksum_cli.ml

ochecksumlib: fsrecurse checksumrecurse ochecksum_lib.ml
	ocamlfind ocamlopt -c \
-linkpkg -package \
nocrypto,cstruct,cstruct.unix \
$(fsi) $(csi) \
ochecksum_lib.ml

checksumrecurse: fsrecurse $(csi)
	ocamlfind ocamlopt -c \
$(fsi) $(csi)

fsrecurse: $(fsi)
	ocamlfind ocamlopt -c \
-linkpkg -package \
cstruct.unix,cstruct \
$(fsi)

clean:
	rm *.cmxa *.cmi *.cmx *.cmo *.native *.o 2>/dev/null | true

