default:
	ocamlc -o strassen.byte strassen.ml
run:
ifeq ("$(wildcard strassen.byte)","")
	make
endif
	./strassen.byte
clean:
	rm -f *.byte *.cmi *.cmo
	rm -rf doc
doc:
	mkdir doc
	ocamldoc -html -d doc -charset utf-8 strassen.ml
