%.obo: %.owl
	blip -i $< io-convert -f thea2_owl -u obo_new_namespaces -u ontol_manifest_metadata_from_iao -to obo -o $@.tmp && obo-grep.pl --neg -r __file: $@.tmp > $@