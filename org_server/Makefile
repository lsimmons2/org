dev:
	dune exec -w bin/api.exe

# --force needed since dune runtest will cache tests unless test file has changed?
# https://discuss.ocaml.org/t/tutorial-for-setting-up-test-suite-with-alcotest-dune/8367/4
test-api:
	ALCOTEST_VERBOSE=1 DUNE_CONFIG__GLOBAL_LOCK=disabled dune runtest --watch --force

diddle:
	DUNE_CONFIG__GLOBAL_LOCK=disabled dune exec -w bin/diddle.exe

pms:
	DUNE_CONFIG__GLOBAL_LOCK=disabled dune exec -w bin/parse_model_sheets.exe
