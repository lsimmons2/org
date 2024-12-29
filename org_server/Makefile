dev:
	dune exec -w bin/api.exe

test-api:
	ALCOTEST_VERBOSE=1 DUNE_CONFIG__GLOBAL_LOCK=disabled dune runtest --watch

diddle:
	DUNE_CONFIG__GLOBAL_LOCK=disabled dune exec -w bin/diddle.exe

pms:
	DUNE_CONFIG__GLOBAL_LOCK=disabled dune exec -w bin/parse_model_sheets.exe
