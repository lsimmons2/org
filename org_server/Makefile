prod:
	ORG_ENV=prod dune exec -w bin/api.exe

dev:
	ORG_ENV=dev dune exec -w bin/api.exe

# "index watch"
iw:
	dune build @ocaml-index --watch

dev-debug:
	ORG_ENV=dev LOG_LEVEL=debug dune exec -w bin/api.exe

run-test-api:
	ORG_ENV=test dune exec -w bin/api.exe

# --force needed since dune runtest will cache tests unless test file has changed?
# https://discuss.ocaml.org/t/tutorial-for-setting-up-test-suite-with-alcotest-dune/8367/4
run-api-tests:
	ORG_ENV=test DUNE_CONFIG__GLOBAL_LOCK=disabled dune runtest --watch --force

run-api-tests-v:
	ORG_ENV=test ALCOTEST_VERBOSE=1 DUNE_CONFIG__GLOBAL_LOCK=disabled dune runtest --watch --force

diddle:
	DUNE_CONFIG__GLOBAL_LOCK=disabled dune exec -w bin/diddle.exe

pms:
	DUNE_CONFIG__GLOBAL_LOCK=disabled dune exec -w bin/parse_model_sheets.exe
