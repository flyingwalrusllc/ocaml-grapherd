.PHONY: coverage-report coveralls

coverage-report:
	dune clean
	BISECT_ENABLE=yes dune runtest
	bisect-ppx-report -I _build/default -html coverage-report/ `find . -name 'bisect*.out'`

coveralls:
	dune clean
	BISECT_ENABLE=yes dune runtest
	bisect-ppx-report -I _build/default -coveralls coveralls.json `find . -name 'bisect*.out'`


