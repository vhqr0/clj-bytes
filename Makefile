.PHONY: test
test:
	clj -M:dev:test

.PHONY: cljs_test_build
cljs_test_build:
	clj -M:dev:cljs:cljs-test

.PHONY: cljs_test
cljs_test: cljs_test_build
	node target/cljs_build_node_test/main.js

.PHONY: lint
lint:
	cljfmt check src
	clj-kondo --lint src
