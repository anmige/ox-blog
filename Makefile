.PHONY: install
install:
	emacs \
		--batch \
		--eval "(require 'package)" \
		--eval '(setq package-user-dir (concat default-directory ".emacs"))' \
		--eval '(package-initialize)' \
		--eval "(package-refresh-contents)" \
		--eval "(package-install-from-archive (cadr (assoc 'org package-archive-contents)))"


.PHONY: test
test:
	emacs \
		--batch \
		--eval '(setq package-user-dir (concat default-directory ".emacs"))' \
		--eval '(package-initialize)' \
		--eval '(normal-top-level-add-to-load-path (list "test" "."))' \
		--eval '(load "test/ox-blog-test.el")' \
		--eval '(load "test/ert-plus-test.el")' \
		--eval '(ert-run-tests-batch-and-exit)'