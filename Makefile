all:
	echo "hmt-base"

mk-cmd:
	(cd cmd; make)

install:
	cabal v1-install --allow-newer

clean:
	rm -Rf dist dist-newstyle *~
	(cd cmd; make clean)

push-all:
	r.gitlab-push.sh hmt-base
	r.github-push.sh hmt-base

push-tags:
	r.gitlab-push.sh hmt-base --tags

indent:
	fourmolu -i Music

doctest:
	doctest -Wno-x-partial -Wno-incomplete-uni-patterns Music
