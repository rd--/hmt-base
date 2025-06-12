GL_GIT=git@gitlab.com:rd--/hmt-base.git
GL_HTTP=https://gitlab.com/rd--/hmt-base.git

all:
	echo "hmt-base"

mk-cmd:
	(cd cmd; make)

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
