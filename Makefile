GL_GIT=git@gitlab.com:rd--/hmt-base.git
GL_HTTP=https://gitlab.com/rd--/hmt-base.git

all:
	echo "hmt-base"

clean:
	rm -Rf dist dist-newstyle *~

push-gl:
	git push $(GL_GIT)

pull-gl:
	git pull $(GL_HTTP)

push-tags:
	git push $(GL_GIT) --tags

update-rd:
	ssh rd@rohandrape.net "(cd sw/hmt-base; git pull $(GL_HTTP))"

push-all:
	make push-gl update-rd

indent:
	fourmolu -i Music
